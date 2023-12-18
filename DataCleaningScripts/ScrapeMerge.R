rm(list=ls())

#library(devtools)
library(tidyverse)
library(readxl)
library(maps)
library(here)
library(fuzzyjoin)
library(lubridate)

##### What this file does ################################################################
#
# This script runs 3 main tasks:
# 1. Scrapes current data from the original sources online, and merges in additional data
# 2. Cleans the data -- both bulk cleaning and detailed error corrections
# 3. Merges the data from FE and WaPo - for WA state only
#
# Resulting cleaned data is saved as an output
# Note: some cleaning is done via sourcing external files
#
# The code is designed to stop with a warning if it finds errors or updates are needed (for pursuit coding)
# These need to be manually fixed/updated, and then this file can be re-run (repeatedly, if needed
# until no more errors/updates are flagged)

###########################################################################################

# functions ----

state_fullname_fn <- function(x){
  snames <- c(state.name, "District of Columbia")
  names(snames) <- c(state.abb, "DC")
  snames[x]
}

scrape_data_fn <- function(dataset_name, url, save_file) {
  dataset_name <- rio::import(url)
  write.csv(dataset_name, save_file)
  dataset_name
}

make_url_fn <- function(x) {
  paste(sprintf('<a href="%1$s">%1$s</a>', x),
        collapse=",")
}

st.abb51 <- c(sort(state.abb), "DC")

# Scrape data ----

## Fatal Encounters

# doc_id = "1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE"
# url_template = 'https://docs.google.com/spreadsheets/d/DOC_ID/export?format=tsv'
# fe_url = sub("(*.)DOC_ID(*.)", 
#              paste("\\1", doc_id, "\\2", sep=""), 
#              url_template)
# fe_save_file = here("Downloads", "fe_raw.csv")
# 
# fe <- scrape_data_fn(fe, fe_url, fe_save_file)

fe <- rio::import(here("Downloads", "fe_raw.csv")) %>%
  select(-c(V1, V33:`Unique identifier (redundant)`))

# Clean up a couple of things so the temp newnames can be added, 
# date is persnickety, and the name needs to be changed for fixes later
# Latitude reads as character bc there is stray comma for fe ID 28891
# Age is a mess bc it has some ranges, and case_when doesn't like "-"

fe <- fe[!is.na(fe$`Unique ID`),] %>% # delete the end row of text
  mutate(date = mdy(`Date of injury resulting in death (month/day/year)`),
         Latitude = as.numeric(sub(",", "", Latitude)),
         Age = gsub("-","", Age),
         age = as.numeric(
           case_when(Age == "1825" ~ "22",
                     Age == "55." ~ "55",
                     TRUE ~ Age))
  ) %>%
  select(-Age)

## New names (while FE updating is paused in 2022)
## modify age and date to match above

newnames <- readr::read_csv("data-raw/temp-newnames.csv") %>%
  mutate(date = mdy(`Date of injury resulting in death (month/day/year)`)) %>%
  rename("age" = "Age")

fe <- bind_rows(fe, newnames)

## Washington Post
## Has been updated for the V2 structure
## Agency info merged in here

wapo_cases_url = "https://github.com/washingtonpost/data-police-shootings/raw/master/v2/fatal-police-shootings-data.csv"
wapo_cases_save_file = here("Downloads", "wapo_raw.csv")
wapo_cases <- scrape_data_fn(wapo, wapo_cases_url, wapo_cases_save_file)

# wapo_cases <- rio::import(wapo_cases_save_file)

wapo_agency_url = "https://github.com/washingtonpost/data-police-shootings/raw/master/v2/fatal-police-shootings-agencies.csv"
wapo_agency_save_file = here("Downloads", "wapo_agency_names.csv")
wapo_agency <- scrape_data_fn(wapo, wapo_agency_url, wapo_agency_save_file) %>%
  rename("agency_ids" = "id", "agency_name" = "name")

# wapo_agency <- rio::import(wapo_agency_save_file) %>%
#   rename("agency_ids" = "id", "agency_name" = "name")


## Transform agency ids to matrix (may be multiple agencies) and match agency names
## Matrix ncols hardcoded at 5, which is max agencies listed for any case by inspection
## Create "agency" variable with all names (to match FE) after matching

wapo_agency_xwalk <- wapo_agency %>% select(agencyid = agency_ids, agency_name, agency_type = type)
wapo_agencyXcase <- data.frame(id = wapo_cases$id, str_split_fixed(wapo_cases$agency_ids, ";", 5)) %>%
  mutate(across(X1:X5, ~ as.numeric(.x))) %>%
  rename_with(~gsub("X", "agencyid", .x, fixed=TRUE)) %>%
  left_join(wapo_agency_xwalk, by=c("agencyid1" = "agencyid")) %>% 
  rename("agency_name1" = "agency_name", "agency_type1" = "agency_type") %>%
  left_join(wapo_agency_xwalk, by=c("agencyid2" = "agencyid")) %>% 
  rename("agency_name2" = "agency_name", "agency_type2" = "agency_type") %>%
  left_join(wapo_agency_xwalk, by=c("agencyid3" = "agencyid")) %>% 
  rename("agency_name3" = "agency_name", "agency_type3" = "agency_type") %>%
  left_join(wapo_agency_xwalk, by=c("agencyid4" = "agencyid")) %>% 
  rename("agency_name4" = "agency_name", "agency_type4" = "agency_type") %>%
  left_join(wapo_agency_xwalk, by=c("agencyid5" = "agencyid")) %>% 
  rename("agency_name5" = "agency_name", "agency_type5" = "agency_type") %>%
  unite(col = "agency", contains("name"), sep = ", ", remove = F, na.rm=T)

## join cases and agencies
wapo <- left_join(wapo_cases, wapo_agencyXcase)


# Cleaning & Variable construction ----

## Pre-cleaning error correction ----
## These errors are typically ID'd during merging
## If they are not due to temporary absence of info, they are fixed now

source(here("DataCleaningScripts", "fixes_precleaning.R"))

## FE cleaning -------------------------------------------------

fe_draft <- fe %>%
  mutate(feID = `Unique ID`) %>%
  
  mutate(name = Name,
         name = case_when(
           name == "Name withheld by police" ~ "Unknown",
           is.na(name) ~ "Unknown",
           TRUE ~ name),
         name = str_remove(name, " Jr."),
         name = str_remove(name, " Jr$"),
         name = str_remove(name, " Sr."),
         name = str_remove(name, " III"),
         name = str_remove(name, " II"),
         name = str_remove(name, " IV"),
         name = str_remove(name, " V$"),
         
         # preserve aka names as "name2"
         name2 = if_else(grepl(" aka ", name), str_remove(name, ".* aka "), ""),
         # and remove the aka from the primary name
         name = str_remove(name, " aka.*"),
         
         name = str_replace_all(name, "-"," "),
         name = str_remove(name, "\\."),
         fname = "NA", # prep for assignment
         lname = "NA") %>%
  mutate(
    raceOrig = case_when(
      is.na(Race) | Race == "Race unspecified" | Race == "" ~ "Unknown",
      grepl("Asian", Race) ~ "API",
      grepl("Black", Race) ~ "BAA",
      grepl("Latino", Race) ~ "HL",
      grepl("Middle Eastern", Race) ~ "ME",
      grepl("Native", Race) ~ "NAA",
      grepl("White", Race) ~ "WEA",
      TRUE ~ Race),
    raceOrig = fct_relevel(raceOrig, "Unknown", after = Inf)
  ) %>%
  
  # We will use race+imputations for the FE race variable
  # Also a consistent set of categories with WaPo
  #  so Middle Eastern -> Other
  mutate(race = `Race with imputations`,
         race = case_when(
           is.na(race) | race == "Race unspecified" | race == "" ~ "Unknown",
           grepl("Asian", race) ~ "API",
           grepl("Black", race) ~ "BAA",
           grepl("Latino", race) ~ "HL",
           #grepl("Middle Eastern", race) ~ "ME",
           grepl("Native", race) ~ "NAA",
           grepl("White", race) ~ "WEA",
           TRUE ~ "Other"),
         
         # a handful of imputations are NA when the orig is known, so fix these
         race = if_else(race == "Unknown" & raceOrig != "Unknown", raceOrig, race),
           
         race = fct_relevel(race, c("Other", "Unknown"), 
                            after = Inf)  
  ) %>%
  mutate(gender = str_to_sentence(Gender),
         gender = case_when(
           is.na(gender) | gender == "" ~ "Unknown",
           #grepl("Trans", gender) ~ "Other",
           TRUE ~ gender),
         gender = fct_relevel(`gender`, "Unknown", after = Inf)
  ) %>%
  # age was pre-fixed above, but needs missing value, since age is used to merge
  mutate(age = ifelse(is.na(age), 999, age) 
  ) %>%
  mutate(month = month(date, label=T),
         day = day(date),
         year = year(date)
  ) %>%
  mutate(city = `Location of death (city)`,
         st = State,
         state = state_fullname_fn(st),
         state.num = match(st, st.abb51),
         zip = `Location of death (zip code)`,
         county = `Location of death (county)`
  ) %>%
  
  # For agency(s) we don't currently split up the multiples for FE like we do for WaPo
  mutate(agency = `Agency or agencies involved`
  ) %>%
  # agency.type classifies only for single agency incidents
  # otherwise it is coded "Multiple agencies"
  mutate(
    agency.type = case_when(
      agency=="" ~ "Unknown",
      grepl(",", agency) ~ "Multiple agencies",
      grepl("Campus|University|College|School", agency) ~ "Campus Police",
      grepl("State|Patrol", agency) ~ "State Police",
      grepl("Sheriff|Sherrif|sheriff", agency) ~ "County Sheriff",
      grepl("Tribe|Tribal|Nation[^a]", agency) ~ "Tribal Police",
      grepl("Police|police|Public Safety|Town|City", agency) ~ "Local Police",
      grepl("Tombstone|Corpus Christi|White Settlement|Charlotte-Mecklenburg|Patagonia", agency) ~ "Local Police",
      grepl("Correction", agency) ~ "Corrections Dept",
      #grepl("Alcohol Tobacco Firearms", agency) ~ "US ATF", too few
      grepl("U.S.*Border.", agency) ~ "US CBP",
      grepl("U.S. Federal Bureau", agency) ~ "US FBI",
      #grepl("U.S. Immigration", agency) ~ "US ICE", too few
      grepl("U.S. Marshal", agency) ~ "US Marshals",
      grepl("Port|National Guard|Metroparks", agency) ~ "Other",
      grepl("U.S.|National|Federal|Pentagon", agency) ~ "Other Federal",
      grepl("County|Harris Constable", agency) ~ "Other County",
      TRUE ~ "Other State"),
    agency.type = factor(
      agency.type, 
      levels = c("Local Police", "County Sheriff", "State Police", "Tribal Police",
                 "Campus Police", "Corrections Dept", 
                 "US CBP", "US FBI", "US Marshals", 
                 "Other Federal", "Other State", "Other County", "Other",
                 "Multiple agencies", "Unknown"))
  ) %>%
  mutate(cod = `Highest level of force`,
         cod = case_when(
           cod %in% c("", "Undetermined", "Unknown") ~ "Unknown",
           grepl("Less", cod) ~ "Other", # one case, fe 30304
           grepl("^.*(phyx).*", cod) ~ "Asphyxiated/Restrained",
           grepl("Beaten", cod) ~ "Beaten",
           grepl("Chemical", cod) ~ "Chemical gas/spray",
           cod == "Tasered" ~ "Taser",
           feID == 22977 ~ "Other", #pursued in car/fled by foot/jumped on tracks
           TRUE ~ cod),
         cod = factor(
           cod,
           levels = c("Gunshot", "Vehicle", "Taser",
                      "Asphyxiated/Restrained", "Medical emergency", "Drowned", 
                      "Beaten", "Drug overdose", 
                      "Fell from a height", "Burned/Smoke inhalation", 
                      "Chemical gas/spray", "Stabbed", "Other", "Unknown")
         )
  ) %>%
  mutate(armed = `Armed/Unarmed`,
         armed = case_when(
           armed == "Armed" | armed == "Arrmed" ~ "Alleged Armed",
           armed == "Unarmed" ~ "Unarmed",
           TRUE ~ "Unknown"),
         armed = fct_relevel(armed, "Unknown", 
                             after = Inf)
  ) %>%
  mutate(weapon = `Alleged weapon`,
         weapon =case_when(
           grepl("Edged", weapon)  ~ "Alleged edged weapon",
           grepl("Firearm", weapon)  ~ "Alleged firearm",
           grepl("Rifle", weapon)  ~ "Alleged firearm",
           weapon == "None" ~ "No weapon",
           weapon == "" | is.na(weapon) ~ "Unknown",
           TRUE ~ "Other"),
         weapon = fct_relevel(weapon, "Unknown", 
                              after = Inf)
  ) %>%
  # Flee variable is a mess, need consistent categories for WaPo
  # Some hierarchical coding of multiples here, with Vehicle > Other > Foot
  mutate(flee = `Fleeing/Not fleeing`,
         flee = case_when(
           is.na(flee) | flee == "" ~ "Unknown",
           grepl("Uncertain", flee) ~ "Unknown",
           grepl("Not|None", flee) ~ "Not fleeing",
           grepl("Veh|veh", flee) ~ "Vehicle",
           grepl("foot|Foot", flee) ~ "Foot",
           TRUE ~ "Other"),
         flee = factor(flee, 
                       levels = c("Not fleeing", "Vehicle", "Foot", "Other", "Unknown"))
  ) %>%
  
  # `Intended use of force (Developing)`
  
  ## from DBB's email about "Intended use of force (Developing)" coding
  ##   Vehicle is the clean non-pursuit category (e.g., accidents not related to pursuits)
  ##   Pursuit is the clean pursuit category
  ##   Veh/Purs is the unclean mix category that he is slowly working through
  ## inspection suggests he hasn't cleaned WA prior to 2016
  ## we do lots of WA cleaning in pursuit-coding.R, but not other states
  
  # several variables are drawn from this field:
  
  ## circumstances - cleaned up variable
  ## homicide - excludes suicides, and the Lake city case
  ## vpursuit.draft -- starts with circumstances, 
  ##                   for the rest see pursuit_coding.R

  ## circumstances: cleaned up FE labels

  mutate(circumstances = `Intended use of force (Developing)`,
         circumstances = case_when(
           circumstances == "" ~ "Unknown",
           circumstances == "Undetermined" ~ "Unknown",
           circumstances == "Vehic/Purs" ~ "Mix of pursuit-related cases",
           circumstances == "Pursuit" ~ "Active pursuit",
           circumstances == "Vehicle" ~ "Vehicle accident",
           circumstances == "No" ~ "Unintended",
           grepl("Nonlethal|Less-than", circumstances) ~ "Less lethal force",
           TRUE ~ circumstances),
         
         circumstances = fct_relevel(circumstances, "Unknown",
                                     after = Inf)
       ) %>%

  # homicide:  Excludes suicides and killed by subject (when not a pursuit vehicular homicide)
  # Also excludes medical emergencies and overdoses, but this is problematic
  # as the medical emergencies are in custody, and often in the context of the use of force
  # (e.g., cardiac arrest while handcuffed or restrained)
  # Includes pursuit vehicular fatalities, and vehicle accidents (which are homicides)
  
  mutate(homicide = case_when(
    grepl("Suicide|Subject", circumstances) ~ 0,
    grepl("Medical|overdose", cod) ~ 0,
    circumstances == "Unknown" ~ NA_real_,
    TRUE ~ 1)
  ) %>%
  
  mutate(medical = if_else(grepl("Medical", cod), 1, 0)) %>%
  mutate(suicide = if_else(grepl("Suicide", circumstances), 1, 0)) %>%
  
  ## vpursuit.draft: the starting variable for pursuit case id and cleaning ----
  ## since we only code WA cases, the rest of the pursuit coding is post merge
  ## draft categories are:

  ## Active pursuit - fatality occurred during pursuit
  ## Terminated pursuit - active pursuit, crash/fatality happened shortly after pursuit terminated 
  ## Involved pursuit - death occurred post pursuit, cod not vehicle (often shot)
  ## Attempted stop - lights/siren activated, subject fled, not pursued, crashed
  ## Vehicle accident - non-pursuit, non-fleeing incidents; on duty officer accident
  ## Needs review - DBB's tag for the related cases still needing review
  
  mutate(vpursuit.draft = case_when(
    grepl("pursuit", circumstances) ~ as.character(circumstances),
    grepl("Mix", circumstances) ~ "Needs review", # DBB tagged cases for review (thru 2021)
    circumstances == "Attempted stop" ~ "Attempted stop", # 2022+ cases
    circumstances == "Vehicle accident" ~ "Vehicle accident" # DBB coding
  )) %>%
  
  
  # NOTE: the `foreknowledge of mental illness? INTERNAL USE, NOT FOR ANALYSIS`
  # field only flags cases where a mental health issue was known before the
  # officers arrived on the scene.  Not sure how reliable this is
  
  mutate(foreknowledge = `Foreknowledge of mental illness? INTERNAL USE, NOT FOR ANALYSIS`,
         foreknowledge = case_when(
           foreknowledge == "" ~ "Unknown",
           foreknowledge == "Yes" ~ "Mental Illness",
           foreknowledge == "No" ~ "None",
           TRUE ~ foreknowledge) 
  ) %>%
  # Latitude was pre-fixed above
  mutate(latitude = Latitude,
         longitude = Longitude
  ) %>%
  mutate(url_info = `Supporting document link`,
         description = `Brief description`
  ) %>%
  
  # New variable:  not.kbp
  # When civilian/subject, not police, kills this person in the context of a police encounter
  # DOES NOT APPLY TO PURSUITS: vehicular homicides by fleeing subject id'd by pursuit variables
  # We only have this info for WA for now
  # feID 25805 Robert Hassan shot by suspect in Lake City; we leave the pursuit related vehicle crash 25804
  # feID 18751 Pamela Parker shot by subject, who then shot himself; we leave the suicide 18750

  mutate(not.kbp = ifelse(feID %in% c(18751, 25805), 1, 0))

### create clickable url for Rpubs reports
fe_draft$url_click <- sapply(fe_draft$url_info, make_url_fn)

# Select variables
## note that officer information is currently only available for WA for 2022
## until MPV merge issues are sorted out

fe_clean <- fe_draft %>%
  select(
    feID, name, name2, fname, lname,
    date, month, day, year,
    city, county, st, state, state.num, zip, 
    latitude, longitude,
    raceOrig, race, gender, age,
    cod, armed, weapon, flee,
    circumstances, homicide, suicide, not.kbp, medical, foreknowledge,
    vpursuit.draft,
    agency, agency.type,
    description, url_info, url_click, officer_names, officer_url
  )


## WAPO cleaning -------------------------------------------------

# 8416 duplicate case? may be Derrick Ameer Cook (info in 8617)
# reported on the WaPo github repo issue #53 appears to be deleted now

wapo_draft <- wapo %>%
  rename(wapoID = id,
         raceOrig = race,
         mental_illness = was_mental_illness_related,
         threat = threat_type,
         flee = flee_status,
         bodycam = body_camera
  ) %>%
  mutate(name = ifelse(name == "", "Unknown", name),
         name = str_remove(name, " Jr.*$"),
         name = str_remove(name, ", Jr.$"),
         name = str_remove(name, " Jr$"),
         name = str_remove(name, " Sr.$"),
         name = str_remove(name, " III"),
         name = str_remove(name, " II"),
         name = str_remove(name, " IV$"),
         name = str_remove(name, " V$"),
         #name = str_remove(name, " aka.*"),
         name = str_replace_all(name, "-"," "),
         fname = "NA",
         lname = "NA"
  ) %>%
  mutate(age = ifelse(is.na(age), 999, age) # missing value
  ) %>% 
  mutate(gender = str_to_sentence(gender),
         gender = case_when(
           gender == "" ~ "Unknown",
           #grepl("Non|Trans", gender) ~ "Other",
           TRUE ~ gender)
  ) %>%
  
  # note race is multiply coded now in V2
  # but we need a consistent set of categories for FE
  # original wapo "race" variable has been renamed to raceOrig above
  mutate(
    race = case_when(
      is.na(raceOrig) | raceOrig == "" | raceOrig == "Unknown" ~ "Unknown",
      raceOrig == "A" ~ "API",
      raceOrig == "B" ~ "BAA",
      raceOrig == "H" ~ "HL",
      raceOrig == "N" ~ "NAA", #Native American/Alaskan
      raceOrig == "O" ~ "Other",
      raceOrig == "W" ~ "WEA",
      TRUE ~ "Other"),
    race = fct_relevel(race, c("Other", "Unknown"), 
                       after = Inf)
  ) %>%
  mutate(armed = armed_with,
         armed = case_when(
           armed=="unarmed" ~ "Unarmed",
           armed=="unknown" | armed=="" ~ "Unknown",
           TRUE ~ "Alleged Armed")
  ) %>%
  mutate(weapon = armed_with,
         weapon = case_when(
           armed=="Unarmed" ~ "No weapon",
           armed=="Unknown" ~ "Unknown",
           grepl("gun", weapon) ~ "Alleged firearm",
           grepl("knife", weapon) ~ "Alleged edged weapon",
           TRUE ~ "Other")
  ) %>%
  mutate(flee = case_when(
           is.na(flee) | flee == "" ~ "Unknown",
           grepl("not", flee) ~ "Not fleeing",
           grepl("car", flee) ~ "Vehicle",
           grepl("foot", flee) ~ "Foot",
           TRUE ~ "Other"),
         flee = factor(flee, 
                       levels = c("Not fleeing", "Vehicle", "Foot", "Other", "Unknown"))
  ) %>%
  rename(st = state) %>%
  mutate(state = state_fullname_fn(st),
         state.num = match(st, st.abb51),
         date = ymd(date),
         month = month(date, label=T),
         day = day(date),
         year = year(date)
  ) %>%
  mutate(agency = gsub("Sheriff's Department", "Sheriff's Office", agency),
         agency = gsub("office", "Office", agency)
  ) %>%
  # agency.type classifies only for single agency incidents
  # otherwise it is coded "Multiple agencies"
  mutate(
    agency.type = case_when(
      grepl(";", agency_ids) ~ "Multiple agencies",
      agency_type1 == "" & grepl("Police", agency_name1) ~ "Local Police",
      grepl("Campus|University|College|School", agency_name1) ~ "Campus Police",
      grepl("Sheriff|Sherrif", agency_name1) ~ "County Sheriff",
      grepl("Tribe|Tribal|Nation[^a]|Choctaw|Apache", agency_name1) ~ "Tribal Police",
      grepl("Correction", agency_name1) ~ "Corrections Dept",
      grepl("Border", agency_name1) ~ "US CBP",
      grepl("Alchohol", agency_name1) ~ "US ATF",
      grepl("Federal Bureau", agency_name1) ~ "US FBI",
      grepl("Marshal", agency_name1) ~ "US Marshals",
      #grepl("Immigration", agency_name1) ~ "US ICE", too few
      #grepl("Drug Enforcement", agency_name1) ~ "US DEA", too few
      grepl("County", agency_name1) ~ "Other County",
      grepl("state_police", agency_type1) ~ "State Police",
      grepl("local_police", agency_type1) ~ "Local Police",
      grepl("federal", agency_type1) ~ "Other Federal",
      grepl("state", agency_type1) ~ "Other State",
      grepl("local", agency_type1) ~ "Other Local",
      TRUE ~ "Other"),
    agency.type = factor(
      agency.type, 
      levels = c("Local Police", "County Sheriff", "State Police", "Tribal Police",
                 "Campus Police",
                 "US CBP", "US FBI", "US Marshals", 
                 "Other Federal", "Other State", "Other County", "Other Local", "Other",
                 "Multiple agencies"))
  )

# select common vars, and some unique vars
wapo_clean <- wapo_draft %>%
  select(
    wapoID, name, fname, lname,
    date, month, day, year,
    city, county, st, state, state.num, 
    latitude, longitude,
    raceOrig, race, race_source, gender, age,
    armed, weapon, flee,
    threat, bodycam,
    mental_illness,
    agency, agency.type)

## Final prep -------------------------------------------------

## Post-cleaning fixes (mostly for WA State) ----
## These fixes are also typically identified during merges, but the fixes can be
## applied to the constructed vars directly

source(here("DataCleaningScripts", "fixes_postcleaning.R"))

## FE name processing ----

name.list <- str_split(fe_clean$name, " ")
for(i in 1:length(name.list)) {
  fe_clean$fname[i] <- name.list[[i]][1]
  fe_clean$lname[i] <- name.list[[i]][length(name.list[[i]])]
}

## WaPo name processing ----
name.list <- str_split(wapo_clean$name, " ")
for(i in 1:length(name.list)) {
  wapo_clean$fname[i] <- name.list[[i]][1]
  wapo_clean$lname[i] <- name.list[[i]][length(name.list[[i]])]
}

# Save clean source datasets, all cases ----

## CSV files ----

write.csv(fe_clean, here("data-outputs", "FE_clean.csv"))
write.csv(wapo_clean, here("data-outputs", "WaPo_clean.csv"))

## Rdata files ----
## These save lots of metadata as well

## key date info

selection <- "all cases"
scrape_date <- Sys.Date()
last_date_fe <- max(fe_clean$date[fe_clean$feID < 90000])
last_date_wapo <- max(wapo_clean$date)
last_newname_date <- max(fe_clean$date[fe_clean$feID > 90000])
last_data_update <- max(last_date_fe, last_date_wapo, last_newname_date)
last_update_is_eoy <- month(last_data_update)==12 & 
  day(last_data_update)==31
last_complete_mo <- ifelse(last_update_is_eoy | month(last_data_update)==1, 
                           12, 
                           month(last_data_update)-1)
last_complete_yr <- ifelse(last_update_is_eoy, 
                           year(last_data_update), 
                           year(last_data_update)-1)

save(list = c("fe_clean", "wapo_clean", "selection",
              "scrape_date", "last_date_fe", "last_date_wapo", "last_newname_date",
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("data-outputs", "CleanData.rda"))

wapo.update.message <- paste("\n *** Last Wapo update: ", last_date_wapo, "\n\n")

# Merge  -----

# We only do this for WA state, to make cleaning feasible
# MPV has merged all cases, so we may be able to use that later
# But we've found some errors in their merge for WA, so we're
# not relying on their merge for now.

# Removing problem cases:

## WaPo 4568
## This is the only WAPO case not found in FE.
## Not clear if the victim died, name unknown, can't find more info online.  
## Have reported the case to FE.
## https://www.kiro7.com/news/local/police-investigating-officer-involved-shooting-in-federal-way/930686522/
## https://komonews.com/news/local/federal-way-police-investigating-officer-involved-shooting

## Filter WA from 2015
fe_2015 <- fe_clean %>% 
  filter(date > "2014-12-31" & st == "WA") %>%
  filter(not.kbp==0) # subject killed victim, not in a pursuit incident

wapo_2015 <- wapo_clean %>% 
  filter(st == "WA") %>%
  filter(wapoID != 4568) %>%
  mutate(cod = "Gunshot") # to facilitate merge

## Merge using stringdist
initialmerge <- stringdist_full_join(
  fe_2015, wapo_2015,
  by = c("lname", "fname", "date", "gender", 
         "cod", "month", "day", "city"),
  max_dist = 2,
  ignore_case = T) %>%
  rename_with(~gsub(".x", ".fe", .x, fixed=T)) %>%
  rename_with(~gsub(".y", ".wapo", .x, fixed=T))

## Validate the merge ----

## This is a multi-stage process, output needs to be carefully reviewed for errors
## every time the script is run
## If they are found, fixes are made to the pre- and/or post- cleaning fix files
## and this script is run again until it runs cleanly

## Check for unmatched WAPO records ----
## WAPO is a subset of FE, so all WAPO cases should be in FE
## Missing cases are due to delays in FE updating
## So the fixes are temporary, 



errors <- 0

aaa <- initialmerge %>% 
  filter(is.na(feID)) %>% 
  select(wapoID, fname.wapo, lname.wapo)

matching.message <- ifelse(nrow(aaa) > 0,
                           "*** Unmatched WAPO records ***",
                           "*** No unmatched WAPO records ***")

if(nrow(aaa) > 0){
  cat("\n *** Unmatched WAPO records (expect none):")
  print(aaa)
  if(nrow(aaa) > 1) {
    errors <- errors+1
  } 
} else {
  cat("\n *** No unmatched WAPO records *** \n\n")
}

### START TEMPORARY FIXES for unmatched WaPo records ##############

## Should only be temporary b/c WAPO is a subset of FE
## Assign WAPO info to FE fields, since the FE fields are used for some of the reports
## feID will be 99999 to id these cases

## missing WAPO info needs to be set manually if needed (in fixes_wapo2FE.R)

## if instead the WaPo info is delayed the temp fix is implemented before the merge
## in fixes_postcleaning 

if (dim(aaa)[1] > 0) {
  
  initialmerge[is.na(initialmerge$feID),] <- initialmerge[is.na(initialmerge$feID),] %>% 
    mutate(feID = ifelse(is.na(feID), 99999, feID),
           name.fe = name.wapo,
           fname.fe = fname.wapo,
           lname.fe = lname.wapo,
           age.fe = age.wapo,
           city.fe = city.wapo,
           county.fe = county.wapo,
           latitude.fe = latitude.wapo,
           longitude.fe = longitude.wapo,
           date.fe = date.wapo,
           month.fe = month.wapo,
           day.fe = day.wapo,
           year.fe = year.wapo,
           cod.fe = "Gunshot",
           gender.fe = gender.wapo,
           race.fe = race.wapo,
           circumstances = "Deadly force",
           homicide = 1,
           suicide = 0,
           not.kbp = 0,
           armed.fe = armed.wapo,
           weapon.fe = weapon.wapo)
  
  
  # And recheck for unmatched wapoIDs
  aaa <- initialmerge %>% 
    filter(is.na(feID)) %>% 
    select("wapoID", "fname.wapo", "lname.wapo")
  if(nrow(aaa) > 0){
    cat("\n *** Unmatched WAPO records (expect none):")
    print(aaa)
    errors <- errors+1
  } else {
    cat("\n *** No unmatched WAPO records after info transfer \n\n")
  }
  
}

### END TEMPORARY FIXES FOR UNMATCHED WAPO RECORDS #############

## Checks for bad matches from the merge ----
## These are relatively stable, and need to be fixed manually
## Most should be fixed by modifying incorrect info in the
## pre or post cleaning fix files.  But if that is not enough to
## prevent the match, then as a last resort the match is reversed by hand in
## the fixes_postmerge file

## Duplicate feIDs
aaa <- table(initialmerge$feID)
if(any(aaa>1)){
  names(aaa[aaa>1])
  sort(unique(aaa))
  cat("\n *** Duplicate FE IDs, #times, 99999 means unmatched WaPo case \n\n")
  print(aaa[aaa>1])
  errors <- errors+1
} else {
  cat("\n *** No duplicate FE IDs \n\n")
}

## Duplicate wapoIDs
aaa <- table(initialmerge$wapoID)
if(any(aaa>1)){
  names(aaa[aaa>1])
  sort(unique(aaa))
  cat("\n *** Duplicate WaPo IDs, #times \n\n")
  print(aaa[aaa>1])
  errors <- errors+1
} else {
  cat("\n *** No duplicate WaPo IDs \n\n")
}


## City/date mismatch

aaa <- initialmerge %>%
  filter(city.wapo != city.fe & date.fe != date.wapo) %>%
  select(feID, wapoID, city.fe, city.wapo, date.fe, date.wapo)
if(nrow(aaa)>1){
  cat("\n *** City & date mismatches: ")
  print(aaa)
  errors <- errors+1
} else {
  cat("\n *** No city & date mismatches \n\n")
}


# County mismatch (lots missing or empty county in wapo, we ignore those) 
aaa <- initialmerge %>%
  filter(county.wapo != county.fe & !is.na(wapoID) & county.wapo != "") %>%
  select(feID, wapoID, city.fe, city.wapo, county.fe, county.wapo, st.fe, st.wapo)
if(nrow(aaa)>1){
  cat("\n *** County mismatches:")
  print(aaa)
  errors <- errors+1
} else {
  cat("\n *** No county mismatches \n\n")
}

# Check for errors in final dataset ----
if(errors > 0){
  msg <- paste("\n\n ***", errors, "cleaning errors found.  Stopping *** \n\n")
  stop(msg)
} else {
  cleaning.error.message <- "\n\n *** No errors during cleaning!! *** \n\n"
}

# Only if bad merges identified above that can't be fixed by cleaning (very rare)
# The cases are unmerged and remerged as needed
# source("fixes_postmerge.R") 


# Final merged dataset prep for 2015+ WA data ----

## Consensus variable construction: ----

## 1. Supplement FE missing data with WaPo when available
## There are many errors in WaPo, and they are slow to fix them when reported.  
## Any errors we find in FE are either fixed by DBB, MPV or us, 
## so FE has the most reliable data.

## 2. Use hierarchical coding for armed, weapon and flee
## WaPo data here may be more reliable, with certainty > uncertainty
## and categories with some ordering

## 3. Legislative year variable
## We create an approximate legislative year variable, to use for
## before and after assessments.  Some bills take effect immediately
## (typically in late May) and others 90 days after end of session 
## (typically late July), and that also varies by short/long leg year.

## Because we are mostly going to be interested in the before/after
## effects of the 2021 legislative reforms, and most of these took
## effect in late July 2021, we will use Aug 1 - Jul 30 as the
## the legislative year.


curr.yr <- lubridate::year(Sys.Date())
curr.mo <- lubridate::month(Sys.Date())
cut.yr <- ifelse(curr.mo > 7, curr.yr+1, curr.yr)                        

finalmerge <- initialmerge %>%
  
  mutate(# Supplement FE missing with WaPo
         name = ifelse(is.na(name.fe), name.wapo, name.fe),
         fname = ifelse(is.na(fname.fe), fname.wapo, fname.fe),
         lname = ifelse(is.na(lname.fe), lname.wapo, lname.fe),
         date = date.fe, # no missing fe data in date
         month = month.fe,
         day = day.fe,
         year = year.fe,
         
         leg.year = 
           cut(date, 
               breaks = as.Date(paste(2000:(cut.yr), "-08-01", sep="")),
               labels = c(paste(month.abb[8], 2000:(cut.yr-1), "-", 
                                month.abb[7], 2001:cut.yr,
                                sep = ""))),
         
         city = ifelse(is.na(city.fe), city.wapo, city.fe),
         county = ifelse(is.na(county.fe), county.wapo, county.fe),
         latitude = ifelse(is.na(latitude.fe), latitude.wapo, latitude.fe),
         longitude = ifelse(is.na(longitude.fe), longitude.wapo, longitude.fe),
         race = if_else(race.fe == "Unknown" & !race.wapo == "Unknown" & !is.na(race.wapo), race.wapo, race.fe),
         race.source = case_when(
           raceOrig.fe != "Unknown" ~ "FE",
           raceOrig.fe == "Unknown" & race.fe != "Unknown" ~ "FE imputed",
           race.fe == "Unknown" & race.wapo != "Unknown" ~ "WaPo",
           TRUE ~ "Race not known"),
         gender = if_else(gender.fe == "Unknown" & !is.na(gender.wapo), gender.wapo, gender.fe),
         age = ifelse(age.fe == 999 & !age.wapo == 999 & !is.na(age.wapo), age.wapo, age.fe),
         cod = cod.fe,
         agency = agency.fe, # no missing fe data in agency or agency type
         agency.type = agency.type.fe,
         
         # Hierarchical coding
         armed = case_when(
           grepl("Armed", armed.fe) | grepl("Armed", armed.wapo) ~ "Alleged Armed", 
           armed.fe == "Unarmed" | armed.wapo == "Unarmed" ~ "Unarmed", 
           TRUE ~ "Unknown"),
         weapon = case_when(
           grepl("firearm", weapon.fe) | grepl("firearm", weapon.wapo) ~ "Alleged firearm", 
           grepl("edged", weapon.fe) | grepl("edged", weapon.wapo) ~ "Alleged edged weapon", 
           grepl("Other", weapon.fe) | grepl("Other", weapon.wapo) ~ "Alleged other weapon", 
           grepl("No", weapon.fe) | grepl("No", weapon.wapo) ~ "No weapon", 
           TRUE ~ "Unknown"),
         flee = case_when(
           flee.fe == "Vehicle" | flee.wapo == "Vehicle"  ~ "Vehicle", 
           flee.fe == "Foot" | flee.wapo == "Foot"  ~ "Foot", 
           flee.fe == "Other" | flee.wapo == "Other"  ~ "Other", 
           grepl("Not", flee.fe) | grepl("Not", flee.wapo) ~ "Not fleeing", 
           TRUE ~ "Unknown")
         ) %>%
  
  # Select consensus variables, along with the few source-specific vars  
  select(feID, wapoID,
         name:county, zip, latitude:agency.type,
         armed:flee,
         # not coded for consistency:
         mental_illness.fe = foreknowledge, 
         mental_illness.wapo = mental_illness,
         # FE only variables
         circumstances, homicide, suicide, not.kbp, 
         medical, 
         vpursuit.draft,
         cod,
         # WaPo only variables
         bodycam, threat,
         # Source variables from FE
         description,
         url_info,
         url_click,
         officer_names,
         officer_url
  ) %>%
  arrange(date)

# Finalize pursuit coding ----
## See the external file for information on the process

source(here::here("DataCleaningScripts", "pursuit_coding.R"))

## Add vpursuit codes to both finalmerge & fe_2015
## Since the cod, url_info and description may have been improved during pursuit
##   review, we use final versions in the coded.pursuits df.
## pursuit.type merges Active and Terminated pursuits into "Pursuit"

finalmerge <- left_join(finalmerge %>% select(-c("cod", "description", "url_info")), 
                        coded.pursuits %>% select(-c("name", "date")),
                        by = "feID") %>%
  mutate(pursuit.type = ifelse(grepl("Active|Terminated", vpursuit), "Pursuit", vpursuit)) %>%
  select(-vpursuit.draft)

fe_2015 <- left_join(fe_2015%>% select(-c("cod", "description", "url_info")), 
                     coded.pursuits %>% select(-c("name", "date"))) %>%
  mutate(pursuit.type = ifelse(grepl("Active|Terminated", vpursuit), "Pursuit", vpursuit)) %>%
  select(-vpursuit.draft)

### re-create clickable url for Rpubs reports
finalmerge$url_click <- sapply(finalmerge$url_info, make_url_fn)
fe_2015$url_click <- sapply(fe_2015$url_info, make_url_fn)


# Save WA clean and merged datasets as Rdata files ----
## Note that the fe and wapo data include all cases, not just WA.
## For WA only analysis, use the merged_data

##  2015 and later ----
selection <- "2015+"
fe_data <- fe_clean %>% filter(date > "2014-12-31")
wapo_data <- wapo_clean
merged_data <- finalmerge 

last.name.message <- paste("\n *** Last WA fatality: ",
                           merged_data$name[nrow(merged_data)],
                           merged_data$date[nrow(merged_data)],
                           "\n\n")

save(list = c("fe_data", "wapo_data", "merged_data", "selection",
              "scrape_date", "last_date_fe", "last_date_wapo","last_newname_date",
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("data-outputs", "WA2015.rda"))

##  Since 940 ----
selection <- "since 940"
fe_data <- fe_clean %>% filter(date > "2018-12-06")
wapo_data <- wapo_clean %>% filter(date > "2018-12-06")
merged_data <- finalmerge %>% filter(date > "2018-12-06")

save(list = c("fe_data", "wapo_data", "merged_data", "selection",
              "scrape_date", "last_date_fe", "last_date_wapo","last_newname_date",
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("data-outputs", "WA940.rda"))


# Print summary of run
message(matching.message)
message(cleaning.error.message)

message(wapo.update.message)
message(last.name.message)

message(pursuit.coding.message)
message(wtsc.update.message)

# Quick descriptives
#Hmisc::describe(finalmerge)
