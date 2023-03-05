rm(list=ls())
library(devtools)
library(tidyverse)
library(readxl)
library(stringr)
library(forcats)
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

num <- 1:51
states <- c(sort(state.abb), "DC")

# Scrape data ----

## Fatal Encounters

doc_id = "1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE"
url_template = 'https://docs.google.com/spreadsheets/d/DOC_ID/export?format=tsv'
fe_url = sub("(*.)DOC_ID(*.)", 
             paste("\\1", doc_id, "\\2", sep=""), 
             url_template)
fe_save_file = here("Downloads", "fe_raw.csv")

fe <- scrape_data_fn(fe, fe_url, fe_save_file)

# Clean up a couple of things so the temp newnames can be added, date is persnickety
fe <- fe[!is.na(fe$`Unique ID`),] %>% # delete the end row of text
  mutate(date = mdy(`Date of injury resulting in death (month/day/year)`))

## New names (while FE updating is paused in 2022)
## modify col classes to match

newnames <- readr::read_csv("data-raw/temp-newnames.csv") %>%
  mutate(Age = as.character(Age),
         `Date of injury resulting in death (month/day/year)` = as.character(`Date of injury resulting in death (month/day/year)`),
         date = mdy(`Date of injury resulting in death (month/day/year)`),
         Latitude = as.character(Latitude))

fe <- bind_rows(fe, newnames)

## Washington Post
## Has been updated for the V2 structure

wapo_url = "https://github.com/washingtonpost/data-police-shootings/raw/master/v2/fatal-police-shootings-data.csv"
wapo_save_file = here("Downloads", "wapo_raw.csv")

wapo <- scrape_data_fn(wapo, wapo_url, wapo_save_file)

# Cleaning & Variable construction ----

## Pre-cleaning error correction ----
## These errors are typically ID'd during merging
## If they are not due to temporary absence of info, they are fixed now

source(here("DataCleaningScripts", "fixes_precleaning.R"))

## FE cleaning -------------------------------------------------

fe_clean <- fe %>%
  filter(!is.na(`Unique ID`) & `Unique ID` != "NA") %>%
  
  mutate(feID = `Unique ID`) %>%
  
  mutate(name = Name,
         name = case_when(
           name == "Name withheld by police" ~ "Unknown",
           is.na(name) ~ "Unknown",
           TRUE ~ name),
         name = str_remove(name, " Jr."),
         name = str_remove(name, " Sr."),
         name = str_remove(name, " III"),
         name = str_remove(name, " II"),
         name = str_remove(name, " IV"),
         name = str_remove(name, " V$"),
         name = str_remove(name, " aka.*"),
         name = str_replace_all(name, "-"," "),
         fname = "NA", # prep for assignment
         lname = "NA") %>%
  mutate(
    raceOrig = case_when(
      is.na(Race) ~ "Unknown",
      Race == "Race unspecified" ~ "Unknown",
      Race == "NA" ~ "Unknown",
      Race == "Asian/Pacific Islander" ~ "API",
      Race == "African-American/Black" ~ "BAA",
      Race == "Hispanic/Latino" ~ "HL",
      Race == "HIspanic/Latino" ~ "HL",
      Race == "Middle Eastern" ~ "ME",
      Race == "Native American/Alaskan" ~ "NAA",
      Race == "European-American/White" ~ "WEA",
      Race == "european-American/White" ~ "WEA",
      TRUE ~ Race),
    raceOrig = fct_relevel(raceOrig, "Unknown", after = Inf)
  ) %>%
  mutate(raceImp = `Race with imputations`,
         raceImp = case_when(
           is.na(raceImp) ~ "Unknown",
           raceImp == "Race unspecified" ~ "Unknown",
           is.na(raceImp) ~ "Unknown",
           raceImp == "Asian/Pacific Islander" ~ "API",
           raceImp == "African-American/Black" ~ "BAA",
           raceImp == "Hispanic/Latino" ~ "HL",
           raceImp == "HIspanic/Latino" ~ "HL",
           raceImp == "Middle Eastern" ~ "ME",
           raceImp == "Native American/Alaskan" ~ "NAA",
           raceImp == "European-American/White" ~ "WEA",
           raceImp == "european-American/White" ~ "WEA",
           TRUE ~ raceImp),
         raceImp = fct_relevel(raceImp, "Unknown", after = Inf)
  ) %>%
  mutate(gender = ifelse(Gender=="" | is.na(Gender), "Unknown", Gender),
         gender = fct_relevel(`gender`, "Unknown", after = Inf)
  ) %>%
  # age is a mess, chr & lots of typos, lots of NA, case_when doesn't like "-"
  mutate(Age = gsub("-","", Age),
         ageChr = case_when(Age == "1825" ~ "22",
                            Age == "55." ~ "55",
                            Age == "4050" ~ "45",
                            TRUE ~ Age),
         age = as.numeric(ageChr),
         age = ifelse(is.na(age), 999, age) # missing value, since age is used to merge
  ) %>%
  mutate(month = month(date, label=T),
         day = day(date),
         year = year(date)
  ) %>%
  mutate(city = `Location of death (city)`,
         st = State,
         state = state_fullname_fn(st),
         zip = `Location of death (zip code)`,
         county = `Location of death (county)`
  ) %>%
  mutate(agency = `Agency or agencies involved`
  ) %>%
  mutate(agency.type = case_when(
    agency=="" ~ "Unknown",
    grepl("^.*,.*", agency) ~ "Multiple agencies",
    grepl("^.*(Campus|University|College).*", agency) ~ "University Police",
    grepl("^.*(State|Patrol).*", agency) ~ "State Police",
    grepl("^.*Sheriff.*", agency) ~ "County Sheriff",
    grepl("^.*(Police|police|Public Safety|Town|City).*", agency) ~ "Local Police",
    grepl("^.*Correction.*", agency) ~ "Corrections Dept",
    grepl("^.*County.*", agency) ~ "Other county level unit",
    grepl("^.*U.S.*Alchohol.*", agency) ~ "US ATF",
    grepl("^.*U.S.*Border.*", agency) ~ "US CBP",
    grepl("^.*U.S. Federal Bureau.*", agency) ~ "US FBI",
    grepl("^.*U.S. Immigration.*", agency) ~ "US ICE",
    grepl("^.*U.S. Marshal.*", agency) ~ "US Marshals",
    grepl("^.*U.S.|Military|Air Force.*", agency) ~ "Other US level unit",
    TRUE ~ "Other state level unit"),
    agency.type = factor(
      agency.type, 
      levels = c("Local Police", "County Sheriff", "State Police", 
                 "University Police", "Corrections Dept", 
                 "US CBP", "US ICE", "US FBI", "US Marshals", 
                 "Other state level unit", "Other county level unit", "Other US level unit",
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
  mutate(weapon.fe = `Alleged weapon`,
         weapon.fe =case_when(
           grepl("Edged", weapon.fe)  ~ "Alleged edged weapon",
           grepl("Firearm", weapon.fe)  ~ "Alleged firearm",
           grepl("Rifle", weapon.fe)  ~ "Alleged firearm",
           weapon.fe == "None" ~ "No weapon",
           weapon.fe == "" | is.na(weapon.fe) ~ "Unknown",
           TRUE ~ "Other"),
         weapon.fe = fct_relevel(weapon.fe, "Unknown", 
                              after = Inf)
  ) %>%
  mutate(fleeing = `Fleeing/Not fleeing`,
         fleeing = case_when(
           fleeing == "" ~ "Unknown",
           grepl("Uncertain", fleeing) ~ "Unknown",
           grepl("Not", fleeing) ~ "No",
           TRUE ~ "Alleged yes"),
         fleeing = fct_relevel(fleeing, "Unknown", 
                               after = Inf)
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

  # homicide:  Exclude suicides and killed by subject (when not a pursuit vehicular homicide)
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
  
  mutate(medical = if_else(grepl("Medical", circumstances), 1, 0)) %>%
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
  
  mutate(latitude = as.numeric(Latitude),
         longitude = as.numeric(Longitude)) %>%
  mutate(url_info = `Supporting document link`) %>%
  mutate(description = `Brief description`) %>%
  mutate(state.num = num[match(st, states)])

### create clickable url for Rpubs reports
fe_clean$url_click <- sapply(fe_clean$url_info, make_url_fn)

# Select variables
## note that officer information is currently only available for WA for 2022
## until MPV merge issues are sorted out

fe_clean <- fe_clean %>%
  select(
    feID, name, fname, lname,
    date, month, day, year,
    city, county, st, state, state.num, zip, 
    latitude, longitude,
    raceOrig, raceImp, gender, age, ageChr, foreknowledge,
    cod, armed, weapon.fe, fleeing,
    circumstances, homicide, suicide, medical, vpursuit.draft,
    agency, agency.type,
    description, url_info, url_click, officer_names, officer_url
  )


## WAPO cleaning -------------------------------------------------

# 8416 duplicate case? may be Derrick Ameer Cook (info in 8617)
# reported on the WaPo github repo issue #53
# appears to be deleted now
# wapo <- wapo[wapo$id != 8416,]

wapo_clean <- wapo %>%
  rename(wapoID = id,
         st = state,
         mental_illness = was_mental_illness_related,
         wapo_threat = threat_type,
         wapo_flee = flee_status,
         wapo_bcam = body_camera) %>%
  mutate(name = ifelse(name == "", "Unknown", name),
         name = str_remove(name, " Jr."),
         name = str_remove(name, " Sr."),
         name = str_remove(name, " III"),
         name = str_remove(name, " II"),
         name = str_remove(name, " IV"),
         name = str_remove(name, " V$"),
         name = str_remove(name, " aka.*"),
         name = str_replace_all(name, "-"," "),
         fname = "NA",
         lname = "NA"
  ) %>%
  mutate(age = ifelse(is.na(age), 999, age)) %>% # missing value
  mutate(gender = recode(gender, 
                         "M"="Male", "F"="Female"),
         gender = ifelse(gender=="", "Unknown", gender),
         gender = fct_relevel(gender, "Unknown", after = Inf)
  ) %>%
  
  # note race is multiply coded now in V2
  mutate(race.wapo = ifelse(race=="", "U", race),
         race.wapo = recode(race.wapo,
                       "A" = "API",
                       "B" = "BAA",
                       "H" = "HL",
                       "N" = "NA",
                       "O" = "Other",
                       "W" = "WEA",
                       "U" = "Unknown",
                       .default = "Mixed"),
         race.wapo = fct_relevel(race.wapo, c("Mixed", "Unknown"), 
                            after = Inf)
  ) %>%
  mutate(wapo_armed = armed_with,
         wapo_armed = case_when(
           wapo_armed=="unarmed" ~ "Unarmed",
           wapo_armed=="unknown" | wapo_armed=="" ~ "Unknown",
           TRUE ~ "Alleged Armed")
  ) %>%
  mutate(wapo_weapon = armed_with,
         wapo_weapon = case_when(
           wapo_armed=="Unarmed" ~ "No weapon",
           wapo_armed=="Unknown" ~ "Unknown",
           grepl("gun", wapo_weapon) ~ "Alleged firearm",
           grepl("knife", wapo_weapon) ~ "Alleged edged weapon",
           TRUE ~ "Other")
  ) %>%
  mutate(state = state_fullname_fn(st),
         state.num = num[match(st, states)],
         date = ymd(date),
         month = month(date, label=T),
         day = day(date),
         year = year(date),
         cod = "Gunshot")

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

# Removing a few problem cases:

## When subject kills the victim, but not in a pursuit
## example: Tad Norman case in Lake City

## WaPo 4568
## This is the only WAPO case not found in FE.
## Not clear if the victim died, name unknown, can't find more info online.  
## Have reported the case to FE.
## https://www.kiro7.com/news/local/police-investigating-officer-involved-shooting-in-federal-way/930686522/
## https://komonews.com/news/local/federal-way-police-investigating-officer-involved-shooting

## Filter WA from 2015
fe_2015 <- fe_clean %>% 
  filter(date > "2014-12-31" & st == "WA") %>%
  filter(!grepl("subject", circumstances)) # subject killed victim, not in pursuit

wapo_2015 <- wapo_clean %>% filter(st == "WA") %>%
  filter(wapoID != 4568)

## Merge using stringdist
initialmerge <- stringdist_full_join(
  fe_2015, wapo_2015,
  by = c("lname", "fname", "date", "gender", 
         "cod", "month", "city"),
  max_dist = 2)

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
  select("wapoID", "fname.y", "lname.y")
if(nrow(aaa) > 0){
  cat("\n *** Unmatched WAPO records (expect none):")
  print(aaa)
  if(nrow(aaa) > 1) {
    errors <- errors+1
  } 
} else {
  cat("\n *** No unmatched WAPO records \n\n")
}

### START TEMPORARY FIXES for unmatched WaPo records ##############

## For these cases, assign WAPO info to FE fields, since the FE fields
## are used for some of the reports
## feID will be 99999 to id these cases

## missing info needs to be set manually if needed (in fixes_wapo2FE.R)

## if WaPo info is delayed the temp fix is implemented earlier 
## in fixes_postcleaning

if (dim(aaa)[1] > 0) {
  
  initialmerge[is.na(initialmerge$feID),] <- initialmerge[is.na(initialmerge$feID),] %>% 
    mutate(feID = ifelse(is.na(feID), 99999, feID),
           name.x = name.y,
           fname.x = fname.y,
           lname.x = lname.y,
           age.x = age.y,
           city.x = city.y,
           county.x = county.y,
           latitude.x = latitude.y,
           longitude.x = longitude.y,
           date.x = date.y,
           month.x = month.y,
           day.x = day.y,
           year.x = year.y,
           cod.x = "Gunshot",
           gender.x = gender.y,
           raceImp = race.wapo,
           circumstances = "Deadly force",
           weapon.fe = wapo_weapon,
           homicide = 1,
           armed = wapo_armed,
           weapon = wapo_weapon)
  
  
  # And recheck for unmatched wapoIDs
  aaa <- initialmerge %>% 
    filter(is.na(feID)) %>% 
    select("wapoID", "fname.y", "lname.y")
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
  cat("\n *** Duplicate FE IDs, #times, 99999 means unmatched WaPo case")
  aaa[aaa>1] 
  errors <- errors+1
} else {
  cat("\n *** No duplicate FE IDs \n\n")
}

## Duplicate wapoIDs
aaa <- table(initialmerge$wapoID)
if(any(aaa>1)){
  names(aaa[aaa>1])
  sort(unique(aaa))
  cat("\n *** Duplicate WaPo IDs, #times")
  aaa[aaa>1]
  errors <- errors+1
} else {
  cat("\n *** No duplicate WaPo IDs \n\n")
}


## City/date mismatch

aaa <- initialmerge %>%
  filter(city.y != city.x & date.x != date.y) %>%
  select(feID, wapoID, city.x, city.y, date.x, date.y)
if(nrow(aaa)>1){
  cat("\n *** City & date mismatches: ")
  print(aaa)
  errors <- errors+1
} else {
  cat("\n *** No city & date mismatches \n\n")
}


# County mismatch (lots of empty county.y in wapo, we ignore those) 
aaa <- initialmerge %>%
  filter(county.y != county.x & !is.na(wapoID) & county.y != "") %>%
  select(feID, wapoID, city.x, city.y, county.x, county.y, st.x, st.y)
if(nrow(aaa)>1){
  cat("\n *** County mismatches:")
  print(aaa)
  errors <- errors+1
} else {
  cat("\n *** No county mismatches \n\n")
}

# Check for errors in final dataset ----
if(errors > 0){
  stop(paste(errors, "cleaning errors found.  Stopping"))
} else {
  cleaning.error.message <- "\n\n *** No errors during cleaning!! *** \n\n"
}

# Only if bad merges identified above that can't be fixed by cleaning (very rare)
# The cases are unmerged and remerged as needed
# source("fixes_postmerge.R") 


# Final merged dataset prep for 2015+ WA data ----

## For most variables, we use the FE version.  
## There are many errors in WaPo, and they don't fix them when reported.  
## Any errors we find in FE are either fixed by DBB, MPV or us, 
## so FE has the most reliable data.

## Legislative year variable ----
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
cut.yr <- ifelse(curr.mo > 8, curr.yr+1, curr.yr)

finalmerge <- initialmerge %>%
  mutate(leg.year = 
           cut(date.x, 
               breaks = as.Date(paste(2000:(cut.yr), "-08-01", sep="")),
               labels = c(paste(month.abb[8], 2000:(cut.yr-1), "-", 
                                month.abb[7], 2001:cut.yr,
                                sep = ""))
                 ),
         weapon = case_when(
           grepl("firearm", weapon.fe) | grepl("firearm", wapo_weapon) ~ "Alleged firearm", 
           grepl("edged", weapon.fe) | grepl("edged", wapo_weapon) ~ "Alleged edged weapon", 
           grepl("Other", weapon.fe) | grepl("Other", wapo_weapon) ~ "Alleged other weapon", 
           grepl("No", weapon.fe) | grepl("No", wapo_weapon) ~ "No weapon", 
           TRUE ~ "Unknown"),
         raceImp = if_else(is.na(raceImp), as.character(race.wapo), 
                           as.character(raceImp)),
         raceImp = fct_relevel(raceImp, "Unknown", after = Inf)
         ) %>%
    
  select(feID, wapoID,
         name = name.x, fname = fname.x, lname = lname.x,
         date = date.x, day = day.x, month = month.x, year = year.x,
         leg.year,
         city = city.x, county = county.x, zip,
         latitude = latitude.x, longitude = longitude.x,
         raceOrig, raceImp, race.wapo,
         gender = gender.x,
         age.fe = age.x, age.wapo = age.y,
         mental_illness.fe = foreknowledge, 
         mental_illness.wapo = mental_illness,
         circumstances, homicide, suicide, medical, vpursuit.draft,
         cod = cod.x, homicide, 
         agency, agency.type,
         armed.wapo = wapo_armed, weapon.wapo = wapo_weapon, weapon.fe,
         weapon,
         flee.wapo = wapo_flee,
         bodycam.wapo = wapo_bcam,
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
##   review, we use those versions in the coded.pursuits df.
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
message(cleaning.error.message)
message(pursuit.coding.message)
message(wtsc.update.message)

message(wapo.update.message)
message(last.name.message)

