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

## Case fixes ----
## ID'd during merging & needed before cleaning
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
         age = ifelse(is.na(age), 999, age) # missing value
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
    grepl("^.*(Campus|University|College).*", agency) ~ "University Campus Police",
    grepl("^.*(State|Patrol).*", agency) ~ "State Police",
    grepl("^.*Sheriff.*", agency) ~ "County Sheriff's Office",
    grepl("^.*(Police|police|Public Safety|Town|City).*", agency) ~ "Local Police Department",
    grepl("^.*Correction.*", agency) ~ "Corrections Dept",
    grepl("^.*County.*", agency) ~ "Other County level unit",
    grepl("^.*U.S.*Alchohol.*", agency) ~ "US ATF",
    grepl("^.*U.S.*Border.*", agency) ~ "US CBP",
    grepl("^.*U.S. Federal Bureau.*", agency) ~ "US FBI",
    grepl("^.*U.S. Immigration.*", agency) ~ "US ICE",
    grepl("^.*U.S. Marshal.*", agency) ~ "US Marshals",
    grepl("^.*U.S.|Military|Air Force.*", agency) ~ "Other US level unit",
    TRUE ~ "Other State level unit"),
    agency.type = factor(
      agency.type, 
      levels = c("Local Police Department", "County Sheriff's Office", "State Police", 
                 "University Campus Police", "Corrections Dept", 
                 "US CBP", "US ICE", "US FBI", "US Marshals", 
                 "Other State level unit", "Other County level unit", "Other US level unit",
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
  ## inspection suggests he hasn't cleaned WA prior to 2016, so we do that below
  
  # several variables are drawn from this field:
  
  ## circumstances - cleaned up variable
  ## vpursuit.draft -- starts with circumstances, 
  ##                   final version is manually reviewed and coded, 
  ##                   sourced from external file pursuit_coding.R
  ## homicide - excludes suicides, and the Lake city case

  # circumstances: cleaned up FE variable,
  # also recode to new categories added for 2022+ for 
  # "Pursuit involved", "Attempted stop", "Terminated pursuit",
  # those are added to
  # 2015-2021 cases by manual review in pursuit_coding.R

  mutate(circumstances = `Intended use of force (Developing)`,
         circumstances = case_when(

           ### Pursuits: Recode the circumstances variable where needed ----
           #   All cases coded by DBB as Vehicle, Pursuit or Mix were reviewed 
           #   Cases below needed recoding
           
           feID == 22977 ~ "Pursuit involved", # pursued/stopped/fled/train
           feID == 30730 ~ "Pursuit involved", # pursued/stopped/fled/drowned
           feID == 27304 ~ "Active pursuit", # was coded mix
           feID == 25804 ~ "Active pursuit", # was coded other
           feID == 26985 ~ "Active pursuit", # was coded mix
           feID == 27119 ~ "Active pursuit", # was coded mix
           feID == 27219 ~ "Active pursuit",  # was coded mix
           feID == 28698 ~ "Vehicle accident", # was coded other
           
           circumstances == "" ~ "Unknown",
           circumstances == "Undetermined" ~ "Unknown",
           circumstances == "Vehic/Purs" ~ "Mix of vehicle pursuit and other",
           circumstances == "Pursuit" ~ "Active pursuit",
           circumstances == "Vehicle" ~ "Vehicle accident",
           circumstances == "No" ~ "Unintended",
           grepl("Nonlethal|Less-than", circumstances) ~ "Less lethal force",
           TRUE ~ circumstances),
         
         circumstances = fct_relevel(circumstances, "Unknown",
                                     after = Inf)
       ) %>%

  ## vpursuit.draft: the starting variable for pursuit case id and cleaning
  ## categories are:
  ## Active pursuit - fatality occurred during pursuit (ID'd by circumstances)
  ## Pursuit involved - death occurred post pursuit (often shot)
  ## Attempted stop - attempted stop or terminated pursuit, accident & fatality resulted
  ## Vehicle accident - non-pursuit, non-fleeing incidents
  
  mutate(vpursuit.draft = case_when(
    circumstances == "Active pursuit" ~ "Active pursuit", # gets all coded by DBB & corrected
    circumstances == "Pursuit involved" ~ "Pursuit involved", # gets the 2022+ cases for now
    circumstances == "Attempted stop" ~ "Attempted stop", # gets the 2022+ cases for now
    circumstances == "Pursuit terminated" ~ "Pursuit terminated" # gets the 2022+ cases for now
  )) %>%
  
  
  # homicide:  Exclude suicides and "Other" (Lake City and Deputy stroke)
  # note this retains pursuit deaths
  
  mutate(homicide = case_when(
    grepl("Suicide|Other", circumstances) ~ 0,
    circumstances == "Unknown" ~ NA_real_,
    TRUE ~ 1)
  ) %>%
  
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
## until MPV issues are sorted out

fe_clean <- fe_clean %>%
  select(
    feID, name, fname, lname,
    date, month, day, year,
    city, county, st, state, state.num, zip, 
    latitude, longitude,
    raceOrig, raceImp, gender, age, ageChr, foreknowledge,
    cod, armed, weapon, fleeing,
    circumstances, homicide, vpursuit.draft,
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
  mutate(race = ifelse(race=="", "U", race),
         race = recode(race,
                       "A" = "API",
                       "B" = "BAA",
                       "H" = "HL",
                       "N" = "NA",
                       "O" = "Other",
                       "W" = "WEA",
                       "U" = "Unknown",
                       .default = "Mixed"),
         race = fct_relevel(race, c("Mixed", "Unknown"), 
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


# Final prep -------------------------------------------------

## post-cleaning fixes (mostly for WA State) ----
source(here("DataCleaningScripts", "fixes_postcleaning.R"))

##FE name processing ----
name.list <- str_split(fe_clean$name, " ")
for(i in 1:length(name.list)) {
  fe_clean$fname[i] <- name.list[[i]][1]
  fe_clean$lname[i] <- name.list[[i]][length(name.list[[i]])]
}

##WaPo name processing ----
name.list <- str_split(wapo_clean$name, " ")
for(i in 1:length(name.list)) {
  wapo_clean$fname[i] <- name.list[[i]][1]
  wapo_clean$lname[i] <- name.list[[i]][length(name.list[[i]])]
}

# Save clean datasets, all cases ----
## NOTE: fixes id'd during WA merge below 
## are implemented in post-cleaning file above so included here

## CSV files ----

write.csv(fe_clean, here("data-outputs", "FE_clean.csv"))
write.csv(wapo_clean, here("data-outputs", "WaPo_clean.csv"))

## Rdata files ----

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



# Merge  ---------
# we only do this for WA state, to make cleaning feasible

# Filter WA from 2015
fe_2015 <- fe_clean %>% 
  filter(date > "2014-12-31" & st == "WA")
wapo_2015 <- wapo_clean %>% filter(st == "WA")

mergefull <- stringdist_full_join(
  fe_2015, wapo_2015,
  by = c("lname", "fname", "date", "gender", 
         "cod", "month", "city"),
  max_dist = 2)

## Check for unmatched WAPO records ----
## WAPO is a subset of FE, so all WAPO cases should be in FE
## Missing cases are due to delays in FE updating
## So the fixes are temporary, 

# except for WaPo 4568

# WaPo 4568 is the only case not found in FE.
# Not clear if the victim died, name unknown, can't find more info online.  
# Have reported the case to FE.
# https://www.kiro7.com/news/local/police-investigating-officer-involved-shooting-in-federal-way/930686522/
# https://komonews.com/news/local/federal-way-police-investigating-officer-involved-shooting

aaa <- mergefull %>% 
  filter(is.na(feID)) %>% 
  select("wapoID", "fname.y", "lname.y")
if(nrow(aaa) > 0){
  print("Unmatched WAPO records (expect 4568):")
  print(aaa)
} else {
  print("No unmatched WAPO records")
}

### START TEMPORARY FIXES for unmatched WaPo records ##############

## For these cases, assign WAPO info to FE fields, since the FE fields
## are used for some of the reports
## feID will be 99999 to id these cases

## missing info set manually if needed (in fixes_wapo2FE.R)

## if WaPo info is delayed the temp fix is implemented earlier 
## in fixes_postcleaning

if (dim(aaa)[1] > 0) {
  
  mergefull[is.na(mergefull$feID),] <- mergefull[is.na(mergefull$feID),] %>% 
    mutate(feID = ifelse(is.na(feID), 99999, feID),
           name.x = name.y,
           fname.x = fname.y,
           lname.x = lname.y,
           age.x = age.y,
           city.x = city.y,
           latitude.x = latitude.y,
           longitude.x = longitude.y,
           date.x = date.y,
           month.x = month.y,
           day.x = day.y,
           year.x = year.y,
           cod.x = "Gunshot",
           gender.x = gender.y,
           raceImp = race,
           circumstances = "Deadly force",
           homicide = 1,
           armed = wapo_armed,
           weapon = wapo_weapon)
  
  # 4568 in WaPo not FE, may not be a fatality, needs county
  #source(here("DataCleaningScripts", "fixes_wapo2FE.R"))
  mergefull$county[mergefull$wapoID==4568] <- "King"
  
  
  # And recheck for unmatched wapoIDs
  aaa <- mergefull %>% 
    filter(is.na(feID)) %>% 
    select("wapoID", "fname.y", "lname.y")
  if(nrow(aaa) > 0){
    print("Unmatched WAPO records (expect none):")
    print(aaa)
  } else {
    print("No unmatched WAPO records after info transfer")
  }
  
}

### END TEMPORARY FIXES FOR UNMATCHED WAPO RECORDS #############

# Checks for bad matches from the merge ----
# these are relatively stable, and need to be fixed manually
# Most should be fixed by modifying incorrect info in the
# pre or post cleaning fix files.  But if that is not enough to
# prevent the match, then the match is reversed by hand in
# the fixes_postmerge file

# Duplicate feIDs
aaa <- table(mergefull$feID)
if(any(aaa>1)){
  names(aaa[aaa>1])
  sort(unique(aaa))
  print("Duplicate FE IDs, #times, 99999 means unmatched WaPo case")
  aaa[aaa>1] 
} else {
  print("No duplicate FE IDs")
}

# Duplicate wapoIDs
aaa <- table(mergefull$wapoID)
if(any(aaa>1)){
  names(aaa[aaa>1])
  sort(unique(aaa))
  print("Duplicate WaPo IDs, #times")
  aaa[aaa>1]
} else {
  print("No duplicate WaPo IDs")
}


# City/date mismatch

aaa <- mergefull %>%
  filter(city.y != city.x & date.x != date.y) %>%
  select(feID, wapoID, city.x, city.y, date.x, date.y)
if(nrow(aaa)>1){
  print("City & date mismatches:")
  aaa
} else {
  print("No city & date mismatches")
}

# County mismatch
# print("County mismatches (fix FE only, not WaPo):")
# mergefull %>%
#   filter(county.y != county.x & !is.na(wapoID)) %>%
#   select(feID, wapoID, city.x, city.y, county.x, county.y, st.x, st.y)

#source("fixes_postmerge.R") # only if bad merges identified above

# Final merged dataset for 2015-current data ----

## For most variables, we use the FE version.  There are many errors in
## WaPo, and they don't fix them when reported.  Any errors we find in
## FE are either fixed by DBB, MPV or us, so FE has the most reliable
## data.

## We create an approximate legislative year variable, to use for
## before and after assessments.  Since some bills take effect immediately
## (typically in late May) and others 90 days after end of session 
## (typically late July), and that also varies by short/long leg year
## we use a standardized approx leg year of Jun 1 - May 30.


curr.yr <- lubridate::year(Sys.Date())
curr.mo <- lubridate::month(Sys.Date())
cut.yr <- ifelse(curr.mo > 6, curr.yr+1, curr.yr)

finalmerge <- mergefull %>%
  mutate(leg.year = 
           cut(date.x, 
               breaks = as.Date(paste(2000:(cut.yr), "-06-01", sep="")),
               labels = c(paste(month.abb[6], 2000:(cut.yr-1), "-", 
                                month.abb[5], 2001:cut.yr,
                                sep = ""))
  )) %>%
    
  select(feID, wapoID,
         name = name.x, fname = fname.x, lname = lname.x,
         date = date.x, day = day.x, month = month.x, year = year.x,
         leg.year,
         city = city.x, county = county.x, zip,
         latitude = latitude.x, longitude = longitude.x,
         raceOrig, raceImp, race.wapo = race,
         gender = gender.x,
         age.fe = age.x, age.wapo = age.y,
         mental_illness.fe = foreknowledge, 
         mental_illness.wapo = mental_illness,
         circumstances, homicide, vpursuit.draft,
         cod = cod.x, homicide, 
         agency, agency.type,
         armed.wapo = wapo_armed,
         threat.wapo = wapo_threat,
         flee.wapo = wapo_flee,
         bodycam.wapo = wapo_bcam,
         description,
         url_info,
         url_click,
         officer_names,
         officer_url
  ) %>%
  arrange(date)


## ###########################################################################################
## Final Pursuit coding for FE ----
## WA, 2015+ only

## This is a multi-stage process:
##  1. above in FE cleaning loop:
##     clean up the circumstances variable
##     use it to create vpursuit.draft

##  2. below:
##     create pursuit.tag -- uses all info from both FE and wapo to tag possible pursuit cases
##     output and manually review !is.na(pursuit.tag) cases for detailed vpursuit coding
##     output and manually review active pursuit cases (pursuit.tag=1)
##      for who was killed/injured and to assign unique incident number

## pursuit.tag: all possible pursuit cases
## used to review all cases that might be classified into one of the
## pursuit or vehicle categories

finalmerge <- finalmerge %>%
  mutate(pursuit.tag = case_when(
    grepl("Active|terminated", vpursuit.draft) ~ 1,
    !is.na(vpursuit.draft) ~ 2,
    grepl('vehicle|car|crash|speed|chase|pursuit', description) |
      grepl('car|Car', flee.wapo) |
      grepl("pursuit", circumstances) ~ 3, #many are people killed in their cars w/o pursuit
    cod == "Vehicle" ~ 4 # this doesn't seem to pick up any additional, but may in the future
  ))


## Output cases for review (has already been done, in all-pursuits-coded.xlsx)
## We still output them in case we want to review again

aaa <- finalmerge %>% 
  filter(!is.na(pursuit.tag)) %>% 
  select(c(feID, name, date, pursuit.tag, description, url_info)) %>%
  arrange(pursuit.tag, date)
write.csv(aaa, here::here("data-outputs", "all-pursuits.csv"))

## Manually code the cases into vpursuit categories

source(here::here("DataCleaningScripts", "pursuit_coding.R"))

## Step 2: active pursuits ---- 
## Manually review/code these cases
## to classify who was killed/injured and assign a unique incident number

### These cases need manual updating whenever there is a new
### active pursuit fatality.  (pursuit.tag=1) 

# active-pursuits.csv is USED FOR MANUAL UPDATES TO coded-pursuits.xlsx;
## it is not read back in here for the cleaned 940 or 2015 finalmerge datasets
## it is used for the WApursuits.R analysis

bbb <- finalmerge %>% 
  filter(pursuit.tag==1) %>% 
  select(c(feID, name, date, pursuit.tag, vpursuit, description, url_info)) %>%
  arrange(pursuit.tag, date)
write.csv(bbb, here::here("data-outputs", "active-pursuits.csv"))


# Save WA clean and merged datasets as Rdata files ----
## Note that the fe and wapo data include all cases, not just WA.
## For WA only analysis, use the merged_data

##  2015 and later ----
selection <- "2015+"
fe_data <- fe_clean %>% filter(date > "2014-12-31")
wapo_data <- wapo_clean
merged_data <- finalmerge 

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




# Print last name and dates
print(paste("Last Wapo update: ", last_date_wapo))
print(paste("Last WA fatality: ",
            merged_data$name[nrow(merged_data)],
            merged_data$date[nrow(merged_data)]))

