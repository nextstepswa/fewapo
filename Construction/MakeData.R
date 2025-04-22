rm(list=ls())

# This file is now obsolete because both FE (12/31/2021) and WaPo (12/31/2024) have stopped updating
# We keep this working version for historical purposes
# It updates FE for WA state only thru 12/31/2024

##### What this file does ################################################################
#
# This script runs 3 main tasks:
# 1. Scrapes current data from the original sources online, and merges in additional data
# 2. Cleans the data -- both bulk cleaning and detailed error corrections
# 3. Merges the data from FE and WaPo - for WA state only
#
# Resulting cleaned data is saved as an output
#
# The code is designed to stop with an informative message if it finds errors
# that need to be manually addressed

# The typical reasons for this will be:
## new pursuit review/coding
## changes in source file formats (common for MPV)
## duplicate cases resulting from the FE-WaPo merge

# If it stops, the errors need to be manually fixed/updated
# and then this file can be re-run (repeatedly, if needed
# until no more errors/updates are flagged)

###########################################################################################

#library(devtools)
library(tidyverse)
library(readxl)
library(maps)
library(here)
library(fuzzyjoin)
library(lubridate)

# Functions ----

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

# Scrape data (script) ----

## 3 original sources are scraped:
## FE (with WA-only updates post 2021), MPV, and WaPo

## One pre-constructed dataset:  WA legislative districts
## This requires a manual geocoding step from geocodio

source(here("Construction", "scrapeDataSets.R"))


# Cleaning & Variable construction (script) ----

## Pre-cleaning error corrections:
## These errors are typically ID'd during merging
## If they are not due to temporary lag in info, they are fixed here

## FE cleaning ----

source(here("Construction", "fixes_preclean_fe.R"))
source(here("Construction", "clean_fe.R"))

## MPV cleaning ----

source(here("Construction", "fixes_preclean_mpv.R"))
source(here("Construction", "clean_mpv.R"))

## WAPO cleaning -----

source(here("Construction", "fixes_preclean_wapo.R"))
source(here("Construction", "clean_wapo.R"))


# Final prep:  FE & WaPo only ------

## Post-cleaning fixes (for WA State) ----
## Temp info lags are addressed here, and removed when obsolete

source(here("Construction", "fixes_postcleaning.R"))

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

write.csv(fe_clean, here("Data", "Clean", "FE_clean.csv"))
write.csv(wapo_clean, here("Data", "Clean", "WaPo_clean.csv"))
write.csv(mpv_clean, here("Data", "Clean", "MPV_clean.csv"))

## Rdata files ----
## These save lots of metadata as well

## Key date info

selection <- "all cases"
scrape_date <- Sys.Date()

last_date_fe <- max(fe_clean$date[fe_clean$feID < 90000])
last_date_newname <- max(fe_clean$date[fe_clean$feID > 90000])
last_date_wapo <- max(wapo_clean$date)
last_date_mpv <- max(mpv_clean$date)

last_data_update <- max(last_date_wapo, last_date_mpv, last_date_newname)

last_update_is_eoy <- month(last_data_update)==12 & 
  day(last_data_update)==31
last_complete_mo <- ifelse(last_update_is_eoy | month(last_data_update)==1, 
                           12, 
                           month(last_data_update)-1)
last_complete_yr <- ifelse(last_update_is_eoy, 
                           year(last_data_update), 
                           year(last_data_update)-1)

save(list = c("fe_clean", "wapo_clean", "mpv_clean", 
              "selection", "scrape_date", 
              "last_date_fe", "last_date_newname",
              "last_date_wapo", "last_date_mpv", 
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("Data", "Clean", "CleanData.rda"))

newname.update.message <- paste("\n *** Last newname update: ", last_date_newname, "\n\n")
wapo.update.message <- paste("\n *** Last Wapo update: ", last_date_wapo, "\n\n")
mpv.update.message <- paste("\n *** Last MPV update: ", last_date_mpv, "\n\n")

# Merge  -----

# We only do this for WA state, to make cleaning feasible
# And WaPo is 2015+

# Note:  MPV has merged all cases, so we may be able to use that later
# We've reported, and they've fixed, some merge errors
# But we're not relying on their merge for now.


## Filter WA from 2015
fe_2015 <- fe_clean %>% 
  filter(date > "2014-12-31" & st == "WA") %>%
  filter(not.kbp==0) # subject killed victim, not in a pursuit incident

wapo_2015 <- wapo_clean %>%
  filter(st == "WA")

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

## If errors are found, fixes are made either to the 
## * pre- and/or post- cleaning fix files, if they are data errors
## * additions to the fe_newnames.xlsx file, if new record is needed
## * postmerge fix file, if they are bad matches after all other fixes
## And this script is run again until it runs cleanly

### 1. Unmatched wapo cases ----

## Should be none b/c wapo is a subset of FE
## Stop if found, and fix by inspection 

## A.  If there is no matching FE record at all, add one to fe_newnames.xlsx

## B. If there is an FE record, but info needed for match is missing/inconsistent
## This is usually temporary, and fix (WaPo and/or FE) is implemented in fixes_postcleaning.
## That fix is deleted when no longer needed.

unmatched.wapo <- initialmerge %>% 
  filter(is.na(feID)) %>% 
  select(wapoID, fname.wapo, lname.wapo)

if(nrow(unmatched.wapo) > 0){
  
    message("\n *** Unmatched WAPO records: *** \n\n")
    print(unmatched.wapo)
    stop("\n\n Stopping to fix unmatched WAPO records \n\n")
    
} else {
  message("\n *** No unmatched WAPO records *** \n\n")
}

### 2. Duplicate matches from the merge ----

## Stop and fix manually
## Most should be fixed by modifying incorrect info in the
## pre or post cleaning fix files.  But if that is not enough to
## prevent the match, then as a last resort the match is reversed by hand in
## the fixes_postmerge file

## Stop if found, and fix by inspection 

## A. If due to bad or missing info, fix in pre-cleaning

## B. If resistant, fix manually in fix_(fe/wapo)dupes.R (last resort).

## Note: this can't be automated; instead need to
##  modify the script in fix_(fe/wapo)dupes.R with the relevant IDs
##  set resistant to 1 below to source the script

## 

#### a. Duplicate feIDs ----

dupe.fe <- table(initialmerge$feID)

# This should normally be set to 0, unless there has been a new resistant
# duplicate match.  Then set at 1 and leave until no longer needed.

resistant <- 0

# Check if resistant should be reset
if(resistant==1) {
  if(!any(dupe.fe>1)){ 
    resistant <- 0
    resistant.message <- "reset resistant duplicate match to 0 for FE"
    message(resistant.message)
  } else {
    resistant.message <- "resistant duplicate match left at 1 for FE"
    message(resistant.message)
  }
}

if(any(dupe.fe>1)){
  
  names(dupe.fe[dupe.fe>1])
  sort(unique(dupe.fe))
  message("\n *** Duplicate FE IDs, #times, 99999 means unmatched WaPo case")
  print(dupe.fe[dupe.fe>1])
  
  if(resistant==0) {
    stop("\n\n Stopping to fix duplicate FE IDs \n\n")
  } else {
    message("\n\n Fixing FE dupes by script \n\n")
    source(here("Construction", "fix_fe_dupes.R"))
  }
  
} else {
  message("\n *** No duplicate FE IDs **** \n\n")
}

#### b. Duplicate wapoIDs ----

dupe.wapo <- table(initialmerge$wapoID)

# This should normally be set to 0, unless there is a new resistant
# duplicate match.  Then set at 1 and leave until no longer needed.

resistant <- 0

# Check if resistant should be reset
if(resistant==1) {
  if(!any(dupe.wapo>1)){ 
    resistant <- 0
    resistant.message <- "reset resistant duplicate match to 0 for WaPo"
    message(resistant.message)
  } else {
    resistant.message <- "resistant duplicate match left at 1 for WaPo"
    message(resistant.message)
  }
}

if(any(dupe.wapo>1)){
  
  names(dupe.wapo[dupe.wapo>1])
  sort(unique(dupe.wapo))
  message("\n *** Duplicate WaPo IDs, #times \n\n")
  print(dupe.wapo[dupe.wapo>1])
  
  if(resistant==0) {
    stop("\n\n Stopping to fix duplicate WaPo IDs \n\n")
  } else {
    message("\n\n Fixing WaPo dupes by script \n\n")
    source(here("Construction", "fix_wapo_dupes.R"))
  }
  
} else {
  message("\n *** No duplicate WaPo IDs **** \n\n")
}


### 3. Inconsistencies in merged info ----

## Not necessary to stop, but should be reviewed
## Fuzzy string matching allows for some errors in key variables
## Fix by inspection as time permits (and ideally report)
## Make corrections as needed in pre-cleaning fixes or fe_newnames

attribute.mismatch <- initialmerge %>%
  filter(!is.na(feID) & !is.na(wapoID)) %>%
  mutate(flname.wapo = paste(fname.wapo, lname.wapo),
         flname.fe = paste(fname.fe, lname.fe)) %>%
  filter(flname.wapo != flname.fe |
           date.fe != date.wapo |
           gender.wapo != gender.fe | 
           age.wapo != age.fe |  
           city.wapo != city.fe) %>%
  select(feID, wapoID, flname.fe, flname.wapo, date.fe, date.wapo,
         gender.fe, gender.wapo, age.fe, age.wapo, city.fe, city.wapo)

if(nrow(attribute.mismatch)>1){
  message("\n *** Attribute mismatches for review: ")
  head(attribute.mismatch)

} else {
  message("\n *** No attribute mismatches \n\n")
}


#### d. County mismatch (lots of missing or empty county in wapo, we ignore those) 
county.mismatch <- initialmerge %>%
  filter(county.wapo != county.fe & !is.na(wapoID) & county.wapo != "") %>%
  select(feID, wapoID, city.fe, city.wapo, county.fe, county.wapo, st.fe, st.wapo)

if(nrow(county.mismatch)>0){
  cat("\n *** County mismatches for review: \n\n")
  print(county.mismatch)

} else {
  cat("\n *** No county mismatches \n\n")
}


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

source(here::here("Construction", "pursuit_coding.R"))

## Add vpursuit codes to both finalmerge & fe_2015
## Since the cod, url_info and description may have been improved during pursuit
##   review, we use final versions in the coded.pursuits df.
## Will also use the new coded vars vpursuit, victim, injury
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


# Join leg.info ----
# This is a premade dataset, read in via scrapeDataSets.R above
# Uses geocodio created file, with all FE incidents thru 90094
# Merge is by cityname, so will match all new cases from cities
# included thru 90094.  If a new city crops up (rare), this needs to
# be added to the geocodio csv file manually, and the make_leg_info
# file rerun to rebuild the leg info file.

finalmerge <- left_join(finalmerge, wa_LDxCity)

## check for missing cases, if so stop and fix

if(any(is.na(finalmerge$WA_District))) 
  {
  stop(paste("Missing LegDists for feID", finalmerge$feID[(is.na(finalmerge$WA_District))])) 
       }
  
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

save(list = c("fe_data", "wapo_data", "merged_data", 
              "selection", "scrape_date", 
              "last_date_fe", "last_date_newname",
              "last_date_wapo", "last_date_mpv",
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("Data", "Clean", "WA2015.rda"))


# Print summary of run

if(exists("resistant.message")){message(resistant.message)}
message(newname.update.message)
message(wapo.update.message)
message(mpv.update.message)
message(last.name.message)
message(pursuit.coding.message)
message(wtsc.update.message)

# Quick descriptives
#Hmisc::describe(finalmerge)
