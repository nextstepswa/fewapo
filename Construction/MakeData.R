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

source(here::here("Construction", "pursuit_coding.R"))

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


# Join leg.info ----

finalmerge <- left_join(finalmerge, wa_LDxCity)

## check for missing cases, if so stop and fix

if(any(is.na(finalmerge$WA_District))) { stop("Missing LegDists") }
  
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
message(matching.message)
message(cleaning.error.message)

message(newname.update.message)
message(wapo.update.message)
message(mpv.update.message)
message(last.name.message)

message(pursuit.coding.message)
message(wtsc.update.message)

# Quick descriptives
#Hmisc::describe(finalmerge)
