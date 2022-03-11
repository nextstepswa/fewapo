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

state_translate_fn <- function(x){
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

newnames <- readxl::read_xlsx("temp-newnames.xlsx") %>%
    mutate(Age = as.character(Age),
           `Date of injury resulting in death (month/day/year)` = as.character(`Date of injury resulting in death (month/day/year)`),
           date = ymd(`Date of injury resulting in death (month/day/year)`),
           Latitude = as.character(Latitude))

fe <- bind_rows(fe, newnames)

## Washington Post

wapo_url = "https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv"
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
    mutate(gender = ifelse(Gender == "", "Unknown", Gender),
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
           state = state_translate_fn(st),
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
               grepl("Less", cod) ~ "Unknown", # one case, fe 30304
               grepl("^.*(phyx).*", cod) ~ "Asphyxiated/Restrained",
               TRUE ~ cod),
           cod = factor(
               cod,
               levels = c("Gunshot", "Vehicle", "Tasered", "Medical emergency",
                          "Asphyxiated/Restrained", "Drowned", 
                          "Beaten/Bludgeoned with instrument", "Drug overdose", 
                          "Fell from a height", "Burned/Smoke inhalation", 
                          "Chemical agent/Pepper spray", "Stabbed", "Other", "Unknown")
           )
    ) %>%
    mutate(armed = `Armed/Unarmed`,
           armed = case_when(
               armed == "Armed" ~ "Alledged Armed",
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
    # see email chain with Burghardt re "Intended use of force (Developing)".  
    # Veh/Purs is the unclean mix category that he is slowly working through
    # Vehicle is the clean non-pursuit category (e.g., accident)
    # Pursuit is the clean hot pursuit category
    mutate(hotPursuit = `Intended use of force (Developing)`,
           hotPursuit = case_when(
               hotPursuit == "" ~ "Unknown",
               hotPursuit == "Undetermined" ~ "Unknown",
               hotPursuit == "Vehic/Purs" | hotPursuit == "Pursuit" ~ "Vehicle Pursuit",
               TRUE ~ "Other"),
           hotPursuit = fct_relevel(hotPursuit, "Unknown", 
                                    after = Inf)
    ) %>%
    
    # `Intended use of force (Developing)` is where Pursuits and Suicides are coded
    
    mutate(circumstances = `Intended use of force (Developing)`,
           circumstances = case_when(
               circumstances == "" ~ "Unknown",
               circumstances == "Undetermined" ~ "Unknown",
               circumstances == "Vehic/Purs" ~ "Mix of Vehicle hot pursuits and accidents",
               circumstances == "Pursuit" ~ "Vehicle hot pursuits",
               circumstances == "Vehicle" ~ "Vehicle accidents",
               circumstances == "No" ~ "Unintended",
               TRUE ~ circumstances),
           circumstances = fct_relevel(circumstances, "Unknown", 
                                       after = Inf)
    ) %>%
    # Here we do not classify "suicide" as a homicide.
    mutate(homicide = circumstances,
           homicide = case_when(
               grepl("Suicide|suicide", circumstances) ~ 0,
               circumstances == "Unknown" ~ NA_real_,
               TRUE ~ 1)
    ) %>%
    # NOTE: the `Foreknowledge of mental illness? INTERNAL USE, NOT FOR ANALYSIS`
    # field only flags cases where a mental health issue was known before the
    # officers arrived on the scene.
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
    mutate(state.num = num[match(st, states)]) %>%
    select(
        feID, name, fname, lname,
        date, month, day, year,
        city, county, st, state, state.num, zip, 
        latitude, longitude,
        raceOrig, raceImp, gender, age, ageChr, foreknowledge,
        cod, armed, weapon, fleeing,
        circumstances, hotPursuit, homicide, agency, agency.type,
        description, url_info
    )

### create clickable url for Rpubs reports
fe_clean$url_click <- sapply(fe_clean$url_info, make_url_fn)

## WAPO cleaning -------------------------------------------------

wapo_clean <- wapo %>%
    rename(wapoID = id,
           st = state,
           wapo_cod = manner_of_death,
           wapo_armed = armed,
           foreknowledge = signs_of_mental_illness,
           wapo_threat = threat_level,
           wapo_flee = flee,
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
    mutate(race = recode(race,
                         "A" = "API",
                         "B" = "BAA",
                         "H" = "HL",
                         "N" = "NA",
                         "W" = "WEA",
                         .default = "Unknown"),
           race = fct_relevel(race, "Unknown", 
                              after = Inf)
    ) %>%
    mutate(wapo_armed = recode(wapo_armed,
                               "unknown weapon" = "Unknown"),
           state = state_translate_fn(st),
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

## CSV files ----

write.csv(fe_clean, here("data-outputs", "FE_clean.csv"))
write.csv(wapo_clean, here("data-outputs", "WaPo_clean.csv"))

## Rdata files ----

## key date info

selection <- "all cases"
scrape_date <- Sys.Date()
last_fe_date <- max(fe_clean$date[fe_clean$feID < 900000])
last_wapo_date <- max(wapo_clean$date)
last_newname_date <- max(fe_clean$date[fe_clean$feID > 900000])
last_data_update <- max(last_fe_date, last_wapo_date, last_newname_date)
last_update_is_eoy <- month(last_data_update)==12 & 
    day(last_data_update)==31
last_complete_mo <- ifelse(last_update_is_eoy | month(last_data_update)==1, 
                           12, 
                           month(last_data_update)-1)
last_complete_yr <- ifelse(last_update_is_eoy, 
                           year(last_data_update), 
                           year(last_data_update)-1)

save(list = c("fe_clean", "wapo_clean", "selection",
              "scrape_date", "last_fe_date", "last_wapo_date", "last_newname_date",
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("data-outputs", "CleanData.rda"))



# Merge  ---------
# we only do this for WA state, to make cleaning feasible

# Filter WA from 2015
fe_2015 <- fe_clean %>% 
    filter(date > "2014-12-31" & st == "WA")
wapo_2015 <- wapo_clean %>% filter(st == "WA")

mergefull <- stringdist_full_join(fe_2015, wapo_2015,
                                  by = c("lname", "fname", "date", "gender", "cod"),
                                  max_dist = 2)

## Check for unmatched WAPO records ----
## WAPO is a subset of FE, so all WAPO cases should be in FE
## Missing cases (so far) have all been due to delays in FE updating
## So the fixes are temporary, until FE updates

aaa <- mergefull %>% 
    filter(is.na(feID)) %>% 
    select("wapoID", "fname.y", "lname.y")
if(nrow(aaa) > 0){
    print("Unmatched WAPO records:")
    print(aaa)
} else {
    print("No unmatched WAPO records")
}

### START TEMPORARY FIXES for unmatched WaPo records ##############

## For these cases, assign WAPO info to FE fields, since the FE fields
## are used for some of the reports

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
               homicide = 1)
}

## WaPo doesn't have all the info needed, so we fill in the rest manually if we have it


# # Sahota
target <- which(mergefull$wapoID==7650 & mergefull$feID==99999)
if(length(target > 0)) {
    print("Fixing Sahota")
    mergefull$agency[target] <- "Clark County Sheriff's Office"
    mergefull$county[target] <- "Clark"
    mergefull$url_info[target] <- "https://www.oregonlive.com/clark-county/2022/02/vancouver-police-officer-mistakenly-shot-by-clark-county-deputy-died-of-multiple-gunshot-wounds.html"
    mergefull$url_click[target] <- make_url_fn(mergefull$url_info[target])
} else {
    print("Sahota fix not needed anymore")
}


### END TEMPORARY FIXES #######################################

# Checks for bad matches from the merge ----
# these are relatively stable, and need to be fixed manually

# Check for duplicate feIDs
aaa <- table(mergefull$feID)
if(any(aaa>1)){
    names(aaa[aaa>1])
    sort(unique(aaa))
    print("Duplicate FE IDs, #times")
    aaa[aaa>1] 
} else {
    print("No duplicate FE IDs")
}

# Check for duplicate wapoIDs
aaa <- table(mergefull$wapoID)
if(any(aaa>1)){
    names(aaa[aaa>1])
    sort(unique(aaa))
    print("Duplicate WaPo IDs, #times")
    aaa[aaa>1]
} else {
    print("No duplicate WaPo IDs")
}

# Check for city/date mismatch
mergefull %>%
    filter(city.y != city.x & date.x != date.y) %>%
    select(feID, wapoID, city.x, city.y, date.x, date.y)
 
## Spot fix these cases manually

### indices for wapo vars in mergefull
indices <- data.frame(colnames(mergefull))
start <- as.numeric(row.names(indices)[indices=="wapoID"])
end <- as.numeric(row.names(indices)[indices=="year.y"])
len <- end-start+1

### Fix incorrect matches
mergefull[mergefull$feID==25615,start:end] <- rep(NA, len) # fixes wapoID 4568 (may not be a fatality, see below)
 
### fix for wapoID 5242: transfer from feID 26695 to 27067
mergefull[mergefull$feID==27067,start:end] <- mergefull[mergefull$feID==26695,start:end]
mergefull[mergefull$feID==27067,]$wapoID <- 5242
mergefull[mergefull$feID==26695,start:end] <- rep(NA, len)
mergefull[mergefull$feID==26695,]$wapoID <- NA

### fix for wapoID 7341: delete duplicate match, leave match to 31223
mergefull[mergefull$feID==31303,start:end] <- rep(NA, len)
mergefull[mergefull$feID==31303,]$wapoID <- NA

### fix duplicate FE ID 31303, not matched to WaPo, so not sure how this ends up getting duplicated

#keep <- finalmerge[finalmerge$feID == 31303,][1,]
#finalmerge <- bind_rows(finalmerge[finalmerge$feID != 31303,], keep)

# Re-check city/date mismatch
mergefull %>%
    filter(city.y != city.x & date.x != date.y) %>%
    select(feID, wapoID, city.x, city.y, date.x, date.y)

# Re-check age mismatch
mergefull %>% filter(age.x != age.y) %>% select(feID, wapoID, age.x, age.y)

# Check that all wapoIDs are still in the merged dataset
if(table(!is.na(mergefull$wapoID))[[2]] != dim(wapo_2015)[1]) {
    print("IDs of non-matched WaPo cases:")
    wapo_2015$wapoID[is.na(!match(wapo_2015$wapoID, mergefull$wapoID))]
} else {
    print("All wapo cases matched")
}
# NOTE: WAPO 4568 is the only case not found in FE.
# Not clear if the victim died, so not included here.  
# Have reported the case to FE.
# https://www.kiro7.com/news/local/police-investigating-officer-involved-shooting-in-federal-way/930686522/

# spotfix WA homicide designations: optional 
## we don't currently do this b/c 
## we haven't looked at all cases, all years, so don't want to make
## one anomalous fix
# mutate(homicide = ifelse(feID == 28698 | #deputy crash while having stroke
#                              feID == 25804 | #next 2 killed by suspect Tad Norman, not police
#                              feID == 25805, 
#                          0, homicide))

# Final merged dataset for 2015-current data ----

finalmerge <- mergefull %>%
    select(feID, wapoID,
           name = name.x, fname = fname.x, lname = lname.x,
           date = date.x, day = day.x, month = month.x, year = year.x,
           city = city.x, county, zip,
           latitude = latitude.x, longitude = longitude.x,
           raceOrig, raceImp, race.wapo = race,
           gender = gender.x,
           age.fe = age.x, age.wapo = age.y,
           foreknowledge.fe = foreknowledge.x, foreknowledge.wapo = foreknowledge.y,
           homicide, circumstances,
           cod.fe = cod.x, cod.wapo = wapo_cod,
           hotPursuit,
           homicide, agency, agency.type,
           armed.wapo = wapo_armed,
           threat.wapo = wapo_threat,
           flee.wapo = wapo_flee,
           bodycam.wapo = wapo_bcam,
           description,
           url_info,
           url_click
           )


# Save WA clean and merged datasets as Rdata files ----
## Note that the fe and wapo data include all cases, not just WA.
## For WA only analysis, use the merged_data

##  2015 and later ----
selection <- "2015+"
fe_data <- fe_clean %>% filter(date > "2014-12-31")
wapo_data <- wapo_clean
merged_data <- finalmerge 

save(list = c("fe_data", "wapo_data", "merged_data", "selection",
              "scrape_date", "last_fe_date", "last_wapo_date","last_newname_date",
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("data-outputs", "WA2015.rda"))

##  Since 940 ----
selection <- "since 940"
fe_data <- fe_clean %>% filter(date > "2018-12-06")
wapo_data <- wapo_clean %>% filter(date > "2018-12-06")
merged_data <- finalmerge %>% filter(date > "2018-12-06")

save(list = c("fe_data", "wapo_data", "merged_data", "selection",
              "scrape_date", "last_fe_date", "last_wapo_date","last_newname_date",
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("data-outputs", "WA940.rda"))


