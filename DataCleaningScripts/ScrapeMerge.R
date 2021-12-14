rm(list=ls())
library(devtools)
library(tidyverse)
library(readxl)
library(stringr)
library(forcats)
library(maps)
library(here)
library(fuzzyjoin)

# functions ----

state_translate <- function(x){
    snames <- c(state.name, "District of Columbia")
    names(snames) <- c(state.abb, "DC")
    snames[x]
}

scrape_data <- function(dataset_name, url, save_file) {
    dataset_name <- rio::import(url)
    write.csv(dataset_name, save_file)
    dataset_name
}

make_url <- function(x) {
    paste(sprintf('<a href="%1$s">%1$s</a>', x),
          collapse=",")
}

#' Fatal Encounters
#source(file.path(path_to_src, "MakeFEData.R"))

doc_id = "1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE"
url_template = 'https://docs.google.com/spreadsheets/d/DOC_ID/export?format=tsv'
fe_url = sub("(*.)DOC_ID(*.)", 
             paste("\\1", doc_id, "\\2", sep=""), 
             url_template)
fe_save_file = here("Downloads", "fe_raw.csv")

fe <- scrape_data(fe, fe_url, fe_save_file)


#' Washington Post
#source(file.path(path_to_src, "MakeWaPoData.R"))

wapo_url = "https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv"
wapo_save_file = here("Downloads", "wapo_raw.csv")

wapo <- scrape_data(wapo, wapo_url, wapo_save_file)

# FE cleaning -------------------------------------------------

fe_clean <- fe %>%
    filter(!is.na(`Unique ID`)) %>%
    mutate(
        feID = `Unique ID`,
        name = case_when(
            Name == "Name withheld by police" ~ "Unknown",
            TRUE ~ Name),
        name = str_remove(name, " Jr."),
        name = str_remove(name, " Sr."),
        name = str_replace_all(name, "-"," "),
        name = ifelse(feID == 28826, "Andrea Churna", name),
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
    mutate(gender = case_when(
        Gender == "" ~ "Unknown",
        TRUE ~ Gender),
        gender = fct_relevel(`gender`, "Unknown", after = Inf)
    ) %>%
    # age is a mess, chr & lots of typos, lots of NA, case_when doesn't like "-"
    mutate(Age = gsub("-","", Age),
           ageChr = case_when(Age == "1825" ~ "22",
                              Age == "55." ~ "55",
                              Age == "4050" ~ "45",
                              TRUE ~ Age),
           age = as.numeric(ageChr)
    ) %>%
    mutate(date = 
               lubridate::mdy(`Date of injury resulting in death (month/day/year)`),
           date = case_when(feID == 27052 ~ lubridate::ymd("2019-11-22"),
                            TRUE ~ date),
           month = lubridate::month(date, label=T),
           day = lubridate::day(date),
           year = lubridate::year(date)
    ) %>%
    mutate(city = `Location of death (city)`,
           st = State,
           state = state_translate(st),
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
               circumstances == "Suicide" ~ 0,
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
    select(
        feID, name, fname, lname,
        date, month, day, year,
        city, county, st, state, zip, 
        latitude, longitude,
        raceOrig, raceImp, gender, age, ageChr, foreknowledge,
        cod, armed, weapon, fleeing,
        circumstances, hotPursuit, homicide, agency, agency.type,
        description, url_info
        )

# create clickable url for Rpubs reports
fe_clean$url_click <- sapply(fe_clean$url_info, make_url)
                             # function(x) 
                             #     paste(sprintf('<a href="%1$s">%1$s</a>', x),
                             #           collapse=","))

# WAPO cleaning -------------------------------------------------

wapo_clean <- wapo %>%
    rename(wapoID = id,
           st = state,
           wapo_cod = manner_of_death,
           wapo_armed = armed,
           foreknowledge = signs_of_mental_illness,
           wapo_threat = threat_level,
           wapo_flee = flee,
           wapo_bcam = body_camera) %>%
    mutate(name = case_when(wapoID == 4967 ~ "Collin Osborn", # spot fixes
                            wapoID == 5802 ~ "River Hudson",
                            wapoID == 5816 ~ "Terry Caver",
                            wapoID == 6024 ~ "Juan Rene Hummel",
                            name == "" ~ "Unknown",
                            TRUE ~ name),
           name = str_remove(name, " Jr."),
           name = str_remove(name, " Sr."),
           name = str_replace_all(name, "-"," "),
           fname = "NA",
           lname = "NA",
           gender = recode(gender, 
                           "M"="Male", "F"="Female"),
           race = recode(race,
                         "A" = "API",
                         "B" = "BAA",
                         "H" = "HL",
                         "N" = "NA",
                         "W" = "WEA",
                         .default = "Unknown"),
           race = fct_relevel(race, "Unknown", 
                             after = Inf),
           wapo_armed = recode(wapo_armed,
                               "unknown weapon" = "Unknown"),
           state = state_translate(st),
           date = lubridate::ymd(date),
           date = case_when(name == "Shaun Lee Fuhr" ~ as.Date("2020-04-29"),
                            TRUE ~ date),
           month = lubridate::month(date, label=T),
           day = lubridate::day(date),
           year = lubridate::year(date),
           cod = "Gunshot")


# Final prep -------------------------------------------------

# known fixes
source(here("DataCleaningScripts", "fixes.R"))

#FE name processing
name.list <- str_split(fe_clean$name, " ")
for(i in 1:length(name.list)) {
    fe_clean$fname[i] <- name.list[[i]][1]
    fe_clean$lname[i] <- name.list[[i]][length(name.list[[i]])]
}

#WaPo name processing
name.list <- str_split(wapo_clean$name, " ")
for(i in 1:length(name.list)) {
    wapo_clean$fname[i] <- name.list[[i]][1]
    wapo_clean$lname[i] <- name.list[[i]][length(name.list[[i]])]
}


# Filter WA from 2015
fe_2015 <- fe_clean %>% 
    filter(date > "2014-12-31" & st == "WA")
wapo_2015 <- wapo_clean %>% filter(st == "WA")

# Merge (we only do this for WA state, to make cleaning feasible) ---------

mergefull <- stringdist_full_join(fe_2015, wapo_2015,
                                  by = c("lname", "fname", "date", "gender", "cod"),
                                  max_dist = 2)

## Check for unmatched WAPO records and update FE fields with WAPO info ----
## missing 6618 (temporary, recent)

### START TEMPORARY FIXES #######################################

aaa <- mergefull %>% filter(is.na(feID)) %>% select("wapoID", "fname.y", "lname.y")
if (dim(aaa)[1] > 0) {
    print("Unmatched WAPO records:")
    print(aaa)
    
    # For these cases, assign WAPO info to FE fields
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

# temp fix for Rebischke
target <- which(mergefull$wapoID==7345 & mergefull$feID==99999)
if(length(target > 0)) {
    print("Fixing Rebischke case")
    mergefull$city.x[target] <- "North Bend"
    mergefull$agency[target] <- "North Bend Police Department"
    mergefull$county[target] <- "King"
    mergefull$url_info[target] <- "https://www.seattletimes.com/seattle-news/law-justice/officer-who-fatally-shot-man-at-north-bend-park-is-identified/"
    mergefull$url_click[target] <- make_url(mergefull$url_info[target])
} else {
    print("Rebischke fix not needed anymore")
}

# temp fix for Karuo
target <- which(mergefull$wapoID==7203 & mergefull$feID==99999)
if(length(target > 0)) {
    print("Fixing Karuo case")
    mergefull$agency[target] <- "Clark Co Sheriff's Office"
    mergefull$county[target] <- "Clark"
    mergefull$url_info[target] <- "https://www.opb.org/article/2021/10/19/clark-county-sheriffs-deputies-shooting-kfin-karuo-death/"
    mergefull$url_click[target] <- make_url(mergefull$url_info[target])
    } else {
    print("Karuo fix not needed anymore")
    }

# temp fix for unk Renton shooting
target <- which(mergefull$wapoID==7341 & mergefull$feID==99999)
if(length(target > 0)) {
    print("Fixing UNK Renton case")
    mergefull$agency[target] <- "Renton Police Department"
    mergefull$county[target] <- "King"
    mergefull$url_info[target] <- "https://www.seattletimes.com/seattle-news/law-justice/renton-police-shoot-person-who-allegedly-charged-at-them-with-a-gun/"
    mergefull$url_click[target] <- make_url(mergefull$url_info[target])
} else {
    print("UNK Renton fix not needed anymore")
}

### END TEMPORARY FIXES #######################################

# Checks ----

# Check for duplicate feIDs
aaa <- table(mergefull$feID)
names(aaa[aaa==2])

# Check for duplicate wapoIDs
aaa <- table(mergefull$wapoID)
names(aaa[aaa==2])
 
# Check for city/date mismatch
mergefull %>%
    filter(city.y != city.x & date.x != date.y) %>%
    select(feID, wapoID, city.x, city.y, date.x, date.y)
 
## Spot fix and check
# 
## indices for wapo vars in mergefull
indices <- data.frame(colnames(mergefull))
start <- as.numeric(row.names(indices)[indices=="wapoID"])
end <- as.numeric(row.names(indices)[indices=="year.y"])
len <- end-start+1

# Fix incorrect matches
mergefull[mergefull$feID==25615,start:end] <- rep(NA, len) # fixes wapoID 4568 (may not be a fatality, see below)
# mergefull[mergefull$feID==28777,start:end] <- rep(NA, len) # fixes wapoID 5387 (not needed now)
# 
# fix for wapoID 5242
# transfer from feID 26695 to 27067
mergefull[mergefull$feID==27067,start:end] <- mergefull[mergefull$feID==26695,start:end]
mergefull[mergefull$feID==27067,]$wapoID <- 5242
mergefull[mergefull$feID==26695,start:end] <- rep(NA, len)
mergefull[mergefull$feID==26695,]$wapoID <- NA
# 
# aaa <- table(mergefull$wapoID)
# names(aaa[aaa==2])

# Check for city/date mismatch
mergefull %>%
    filter(city.y != city.x & date.x != date.y) %>%
    select(feID, wapoID, city.x, city.y, date.x, date.y)

# Re-check for age mismatch
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

# spotfix WA homicide designations. 
## we don't do this b/c 
## we haven't looked at all cases, all years, so don't want to make
## one anomalous fix
# mutate(homicide = ifelse(feID == 28698 | #deputy crash while having stroke
#                              feID == 25804 | #next 2 killed by suspect, not police
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

# Write out clean unmerged datasets as csv files ----

write.csv(fe_clean, here("data-outputs", "FE_clean.csv"))
write.csv(wapo_clean, here("data-outputs", "WaPo_clean.csv"))

# Save clean datasets as Rdata files ----

scrape.date <- Sys.Date()
last.fe.date <- max(fe_clean$date)
last.wapo.date <- max(wapo_clean$date)

## All cases ----
save(list = c("fe_clean", "wapo_clean", 
              "scrape.date", "last.fe.date", "last.wapo.date"),
     file = here("data-outputs", "CleanData.rda"))

##  WA after 2015 ----
fe_2015 <- fe_clean %>% filter(date > "2014-12-31")
wapo_2015 <- wapo_clean
finalmerge_2015 <- finalmerge

save(list = c("fe_2015", "wapo_2015", "finalmerge_2015", 
              "scrape.date", "last.fe.date", "last.wapo.date"),
     file = here("data-outputs", "WA2015.rda"))

##  WA post 940 ----
fe_940 <- fe_clean %>% filter(date > "2018-12-06")
wapo_940 <- wapo_clean %>% filter(date > "2018-12-06")
finalmerge_940 <- finalmerge %>% filter(date > "2018-12-06")

save(list = c("fe_940", "wapo_940", "finalmerge_940", 
              "scrape.date", "last.fe.date", "last.wapo.date"),
     file = here("data-outputs", "WA940.rda"))



#load(file = here("data-outputs", "WA940.rda"))

# state_fips_df <- "https://en.wikipedia.org/wiki/" %>%
#     str_c("Federal_Information_Processing_Standard_state_code") %>%
#     GET() %>%
#     content("text") %>%
#     {readHTMLTable(doc=.)} %>%
#     .[[1]] %>%
#     as_tibble() %>%
#     filter(V1 != "Name") %>%
#     select(state_abb = V2, GEOID = V3) %>%
#     mutate(state_abb = as.character(state_abb)) %>%
#     mutate(State = state_translate(state_abb)) %>%
#     filter(!is.na(State) & as.numeric(GEOID) <= 56)
# 
# county_fips_df <- county.fips %>%
#     as_tibble() %>%
#     mutate(GEOID = sprintf("%05d", fips)) %>%
#     mutate(State = str_to_title(str_split_fixed(polyname, ",", 2)[,1])) %>%
#     mutate(County = str_to_title(str_split_fixed(polyname, ",", 2)[,2])) %>%
#     select(GEOID, State, County) %>%
#     left_join(select(state_fips_df, -GEOID), by = c("State"))
# 
# use_data(fe_df, fe_df_clean, state_fips_df, county_fips_df, overwrite = TRUE)
