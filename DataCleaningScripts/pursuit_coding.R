#############################################################################################

# Final Pursuit coding WA, 2015+ only ----
# this file is sourced by ScrapeMerge.R

# input df: finalmerge
# output files: csv files with cases from finalmerge to review; WTSC RDA file
# return df: coded.pursuits, with WTSC matched case indicators, for merging onto data

#############################################################################################

# Outline of pursuit and vehicle fatality coding process: ----

##  A. In FE cleaning loop of ScrapeMerge.R:
##     use cleaned "circumstances variable" to create vpursuit.draft (DBB original coding and 2022+)

##  B. In this file:
##     1. create pursuit.tag -- uses all info from both FE and WaPo to tag possible pursuit cases
##        a. this was initially used to triage and review !is.na(pursuit.tag) cases for vpursuit coding.
##        b. but as of Feb 14/2023 *all* cases from 2015+ have been reviewed and coded
##           so it is left here for historical interest
##
##     2. check if any new cases need to be reviewed for vpursuit/victim/injury/incident.num coding
##
##     3. Review/code/merge in WTSC pursuit fatality data
##          Data are released annually in May of the following year
##          Request for new data needs to be made here: https://wtsc.wa.gov/request-fatal-crash-data/

## Notes:
## A complete independent review of active pursuit cases was completed by SC on 2/1/2023
## A complete review of all WA cases from 2015 forward was completed Feb ? 2023

# Final vpursuit codes are:

## Active pursuit - fatality occurred during pursuit
## Terminated pursuit - active pursuit, crash/fatality happened shortly after pursuit terminated 
## Involved pursuit - death occurred post pursuit, cod not vehicle (often shot)
## Attempted stop - lights/siren activated, subject fled, not pursued, crashed
## Vehicle accident - non-pursuit, non-fleeing incidents; on duty officer accident
## Reviewed not related - case was reviewed, no vehicle/pursuit relevance
## NA - case not reviewed or coded

# Source variable: ----

# Original FE variable: Intended use of force (Developing) ----
# 2022+ cases use new coding for this field when relevant

# > fe %>% group_by(`Intended use of force (Developing)`) %>% count()
# A tibble: 15 x 2
# Groups:   Intended use of force (Developing) [15]
# `Intended use of force (Developing)`     n
# <chr>                                <int>
# 1 ""                                       2
# 2 "Active pursuit"                         4 # 2022+
# 3 "Attempted stop"                         3 # 2022+
# 4 "Deadly force"                       18924
# 5 "Involved pursuit"                      13 # 2022+
# 6 "Less-than-lethal force"              1766
# 7 "No"                                   883
# 8 "Nonlethal force"                        1
# 9 "Other"                                  1
# 10 "Pursuit"                             2397
# 11 "Suicide"                             3350
# 12 "Undetermined"                          48
# 13 "Vehic/Purs"                          3765
# 14 "Vehicle"                              393
# 15 "Vehicle accident"                       1 # 2022+

# from DBB's email about the "Intended use of force" coding

# Vehicle is the clean non-pursuit category (e.g., accidents not related to pursuits)
# Pursuit is the clean pursuit category
# Veh/Purs is the unclean mix category that he is slowly working through
## inspection suggests he hasn't cleaned WA prior to 2016

# Cleaned variable: "circumstances" (renamed and relabeled)  ----

# > fe_clean %>% group_by(circumstances) %>% count()
# A tibble: 11 x 2
# Groups:   circumstances [11]
# circumstances                    n
# <fct>                        <int>
# 1 Active pursuit                2401
# 2 Attempted stop                   3
# 3 Deadly force                 18923
# 4 Involved pursuit                13
# 5 Less lethal force             1768
# 6 Mix of pursuit-related cases  3765
# 7 Other                            1
# 8 Suicide                       3350
# 9 Unintended                     883
# 10 Vehicle accident               394
# 11 Unknown                         49

# Recoded variable:  vpursuit.draft (WA 2015+ cases only), starting place for pursuit tagging  ----

# > fe_clean %>% group_by(vpursuit.draft) %>% count()
# vpursuit.draft                   n
# <chr>                        <int>
# 1 Active pursuit                2401
# 2 Attempted stop                   3
# 3 Involved pursuit                13
# 4 Mix of pursuit-related cases  3765
# 5 Vehicle accident               394
# 6 NA                           24974

######################################################################################

# 1. pursuit.tag: automated selection of possible pursuit cases for WA 2015+ ----
#    no longer used, but left for comparison here

all.pursuit.tags <- finalmerge %>%
  mutate(pursuit.tag = case_when(
    !is.na(vpursuit.draft) ~ 1, # All DBB pursuit related codes and 2022+ codes
    grepl('vehicle|car|crash|speed|chase|pursuit|flee|fled', description) |
      grepl('car|Car', flee.wapo) ~ 2, # any other indication of vehicle or pursuit
    cod == "Vehicle" ~ 3 # this doesn't seem to pick up any additional, but may in the future
  )) %>%
  filter(!is.na(pursuit.tag)) %>%
  select(feID, name, date, homicide, suicide, pursuit.tag,  vpursuit.draft, cod, description, url_info)
write.csv(all.pursuit.tags, here::here("data-outputs", "tagged-pursuits.csv"))

cat(paste("\n\n *** ", nrow(all.pursuit.tags), "possible pursuit cases tagged ****\n\n"))


# 2. Review/update new cases as needed ----

# Check if we have something new.  
# If yes, stop to review before going on; 
#   this will be an iterative process, rerun ScrapeMerge after new cases are coded
# If no, we return the coded pursuit data for WTSC merge

#old.file.location <- 'read_xlsx(here::here("data-outputs", "pursuits-coded.xlsx"), sheet = "Coded")'

# google sheet location
coded.pursuit.file.location <- "https://docs.google.com/spreadsheets/d/1De0ih8yfbnHVX8iNMa_OGcn03xHrjRYr2m2ql-xSTho/edit?usp=sharing"
googlesheets4::gs4_deauth()
raw.coded.pursuits <- googlesheets4::read_sheet(coded.pursuit.file.location, sheet = "coded_data")


# Read in existing coded results 

coded.pursuits <-  raw.coded.pursuits %>%
  
  # use final values for all variables post-review
  mutate(cod = if_else(!is.na(cod.final), cod.final, cod),
         victim = if_else(!is.na(victim.final), victim.final, victim),
         description = if_else(!is.na(description.final), description.final, description),
         url_info = if_else(!is.na(url_additional), url_additional, url_info)
  ) %>%
  select(feID, name, date, vpursuit = vpursuit.final, cod, victim, injury, description, url_info, incident.num,
         pursuit.notes = notes)

# Anti-join old and new cases to identify any discrepancies
# NB: new cases can be earlier or most recent, so we don't use dates to filter

old.coded <- coded.pursuits %>%
  filter(!is.na(vpursuit)) %>%
  select(feID, date, vpursuit)

all.cases <- finalmerge %>%
  select(feID, date, homicide, suicide, vpursuit.draft, cod, description, url_info)

case_check_new <- anti_join(all.cases, old.coded, by = "feID")

# If new cases found, output file for review, otherwise continue

if(nrow(case_check_new) != 0){ # pursuit updates are needed
  
  # Output new cases for review and add to pursuits-coded.xlsx as appropriate
  case_check_new %>% 
    write.csv(., here::here("data-outputs", "new-pursuits-review.csv"))
  
  # Stop until new cases are reviewed
  stop("Stopping:  There are new pursuits to review")
  
} # no updates needed for pursuits

pursuit.coding.message <- "\n\n *** No new pursuit coding required *** \n\n"
#cat("\n\n *** Don't forget to uncomment the stop after review is finalized *** \n\n")


## 3. WTSC data: only updated annually in May ----

if(!file.exists(here::here("data-outputs", "WTSC.rda"))) { # Start from scratch
  
  # Read in WTSC data
  
  WTSC <- readxl::read_excel(here::here("data-raw", 
                                        "WTSC_Pursuit_Fatalities/WTSC_pursuit fatalities_MR_2-9-2023.xlsx")) %>%
    select(feID, agency.wtsc=repag_long, year:co_char, city.num=city, 
           numfatal:vforms, long=x, lat=y, vnumber, pnumber:race,
           race_me, race1:race5,
           injury, diedscene,
           unrest_fat:bike_fat, urbrur, CoRoadName, st_rd:oth_rd,
           drf1:drf4, crf1:crf3, possID = `possible match`) %>%
    rename(date.wtsc = crash_dt,
           county.num = county,
           county.wtsc = co_char,
           age.wtsc = age,
           race.wtsc = race,
           race.single.wtsc = race_me
    ) %>%
    mutate(injury.type = ifelse(injury==4, "fatal", "non-fatal"),
           when.died = case_when(
             diedscene==0 ~ "post-crash", 
             diedscene==7 ~ "on-scene",
             diedscene==8 ~ "en-route",
             diedscene==9 ~ "unknown"),
           road.type = case_when(
             st_rd==1 ~ "State route",
             cty_rd==1 ~ "County road",
             city_rd==1 ~ "City road",
             oth_rd==1 ~ "Other"),
           victim.wtsc = case_when(
             ptype==1 ~ "driver",
             ptype==2 ~ "passenger",
             ptype %in% c(88, 99) ~ "unknown",
             TRUE ~ "other")
    ) %>%
    mutate(driver.pursued = ifelse(if_any(contains("drf"), function(x){x==37}), 
                                   "yes", "no"),
           vehicle.pursued = ifelse(if_any(contains("crf"), function(x){x==20}), 
                                    "yes", "no")) %>%
    select(-c("diedscene", contains("rd"), contains("drf"), contains("crf"))) %>%
    mutate(wtsc=1)
  
  last.wtsc.update <- lubridate::year(max(WTSC$date.wtsc))
  
  save(list = c("last.wtsc.update", "WTSC"),
       file = here::here("data-outputs", "WTSC.rda"))
  
} else { # read in WTSC data
  
  load(here::here("data-outputs", "WTSC.rda"))
  
} # WSTC data read in

# Should we check for a WTSC update (in May)?

wtsc.update.message <- NULL
if(lubridate::month(Sys.Date()) == 5 & 
   last.wtsc.update < lubridate::year(Sys.Date())) {
  wtsc.message <- "\n\n *** CHECK FOR WTSC ANNUAL UPDATE **** \n\n\n"
  
} 

# Here we only tag whether an FE case matches a case in WSTC

coded.pursuits <- left_join(coded.pursuits, WTSC %>% select(feID, wtsc), 
                            by="feID")
