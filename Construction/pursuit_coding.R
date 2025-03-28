#############################################################################################

# Final Pursuit coding WA, 2015+ only ----
# this file is sourced by ScrapeMerge.R

# input df: finalmerge
# output files: csv files with cases from finalmerge to review; WTSC RDA file
# return df: coded.pursuits, with WTSC matched case indicators, for merging onto data

# NOTE: When FE stopped updating (12/31/2021) this type of pursuit data was no longer 
# being recorded by any group.  We have continued updating pursuit related fatality incidents
# for WA only, using FE google search methods and other notifications

#############################################################################################

# Outline of pursuit and vehicle fatality coding process for Fatal Encounters data (thru 2021): ----

##  A. In FE cleaning loop of ScrapeMerge.R:
##     use cleaned "circumstances variable" to create vpursuit.draft (DBB original coding and 2022+)

##  B. In this file:
##     1. create pursuit.tag -- uses all info from both FE and WaPo to tag possible pursuit cases
##        a. this was initially used to triage and review !is.na(pursuit.tag) cases for vpursuit coding.
##        b. but as of Feb 14/2023 *all* cases from 2015+ are reviewed and coded
##           so it is left here for historical interest
##
##     2. check if any new active pursuit cases need to be reviewed and coded for vpursuit/victim/injury/incident.num
##
##     3. Review/code/merge in WTSC pursuit fatality data
##          Data are released annually in May of the following year
##          Request for new data needs to be made here: https://wtsc.wa.gov/request-fatal-crash-data/

## Notes:
## A complete independent review of 
###  active pursuit cases was completed by SC on 2/1/2023
###  all WA cases from 2015 forward was completed by SC on 2/22/2023

# Final vpursuit codes are:

## Active pursuit - fatality occurred during pursuit
## Terminated pursuit - active pursuit, crash/fatality happened shortly after pursuit terminated 
## Involved pursuit - death occurred post pursuit, cod typically not vehicle (often shot)
## Attempted stop - lights/siren activated, subject fled, not pursued or pursuit terminated well before accident, crashed
## Vehicle accident - non-pursuit, non-fleeing incidents; on duty officer accident
## Reviewed not related - case was reviewed, no vehicle/pursuit relevance
## NA - case not reviewed or coded

# Source variable: ----

# Original FE variable: Intended use of force (Developing) ----
# 2022+ cases use new coding for this field for WA only when relevant

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

# Recoded variable:  vpursuit.draft (WA 2015+ cases only), starting place for pursuit review  ----

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
#    no longer used, because visual inspection found additional cases
#    but left for comparison here

all.pursuit.tags <- finalmerge %>%
  mutate(pursuit.tag = case_when(
    !is.na(vpursuit.draft) ~ 1, # All DBB pursuit related codes and 2022+ codes
    grepl('vehicle|car|crash|speed|chase|pursuit|flee|fled', description) |
      grepl('Vehicle|Other', flee) ~ 2, # any other indication of vehicle or pursuit
    cod == "Vehicle" ~ 3 # this doesn't seem to pick up any additional, but may in the future
  )) %>%
  filter(!is.na(pursuit.tag)) %>%
  select(feID, name, date, homicide, suicide, not.kbp, pursuit.tag,  vpursuit.draft, cod, description, url_info)
write.csv(all.pursuit.tags, here::here("Data", "Interim", "tagged-pursuits.csv"))

cat(paste("\n\n *** ", nrow(all.pursuit.tags), "possible pursuit cases tagged ****\n\n"))


# 2. Review/update new cases as needed ----

# Check if we have new active/terminated pursuits to review.  
# If yes, stop to review before going on; 
#   this will be an iterative process, rerun ScrapeMerge after new cases are coded
# If no, we construct the coded pursuit data, merge with WTSC tags, and return to ScrapeMerge

#old.file.location <- 'read_xlsx(here::here("Data", "Clean", "pursuits-coded.xlsx"), sheet = "Coded")'

## Read in coded data ----

# google sheet location for pursuit coding
coded.pursuit.file.location <- "https://docs.google.com/spreadsheets/d/1De0ih8yfbnHVX8iNMa_OGcn03xHrjRYr2m2ql-xSTho/edit?usp=sharing"
# drive_get("~/PoliceReform/Pursuits/Data/WA pursuit coding/all.case.pursuit.coding")

# google sheet for testing:
# coded.pursuit.file.location <- "https://docs.google.com/spreadsheets/d/1a7ougZ2ncUn5mOj8mQq8D0hgW9ndn7_dxLBg_Y5oENs/edit?usp=sharing"
# drive_get("~/PoliceReform/Pursuits/Data/WA pursuit coding/copy.all.case.pursuit.coding")

googlesheets4::gs4_deauth()
raw.coded.pursuits <- googlesheets4::read_sheet(coded.pursuit.file.location, 
                                                sheet = "coded_data")

# Anti-join old and new cases to identify any new cases for review
# NB: new cases can be earlier cases we missed or the most recent cases, so we can't use dates to filter
# And we replace NA in vpursuit.draft with a reminder to review

old.coded <- raw.coded.pursuits %>%
  select(feID, date)

all.cases <- finalmerge %>%
  select(feID, name, date, homicide, suicide, vpursuit.draft, cod, description, url_info)

new.cases <- anti_join(all.cases, old.coded, by = "feID") %>% 
  replace_na(list(vpursuit.draft = "Not yet reviewed"))


## Update as appropriate ----

## If new cases since last run:
## output to googlesheet
## stop and request review for active pursuits, 
## update message that review is needed for any other new cases
## otherwise continue

if(nrow(new.cases) != 0){ # pursuit updates are needed

  pursuit.coding.message <- "\n\n *** NEW POSSIBLE PURSUIT CASES TO REVIEW *** \n\n https://docs.google.com/spreadsheets/d/1De0ih8yfbnHVX8iNMa_OGcn03xHrjRYr2m2ql-xSTho/edit?usp=sharing \n\n"

  # create df with new cases and correct col structure
  new_case_df <- bind_rows(raw.coded.pursuits[1,], new.cases)[-1,] 

  # add these to the sheet -- only authorized users can do this
  googlesheets4::gs4_auth(
    email = TRUE, #gargle::gargle_oauth_email(),
    path = NULL,
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token = NULL
  )
  
  googlesheets4::sheet_append(ss=coded.pursuit.file.location, 
                              data = new_case_df, 
                              sheet = "coded_data")
  
  # check for new cases; these need review before continuing
  if(nrow(new_case_df) > 0) {

    # Stop if new cases
    stop(paste("Stopping: ", pursuit.coding.message))
    
  } else {
    
    # Otherwise replace the old raw.coded.pursuits with the updated data
    googlesheets4::gs4_deauth()
    raw.coded.pursuits <- googlesheets4::read_sheet(coded.pursuit.file.location, 
                                                    sheet = "coded_data")
  }
  
} else { # no updates needed for pursuits

pursuit.coding.message <- "\n\n *** No new pursuit coding required *** \n\n"

}
  
  
# Pull final pursuit variables from coded worksheet 

coded.pursuits <-  raw.coded.pursuits %>%
  mutate(vpursuit = if_else(!is.na(vpursuit.final), vpursuit.final, vpursuit.draft),
         cod = if_else(!is.na(cod.final), cod.final, cod),
         victim = if_else(!is.na(victim.final), victim.final, victim),
         description = if_else(!is.na(description.final), description.final, description),
         url_info = if_else(!is.na(url_additional), url_additional, url_info)
  ) %>%
  select(feID, name, date, vpursuit, cod, victim, injury, description, url_info, incident.num,
         pursuit.notes = notes)


## 3. WTSC data: only updated annually in May ----

## This is based on an excel extract of pursuit cases only from WTSC, 
## not the entire WTSC person files.  FE cases were matched by
## hand/inspection.

if(!file.exists(here::here("Data", "Clean", "WTSC.rda"))) { # Start from scratch
  
  # Read in WTSC data
  
  WTSC <- readxl::read_excel(here::here("data-raw", "WTSC_pursuit fatalities_MR_2-9-2023.xlsx")) %>%
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
       file = here::here("Data", "Clean", "WTSC.rda"))
  
} else { # read in WTSC data
  
  load(here::here("Data", "Clean", "WTSC.rda"))
  
} # WSTC data read in

# Should we check for a WTSC update (in May)?

wtsc.update.message <- NULL
if(lubridate::month(Sys.Date()) > 5 & 
   last.wtsc.update < lubridate::year(Sys.Date())) {
  wtsc.message <- "\n\n *** CHECK FOR WTSC ANNUAL UPDATE **** \n\n\n"
  
} 

# Here we only tag whether an FE case matches a case in WSTC

coded.pursuits <- left_join(coded.pursuits, WTSC %>% select(feID, wtsc), 
                            by="feID")

