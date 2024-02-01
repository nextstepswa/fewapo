# All files named _raw when read in
# Modified files are named _temp for output to fixes_precleaning

# Fatal Encounters (1/1/2000-12/31/2021) ----
## Data are no longer updated, so only need to download once

if(file.exists(here::here("Data", "Raw", "fe_raw.csv"))) {
  
  message("Reading in existing FE download")
  
  fe_raw <- rio::import(here::here("Data", "Raw", "fe_raw.csv")) %>%
    select(-c(V1, V33:`Unique identifier (redundant)`))
  
} else {
  
  message("Downloading FE")
  
  doc_id = "1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE"
  url_template = 'https://docs.google.com/spreadsheets/d/DOC_ID/export?format=tsv'
  fe_url = sub("(*.)DOC_ID(*.)",
               paste("\\1", doc_id, "\\2", sep=""),
               url_template)
  fe_save_file = here::here("Data", "Raw", "fe_raw.csv")

  fe_raw <- scrape_data_fn(fe_raw, fe_url, fe_save_file)
}

# New names (1/1/2022-):  WA ONLY ----
## Preserves the original coverage of FE: 
## fatalities from all encounters, not just gunshots.  
## Vehicular pursuits are included.

## Modify age and date to match to orig FE below

message("Reading in new names for WA")

fe_newnames <- readxl::read_xlsx(here::here("Data", "Raw", "fe_newnames.xlsx")) %>%
  mutate(date = as.Date(`Date of injury resulting in death (month/day/year)`)) %>%
  rename("age" = "Age") %>%
  select(-`Date of injury resulting in death (month/day/year)`)

# Clean up a couple of things so the post 2021 names can be added, 
# date is persnickety, and the name needs to be changed for fixes later
# Latitude reads as character bc there is stray comma for fe ID 28891
# Age is a mess bc it has some ranges, and case_when doesn't like "-"
# Add newnames

fe_temp <- fe_raw[!is.na(fe_raw$`Unique ID`),] %>% # delete the end row of text
  mutate(date = mdy(`Date of injury resulting in death (month/day/year)`),
         Latitude = as.numeric(sub(",", "", Latitude)),
         Age = gsub("-","", Age),
         age = as.numeric(
           case_when(Age == "1825" ~ "22",
                     Age == "55." ~ "55",
                     TRUE ~ Age))
  ) %>%
  select(-c(Age, `Date of injury resulting in death (month/day/year)`)) %>%
  bind_rows(fe_newnames) %>%
  rename(feID = `Unique ID`)

#######################################################################################

# MPV ----

url <- "https://mappingpoliceviolence.us/s/MPVDatasetDownload.xlsx"
destfile <- here::here("Data", "Raw", "MPVDatasetDownload.xlsx")
curl::curl_download(url, destfile)

# Note cols don't read in properly if too many initial rows are missing --
# To fix: use guessmax arg 
# https://github.com/tidyverse/readxl/issues/413#issuecomment-350520309

message("Scraping MPV")

mpv_raw <- read_excel(destfile, guess_max = 10000)

# Also need to modify the coltypes for age & zip since these
# don't get identified correctly by default.  But we do in the cleaning loop;
# will generate warnings b/c age has 'Unknown' for missing

# To facilitate scripted cleaning
# Check periodically to see if they've modified the file structure
mpv_temp <- mpv_raw %>%  
  mutate(mpvID = `MPV ID`, 
         feID = as.numeric(`Fatal Encounters ID`),
         wapoID = `WaPo ID (If included in WaPo database)`,
         name = `Victim's name`,
         date = as.Date(`Date of Incident (month/day/year)`)
  ) %>%
  select(mpvID:date, `Victim's age`:`Prosecutor Source Link`) #omits KBP, col of 1's

#######################################################################################

# Washington Post ----
## Has been updated for the V2 structure, so this doesn't have the wapo_raw file
## Agency info merged in here

message("Scraping WaPo")

wapo_cases_url = "https://github.com/washingtonpost/data-police-shootings/raw/master/v2/fatal-police-shootings-data.csv"
wapo_cases_save_file = here::here("Data", "Raw", "wapo_raw.csv")
wapo_cases <- scrape_data_fn(wapo, wapo_cases_url, wapo_cases_save_file)

wapo_agency_url = "https://github.com/washingtonpost/data-police-shootings/raw/master/v2/fatal-police-shootings-agencies.csv"
wapo_agency_save_file = here::here("Data", "Raw", "wapo_agency_names.csv")
wapo_agency <- scrape_data_fn(wapo, wapo_agency_url, wapo_agency_save_file) %>%
  rename("agency_ids" = "id", "agency_name" = "name")


## Transform agency ids to matrix (may be multiple agencies) and match agency names
## Matrix ncols hardcoded at 5, which is max agencies listed for any case by inspection
## Create "agency" variable with all names (to match FE) after matching

wapo_agency_xwalk <- wapo_agency %>% 
  select(agencyid = agency_ids, agency_name, agency_type = type)
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
wapo_temp <- left_join(wapo_cases, wapo_agencyXcase) %>%
  rename("wapoID" = "id")

# Leg District info ----

message("Loading prebuilt WA legislative district data")
load(here::here("Data", "Clean", "LegInfo_Clean.rda"))