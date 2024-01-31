# WaPo cleaning script ----

## input:  wapo_temp (from fixes_preclean_wapo.R)
## interim: wapo_draft
## output: wapo_clean

# 8416 duplicate case? may be Derrick Ameer Cook (info in 8617)
# reported on the WaPo github repo issue #53 appears to be deleted now

wapo_draft <- wapo_temp %>%
  rename(raceOrig = race,
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
         
         #name = str_remove(name, " aka.*"), # no aka names in wapo
         
         name = str_replace_all(name, "-"," "),
         fname = "NA",
         lname = "NA"
  ) %>%
  mutate(age = ifelse(is.na(age), 999, age) # missing value
  ) %>% 
  mutate(gender = str_to_sentence(gender),
         gender = case_when(
           gender == "" ~ "Unknown",
           grepl("Non|Trans", gender) ~ "Other",
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

# Final dataset ----

## Select common vars, and some unique vars

## Remove a problem case
## WaPo 4568
## This is the only WAPO case not found in FE.
## Not clear if the victim died, name unknown, can't find more info online.  
## https://www.kiro7.com/news/local/police-investigating-officer-involved-shooting-in-federal-way/930686522/
## https://komonews.com/news/local/federal-way-police-investigating-officer-involved-shooting

wapo_clean <- wapo_draft %>%
  mutate(cod = "Gunshot") %>% # to facilitate merge
  filter(wapoID != 4568) %>% # problem case
  select(
    wapoID, name, fname, lname,
    date, month, day, year,
    city, county, st, state, state.num, 
    latitude, longitude,
    raceOrig, race, race_source, gender, age,
    cod, armed, weapon, flee,
    threat, bodycam,
    mental_illness,
    agency, agency.type) 
