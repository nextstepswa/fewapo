# FE cleaning script ----

## input:  fe_temp (from fixes_preclean_fe.R)
## interim: fe_draft
## output: fe_clean

fe_draft <- fe_temp %>%

  mutate(name = Name,
         name = case_when(
           name == "Name withheld by police" ~ "Unknown",
           is.na(name) ~ "Unknown",
           TRUE ~ name),
         name = str_remove(name, " Jr."),
         name = str_remove(name, " Jr$"),
         name = str_remove(name, " Sr."),
         name = str_remove(name, " III"),
         name = str_remove(name, " II"),
         name = str_remove(name, " IV"),
         name = str_remove(name, " V$"),
         
         # preserve aka names as "name2"
         name2 = if_else(grepl(" aka ", name), str_remove(name, ".* aka "), ""),
         # and remove the aka from the primary name
         name = str_remove(name, " aka.*"),
         
         name = str_replace_all(name, "-"," "),
         name = str_remove(name, "\\."),
         fname = "NA", # prep for assignment
         lname = "NA") %>%
  mutate(
    raceOrig = case_when(
      is.na(Race) | Race == "Race unspecified" | Race == "" ~ "Unknown",
      grepl("Asian", Race) ~ "API",
      grepl("Black", Race) ~ "BAA",
      grepl("Latino", Race) ~ "HL",
      grepl("Middle Eastern", Race) ~ "ME",
      grepl("Native", Race) ~ "NAA",
      grepl("White", Race) ~ "WEA",
      TRUE ~ Race),
    raceOrig = fct_relevel(raceOrig, "Unknown", after = Inf)
  ) %>%
  
  # We will use race+imputations for the FE race variable
  # Also a consistent set of categories with WaPo
  #  so Middle Eastern -> Other
  mutate(race = `Race with imputations`,
         race = case_when(
           is.na(race) | race == "Race unspecified" | race == "" ~ "Unknown",
           grepl("Asian", race) ~ "API",
           grepl("Black", race) ~ "BAA",
           grepl("Latino", race) ~ "HL",
           #grepl("Middle Eastern", race) ~ "ME",
           grepl("Native", race) ~ "NAA",
           grepl("White", race) ~ "WEA",
           TRUE ~ "Other"),
         
         # a handful of imputations are NA when the orig is known, so fix these
         race = if_else(race == "Unknown" & raceOrig != "Unknown", raceOrig, race),
         
         race = fct_relevel(race, c("Other", "Unknown"), 
                            after = Inf)  
  ) %>%
  mutate(gender = str_to_sentence(Gender),
         gender = case_when(
           is.na(gender) | gender == "" ~ "Unknown",
           grepl("Trans", gender) ~ "Other",
           TRUE ~ gender),
         gender = fct_relevel(`gender`, "Unknown", after = Inf)
  ) %>%
  # age was pre-fixed above, but needs missing value, since age is used to merge
  mutate(age = ifelse(is.na(age), 999, age) 
  ) %>%
  mutate(month = month(date, label=T),
         day = day(date),
         year = year(date)
  ) %>%
  mutate(city = `Location of death (city)`,
         st = State,
         state = state_fullname_fn(st),
         state.num = match(st, st.abb51),
         zip = `Location of death (zip code)`,
         county = `Location of death (county)`
  ) %>%
  
  # For agency(s) we don't currently split up the multiples for FE like we do for WaPo
  # fix double spaces for splitting in other analyses
  mutate(agency = `Agency or agencies involved`,
         agency = gsub("  ", " ", agency)
  ) %>%
  # agency.type classifies only for single agency incidents
  # otherwise it is coded "Multiple agencies"
  mutate(
    agency.type = case_when(
      agency=="" ~ "Unknown",
      grepl(",", agency) ~ "Multiple agencies",
      grepl("Campus|University|College|School", agency) ~ "Campus Police",
      grepl("State|Patrol", agency) ~ "State Police",
      grepl("Sheriff|Sherrif|sheriff", agency) ~ "County Sheriff",
      grepl("Tribe|Tribal|Nation[^a]", agency) ~ "Tribal Police",
      grepl("Police|police|Public Safety|Town|City", agency) ~ "Local Police",
      grepl("Tombstone|Corpus Christi|White Settlement|Charlotte-Mecklenburg|Patagonia", agency) ~ "Local Police",
      grepl("Correction", agency) ~ "Corrections Dept",
      #grepl("Alcohol Tobacco Firearms", agency) ~ "US ATF", too few
      grepl("U.S.*Border.", agency) ~ "US CBP",
      grepl("U.S. Federal Bureau", agency) ~ "US FBI",
      #grepl("U.S. Immigration", agency) ~ "US ICE", too few
      grepl("U.S. Marshal", agency) ~ "US Marshals",
      grepl("Port|National Guard|Metroparks", agency) ~ "Other",
      grepl("U.S.|National|Federal|Pentagon", agency) ~ "Other Federal",
      grepl("County|Harris Constable", agency) ~ "Other County",
      TRUE ~ "Other State"),
    agency.type = factor(
      agency.type, 
      levels = c("Local Police", "County Sheriff", "State Police", "Tribal Police",
                 "Campus Police", "Corrections Dept", 
                 "US CBP", "US FBI", "US Marshals", 
                 "Other Federal", "Other State", "Other County", "Other",
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
           grepl("Gun", weapon)  ~ "Alleged firearm",
           grepl("Rifle", weapon)  ~ "Alleged firearm",
           weapon == "None" ~ "No weapon",
           weapon == "" | is.na(weapon) ~ "Unknown",
           TRUE ~ "Other"),
         weapon = fct_relevel(weapon, "Unknown", 
                              after = Inf)
  ) %>%
  # Flee variable is a mess, need consistent categories for WaPo
  # Some hierarchical coding of multiples here, with Vehicle > Other > Foot
  mutate(flee = `Fleeing/Not fleeing`,
         flee = case_when(
           is.na(flee) | flee == "" ~ "Unknown",
           grepl("Uncertain", flee) ~ "Unknown",
           grepl("Not|None", flee) ~ "Not fleeing",
           grepl("Veh|veh", flee) ~ "Vehicle",
           grepl("foot|Foot", flee) ~ "Foot",
           TRUE ~ "Other"),
         flee = factor(flee, 
                       levels = c("Not fleeing", "Vehicle", "Foot", "Other", "Unknown"))
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
  
  # homicide:  Excludes suicides and killed by subject (when not a pursuit vehicular homicide)
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
  
  mutate(medical = if_else(grepl("Medical", cod), 1, 0)) %>%
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
  # Latitude was pre-fixed above
  mutate(latitude = Latitude,
         longitude = Longitude
  ) %>%
  mutate(url_info = `Supporting document link`,
         description = `Brief description`
  ) %>%
  
  # New variable:  not.kbp
  # When civilian/subject, not police, kills this person in the context of a police encounter
  # We only have this info for WA for now
  
  # DOES NOT APPLY TO PURSUITS: vehicular homicides/homicides by fleeing subject id'd by pursuit variables
  #    These fatalities are part of the collateral damage of pursuits that we seek to count
  #    So not.kbp is set to 0 for these cases
  
  # Only 2 WA cases in FE are not kbp
  # feID 25805 Robert Hassan shot by suspect in Lake City; we leave the pursuit related vehicle crash 25804
  # feID 18751 Pamela Parker shot by subject, who then shot himself; we leave the suicide 18750
  
  mutate(not.kbp = ifelse(feID %in% c(18751, 25805), 1, 0))

### create clickable url for Rpubs reports
fe_draft$url_click <- sapply(fe_draft$url_info, make_url_fn)

# Select variables
## note that officer information is currently only available for WA for 2022
## until MPV merge issues are sorted out

fe_clean <- fe_draft %>%
  select(
    feID, name, name2, fname, lname,
    date, month, day, year,
    city, county, st, state, state.num, zip, 
    latitude, longitude,
    raceOrig, race, gender, age,
    cod, armed, weapon, flee,
    circumstances, homicide, suicide, not.kbp, medical, foreknowledge,
    vpursuit.draft,
    agency, agency.type,
    description, url_info, url_click, officer_names, officer_url
  )

