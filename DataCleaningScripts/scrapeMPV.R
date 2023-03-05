#############################################################################

# There are 2 competing datasets from MPV:

#   * https://mappingpoliceviolence.us/  
#     "The Official Mapping Police Violence Database"
#     stored as xlsx, reasonably current

#   * https://mappingpoliceviolence.org/ 
#     from Campaign Zero
#     stored as an "airtable", delayed updates

#############################################################################


library(tidyverse)
library(kableExtra)
library(lubridate)

state_translate_fn <- function(x){
  snames <- c(state.name, "District of Columbia")
  names(snames) <- c(state.abb, "DC")
  snames[x]
}

make_url_fn <- function(x) {
  paste(sprintf('<a href="%1$s">%1$s</a>', x),
        collapse=",")
}

num <- 1:51
states <- c(sort(state.abb), "DC")

# Download, then read in ----

library(readxl)
url <- "https://mappingpoliceviolence.us/s/MPVDatasetDownload.xlsx"
destfile <- "Downloads/MPVDatasetDownload.xlsx"
curl::curl_download(url, destfile)

# we specify coltypes to make sure cols with missing data in first
# row are read correctly.
# also modify the coltypes for age, date, zip since these
# don't get identified correctly by default

# note this generates warnings for age b/c it has 'Unknown' for missing

# Bit of a PITA and may need to be updated if 
# cols change in MPV.  
# Read in with point and click, Use lapply(data, class)
# to get names/classes, futz in word and excel to get the list


MPV1_raw <- read_excel(
  destfile,
  col_types = c("text", "numeric", "text", "text", "text", 
  "date" , "text", "text", "text", 
  "numeric", "text", "text", "text", "text", 
  "text", "text", "text", "text", "text", 
  "text", "text", "text", "text", "text", 
  "text", "numeric", "text", "text", "numeric", "numeric", 
  "text", "text", "text", "text", "text", 
  "text", "numeric", "text", "text", "numeric", "numeric", 
  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
  "numeric", "numeric", "text", "text", "text", "text", 
  "text", "text", "text", "text", "numeric", "text", 
  "text", "text", "text", "numeric")
)


# MPV1 cleaning ----

MPV1_clean <- MPV1_raw %>%
  
  mutate(
    mpvID = `MPV ID`,
    feID = `Fatal Encounters ID`,
    wapoID = `WaPo ID (If included in WaPo database)`,
    kbp = `Killed by Police 2013-22`
  ) %>%
  
  rename_with(.fn = ~sub(" \\(Source.*\\)", "", .),               .cols = contains("Source:") 
  ) %>% 
  
  # Victim info ----

  mutate(name = `Victim's name`,
         
         name = case_when(
           name == "Name withheld by police" ~ "Unknown",
           TRUE ~ name),
         name = str_remove(name, " Jr."),
         name = str_remove(name, " Sr."),
         name = str_remove(name, " III"),
         name = str_remove(name, " II"),
         name = str_remove(name, " IV"),
         name = str_remove(name, " V$"),
         name = str_remove(name, " aka.*"),
         name = str_replace_all(name, "-"," "),
         fname = "NA", # prep for assignment below
         lname = "NA"
  ) %>%
  
  # age is a mess, chr num mix in orig file, read in as num so all
  # chr entries are NA
  
  mutate(age = replace_na(`Victim's age`, 999)
  ) %>%
  
  mutate(raceImp = `Victim's race`,
         
         raceImp = case_when(
           raceImp == "Asian" ~ "API",
           raceImp == "Black" ~ "BAA",
           raceImp == "Hispanic" ~ "HL",
           raceImp == "Native American" ~ "NAA",
           raceImp == "Pacific Islander" ~ "PI",
           raceImp == "White" ~ "WEA",
           raceImp == "Unknown race" ~ "Unknown",
           TRUE ~ raceImp),
         raceImp = fct_relevel(raceImp, "Unknown", after = Inf)
  ) %>%
  
  mutate(gender = `Victim's gender`,
         gender = fct_relevel(gender, "Unknown", after = Inf)
  ) %>%
  
  mutate(date = date(`Date of Incident (month/day/year)`),
         month = month(date, label=T),
         day = day(date),
         year = year(date)
  ) %>%
  
  # COD info ----

  ## Note that this records the types of force used
  ## Not the official cause of death

  mutate(cod = `Cause of death`,
         
         cod = case_when(
           grepl(",", cod) ~ "Multiple types of force used",
           grepl("Gunshot", cod) ~ "Gunshot",
           grepl("Asph|Physical|Spray|spray", cod) ~ "Asphyxiate/restrain/CW",
           grepl("Beaten", cod) ~ "Beaten",
           grepl("Taser", cod) ~ "Taser",
           grepl("Bomb|Bean", cod) ~ "Other",
           TRUE ~ cod),
         cod = factor(cod),
         cod = fct_relevel(cod, "Multiple types of force used", after = Inf),
         cod = fct_relevel(cod, "Other", after = Inf)
  ) %>%
  
  # Location info ----

  mutate(city = City,
         st = State,
         state = state_translate_fn(st),
         state.num = num[match(st, states)],
         zip = Zipcode,
         county = County
  ) %>%
  
  # Agency info ----

  mutate(agency = `Agency responsible for death`,
         
         agency.type = case_when(
           is.na(agency) ~ "Unknown",
           grepl("^.*,.*", agency) ~ "Multiple agencies",
           grepl("^.*(Campus|University|College).*", agency) ~ "University Campus Police",
           grepl("^.*(State|Patrol).*", agency) ~ "State Police",
           grepl("^.*Sheriff.*", agency) ~ "County Sheriff's Office",
           grepl("^.*(Police|police|Public Safety|Town|City).*", agency) ~ "Local Police Department",
           grepl("^.*Correction.*", agency) ~ "Corrections Dept",
           grepl("^.*County.*", agency) ~ "Other County unit",
           grepl("^.*U.S.*Alchohol.*", agency) ~ "US ATF",
           grepl("^.*U.S.*Border.*", agency) ~ "US CBP",
           grepl("^.*U.S. Federal Bureau.*", agency) ~ "US FBI",
           grepl("^.*U.S. Immigration.*", agency) ~ "US ICE",
           grepl("^.*U.S. Marshal.*", agency) ~ "US Marshals",
           grepl("^.*U.S.|Military|Air Force.*", agency) ~ "Other US unit",
           TRUE ~ "Other State unit"),
         
         agency.type = factor(agency.type,
                              levels = c("Local Police Department", "County Sheriff's Office", "State Police",
                                         "University Campus Police", "Corrections Dept",
                                         "US CBP", "US ICE", "US FBI", "US Marshals",
                                         "Other State unit", "Other County unit", "Other US unit",
                                         "Multiple agencies", "Unknown")),
         
         agency.ori = `ORI Agency Identifier (if available)`
  ) %>%

  
  ## LE narrative ----
  
  mutate(armed = `Armed/Unarmed Status`,
         
         armed = case_when(
           grepl("Unarmed", armed) ~ "Unarmed",
           grepl("armed|Armed", armed) ~ "Alleged armed",
           armed == "Vehicle" ~ "Vehicle",
           TRUE ~ "Unknown"),
         armed = fct_relevel(armed, "Unknown", 
                             after = Inf)
  ) %>%
  
  mutate(weapon = `Alleged Weapon`,
         
         weapon =case_when(
           weapon %in% c("undetermined", "unknown weapon") ~ "unknown",
           grepl("airsoft|bb|BB|bean|flare|toy", weapon) ~ "toy or nonlethal firearm",
           grepl("gun|Gun", weapon)  ~ "firearm",
           grepl("taser", weapon) ~ "taser",
           grepl("ax|Ax|blade|bayonet|bow|cleaver|cutter|edge|glass|hatchet|knife|knives", weapon)  ~ "edged weapon",
           grepl("machete|pitch|screwdiver|scissor|Scissor|sharp|spear|spike|sword", weapon)  ~ "edged weapon",
           grepl("blunt|bat|baton|brick|chair|club|crowbar|hammer|Hammer|iron|lamp|level", weapon)  ~ "blunt object",
           grepl("mallett|metal|oar|post|pole|pipe|rock|rod", weapon)  ~ "blunt object",
           grepl("shovel|hockey|table|tree|wrench|wood", weapon)  ~ "blunt object",
           weapon == "vehicle" ~ "vehicle",
           grepl("spray", weapon) ~ "chemical spray",
           grepl("None|No|no|none", weapon) ~ "none",
           TRUE ~ "other"),
         weapon = fct_relevel(weapon, "none", after = 0),
         weapon = fct_relevel(weapon, "other", after = Inf),
         weapon = fct_relevel(weapon, "unknown", after = Inf)
  ) %>%
  
  mutate(bodycam = `Body Camera`,
         
         bodycam = case_when(
           grepl("Bystand", bodycam) ~ "Bystander",
           grepl("Dash", bodycam) ~ "Dashcam",
           grepl("Surv", bodycam) ~ "Surveillance",
           grepl("yes|Yes", bodycam) ~ "Bodycam",
           grepl("no|No", bodycam) ~ "None",
           bodycam == "none" ~ "None",
           TRUE ~ "Unknown"),
         bodycam = fct_relevel(bodycam, "None", after = Inf),
         bodycam = fct_relevel(bodycam, "Unknown", after = Inf)
  ) %>%
  
  ## Case processing/disposition info 
  
  mutate(case.disposition = `Official disposition of death (justified or other)`,
         
         case.disposition = case_when(
           is.na(case.disposition) ~ "Unknown",
           grepl("Just|Clear|accident|Accident|Excusable", case.disposition) ~ "Justified or cleared",
           grepl("Convicted|convicted|guilty", case.disposition) ~ "Criminal conviction",
           grepl("Charged", case.disposition) ~ "Criminal charge",
           grepl("Civil.*award", case.disposition) ~ "Civil award",
           grepl("Civil", case.disposition) ~ "Civil suit filed",
           grepl("Pend|Under", case.disposition) ~ "Pending",
           grepl("Suicide|suicide", case.disposition) ~ "Murder/suicide",
           grepl("unknown|Unreported", case.disposition) ~ "Unknown",
           grepl("Unjustified", case.disposition) ~ "Administrative discipline",
           TRUE ~ case.disposition),
         case.disposition = factor(case.disposition),
         
         case.charges = `Criminal Charges?`,
         
         chief.prosecutor = `Officer Prosecuted by (Chief Prosecutor)`,
         court.prosecutor = `Officer Prosecuted by (Prosecutor in Court)`,
         prosecutor.race = `Prosecutor Race`,
         prosecutor.gender = `Prosecutor Gender`,
         prosecutor.term = `Chief Prosecutor Term`,
         prosector.party = `Chief Prosecutor Political Party`,
         prosecutor.special = `Special Prosecutor?`,

         case.ind.investigation = `Independent Investigation?`
  ) %>%
  
  # The following FE vars are not included in MPV
  
  # # see email chain with Burghardt re "Intended use of force (Developing)".  
  # # Veh/Purs is the unclean mix category that he is slowly working through
  # # Vehicle is the clean non-pursuit category (e.g., accident)
  # # Pursuit is the clean hot pursuit category
  # mutate(hotPursuit = `Intended use of force (Developing)`,
  #        hotPursuit = case_when(
  #          hotPursuit == "" ~ "Unknown",
  #          hotPursuit == "Undetermined" ~ "Unknown",
  #          hotPursuit == "Vehic/Purs" | hotPursuit == "Pursuit" ~ "Vehicle Pursuit",
  #          TRUE ~ "Other"),
#        hotPursuit = fct_relevel(hotPursuit, "Unknown", 
#                                 after = Inf)
#) %>%
# mutate(circumstances = `Intended use of force (Developing)`,
#        circumstances = case_when(
#          circumstances == "" ~ "Unknown",
#          circumstances == "Undetermined" ~ "Unknown",
#          circumstances == "Vehic/Purs" ~ "Mix of Vehicle hot pursuits and accidents",
#          circumstances == "Pursuit" ~ "Vehicle hot pursuits",
#          circumstances == "Vehicle" ~ "Vehicle accidents",
#          circumstances == "No" ~ "Unintended",
#          TRUE ~ circumstances),
#        circumstances = fct_relevel(circumstances, "Unknown", 
#                                    after = Inf)
#) %>%
# # Here we do not classify "suicide" as a homicide.
# mutate(homicide = circumstances,
#        homicide = case_when(
#          circumstances == "Suicide" ~ 0,
#          circumstances == "Unknown" ~ NA_real_,
#          TRUE ~ 1)
#) %>%

# NOTE: in FE the `Symptoms of mental illness? INTERNAL USE, NOT FOR ANALYSIS`
# field only flags cases where a mental health issue was known before the
# officers arrived on the scene.
mutate(symptoms = `Symptoms of mental illness?`,
       symptoms = case_when(
         symptoms %in% c("", "Unknown", "Unclear", "unknown") ~ "Unknown",
         symptoms == "Yes" ~ "Mental Illness",
         symptoms == "No" ~ "None",
         grepl("Drug", symptoms) ~ "Drugs or Alcohol",
         TRUE ~ symptoms) 
) %>%

  ## Officer info ----

  mutate(officer.names = `Names of Officers Involved (DRAFT)`,
         officer.races = `Race of Officers Involved (DRAFT)`,
         officer.previous = `Known Past Shootings of Officer(s) (DRAFT)`
  ) %>%  
  
  mutate(offduty = `Off-Duty Killing?`,
         
         offduty = ifelse(grepl("duty", offduty), "Yes", "Unknown"),
         offduty = fct_relevel(offduty, "Unknown", after = Inf)
         
  ) %>%
  
  ## Context info ----

  mutate(context = `Encounter Type (DRAFT)`,
         
         context = case_when(
           is.na(context) ~ "None or unknown",
           grepl("Crime/Dom", context) ~ "Violent-Domestic",
           grepl("Crime/Men", context) ~ "Violent-Mental health",
           grepl("Non-Violent", context) ~ "Nonviolent-Other",
           grepl("Violent|VIolent", context) ~ "Violent-Other",
           grepl("Weapon|weapon", context) ~ "Weapon",
           grepl("Traffic", context) ~ "Traffic stop",
           grepl("People", context) ~ "Other crimes against people",
           grepl("Other|None", context) ~ "None or unknown",
           grepl("Mental", context) ~ "Mental health or welfare check",
           grepl("Domestic", context) ~ "Domestic disturbance",
           TRUE ~ context),
         
         context.violent = case_when(
           grepl("Violent|Weapon", context) ~ "Violent crime",
           grepl("people", context) ~ "Nonviolent-Crime against person",
           TRUE ~ context),
         
         context.detail = `Initial Reported Reason for Encounter (DRAFT)`,
         
         context = fct_relevel(context, "None or unknown", after = Inf),
         context.violent = fct_relevel(context.violent, "None or unknown", after = Inf)
  ) %>%
  
  mutate(call4svc = `Call for Service? (DRAFT)`,
         
         call4svc = case_when(
           call4svc=="yes" ~ "Yes",
           call4svc == "Unavailable" ~ "Unknown",
           TRUE ~ call4svc),
         call4svc = fct_relevel(call4svc, "Unknown", after = Inf)
  ) %>%
  
  # Geographic-related info
  
  mutate(census.tract = `Census Tract Code`,
         latitude = as.numeric(Latitude),
         longitude = as.numeric(Longitude),
         #community.trulia = MPV1_raw[, grep("Trulia", names(MPV1_raw))][[1]],
         community.hud = `HUD UPSAI Geography`,
         community.nchs = MPV1_raw[, grep("NCHS", names(MPV1_raw))][[1]],
         census.pop = `Total Population of Census Tract 2019 ACS 5-Year Estimates`,
         census.nhw = `White Non-Hispanic Percent of the Population ACS`,
         census.nhb = `Black Non-Hispanic Percent of the Population ACS`,
         census.na = `Native American Percent of the Population ACS`,
         census.asian = `Asian Percent of the Population ACS`,
         census.pi = `Pacific Islander Percent of the Population ACS`,
         census.om = `Other/Two or More Race Percent of the Population ACS`,
         census.hisp = `Hispanic Percent of the Population ACS`,
         census.medhhinc = `Median household income ACS Census Tract`,
         congress.dist = `Congressional District`,
         congress.rep.lname = `Congressional Representative Last name`,
         congress.rep.fname = `Congressional Representative First name`,
         congress.rep.party = `Congressional Representative Party`
  ) %>%
  
  ## Media links and description ----

  mutate(url_info = `Link to news article or photo of official document`,
         url_pic = `URL of image of victim`,
         url.prosecutor = `Prosecutor Source Link`,
  ) %>%
  
  mutate(description = `A brief description of the circumstances surrounding the death`) %>%
  
  ## Create date/state/city-sorted rowID for cleaning ----
  arrange(date, st, city) %>%
  mutate(rowID = row_number()) %>%
  
  ## Pull cleaned variables into the new df ----
  select(c(rowID, mpvID:description))


# Create clickable url for Rpubs reports ----

MPV1_clean$url_click <- sapply(MPV1_clean$url_info, make_url_fn)

# Victim first and last name processing ----

name.list <- str_split(MPV1_clean$name, " ")
for(i in 1:length(name.list)) {
  MPV1_clean$fname[i] <- name.list[[i]][1]
  MPV1_clean$lname[i] <- name.list[[i]][length(name.list[[i]])]
}

# Officer name data ----
## Create tidy officer name dataframe with all IDs
## one record per officer name (NAs stripped at the end)
## This is saved as a separate df

officer_names <- MPV1_clean %>% 
  select(rowID, feID, wapoID, mpvID, st, year, officer.names) %>%
  mutate(name = officer.names,
         
         # fix long lists
         name = sub("Benjamin LeBlanc, officers.*",
                    "Benjamin LeBlanc, Officer Patrick Rubio, Officer Omar Tapia, Officer Luis Alvarado",
                    name),
         # fnames found here: https://news.yahoo.com/no-criminal-case.charges-6-spd-020200618.html
         name = sub("Austin, Cpl.*",
                    "Cpl. Anthony Guzzo, Officer Kyle Heuett, Officer Brandon Lynch, Officer Brandon Fabian, Officer Winston Brooks",
                    name),
         name = sub("Altoona Police Department:.*",
                    "Officer Kim Schuch, Officer Leah Wolff, Officer Gracia Larson, Officer Robert Schreier, Officer Michael Cullen, Deputy Joseph Wollum, Deputy Jacob Pake",
                    name),
         name = sub("Lake County Sheriff's Department deputy Victor Zamora.*",
                    "Deputy Victor Zamora, Officer Luke Schreiber, Officer Jacob Patzschke, Daniel Kolodzie",
                    name),
         name = sub("Officer Irwin, Norton, Davis. Becketts",
                    "Officer Irwin, Officer Norton, Officer Davis, Officer Becketts",
                    name),
          name = sub("Michael Dekun, James Heaslip, .*",
                    "Michael Dekun, James Heaslip, Ryan Young, BP agent Don Kress, BP agent Ryan Champion",
                    name),
         name = sub("Waukesha County Deputy James Soneberg.*",
                    "Deputy James Soneberg, Officer Daniel Bloedow",
                    name),
         name = sub("Lane County sheriff's dept: Sgt. Jason Wilson.*",
                    "Sgt. Jason Wilson, Deputy Derek Bastinelli, Deputy Marvin Combs, Deputy Brian Devault, Deputy Brian Jessee, Sgt. James Macfarlane, Officer Robert Merryman, Deputy Kody Reavis, Deputy Eric Tipler and Deputy Aaron Gardner",
                    name),
         name = sub("Arapahoe County Sheriff.*Deputy Robert Knudson",
                    "Deputy Robert Knudson",
                    name),
         name = sub("Everest Metropolitan Police Department Detective.*",
                    "Detective Sergeant Dan Goff, Deputy Matthew Bell",
                    name),
         name = sub("Reynaldo Rodriguez of the Bulloch County .*",
                    "Reynaldo Rodriguez, Gary Provost, Deputy Nathan Singletary",
                    name),
         name = sub(" and detectives Clay Saunders.*",
                    ", Detective Clay Saunders, Detective Jason Willis, Detective Thomas Akin",
                    name),
         name = sub("state law protects the identity of undercover officers.",
                    "Undercover Officer",
                    name),

         name = sub("\\(.*\\)", "", name), # delete parentheticals
         
         # fix comma issues; comma used for string separation later
         name = gsub(",  ", ", ", name),
         name = gsub(" and|, and", ",", name),
         name = gsub(" \\+", ",", name),
         
         name = sub("\\.$|\\. $|,$|, $", "", name), # delete trailing punctuation (need to do this last)
         
         # fix idiosyncratic problems
         name = sub(" and others", "", name),
         name = sub(", unknown", "", name),
         name = sub("32, ", "", name),
         name = sub("William Evans \\(deceased.*", "William Evans", name),
         name = sub("Geoffrey Barber Alex Jansen", "Geoffrey Barber, Alex Jansen", name),
         name = sub("Paulding County ", "", name),
         name = sub("fired several times", "", name),
         name = sub("Officers ", "", name),
         name = sub(", 2nd officer unknown", "", name),
         
         name = sub("declined.*|.*declined.*", NA, name),
         name = sub("5 officers involved", NA, name),
         name = sub("SWAT team arrived.*", NA, name),
         name = sub("4 officers", NA, name),
         name = sub("Multiple names in police report.*", NA, name)
         
  ) %>%

  separate_rows(name, sep = ',\\s*') %>%
  #separate_rows(name) %>%
  
  # Create title field
  mutate(title = case_when(
    grepl("Deputy", name) ~ "Deputy",
    grepl("Officer|officer", name) ~ "Officer",
    grepl("Det\\.|Detective|Det ", name) ~ "Detective",
    grepl("Cpl\\.", name) ~ "Corporal",
    grepl("Lt\\.", name) ~ "Lieutenant",
    grepl("Sgt\\.|Sgt", name) ~ "Seargant",
    grepl("Investigator", name) ~ "Investigator",
    grepl("Trooper First Class", name) ~ "Trooper First Class",
    grepl("Trooper", name) ~ "State Trooper",
    grepl("Senior Inspector", name) ~ "Senior Inspector",
    grepl("Senior Special Agent", name) ~ "Senior Special Agent",
    grepl("Sergeant", name) ~ "Sergeant",
    grepl("Sheriff", name) ~ "Sheriff",
    grepl("Safety Officer", name) ~ "Safety Officer",
    grepl("Patrolman", name) ~ "Patrolman",
    grepl("BP agent", name) ~ "Border Patrol Agent",
    grepl("Chief", name) ~ "Chief",
    grepl("MPO", name) ~ "MPO",
  ))  %>%
  
  # Delete titles from names
  mutate(name = ifelse(grepl("Roessel", name),
                       "Dahle Roessel",
                       name),
         name = sub(".*several times", "", name),
         name = sub("Deputy |Officer |officer |", "", name),
         name = sub("Det\\. |Detective |Det ", "", name),
         name = sub("Cpl\\. |Corporal |Lt\\. |Sgt. |Sgt |Investigator ", "", name),
         name = sub("Trooper First Class |Trooper |State Trooper ", "", name),
         name = sub("Senior Inspector |Sergeant |Patrolman |Sheriff ", "", name),
         name = sub("BP Agent |Chief |MPO ", "", name),
         name = sub("Safety Officer ", "", name),
         name = sub("Senior Special Agent ", "", name)
  ) %>%
  
  # fix initials and name decorators
  mutate(name = sub(" Jr\\.| Jr| Sr\\.| II| III|III ", "", name),
         name = sub("([A-Z])\\.([A-Z])\\.|([A-Z])\\. ([A-Z])\\.", "\\1\\2", name), # combine 2 initials
         name = gsub("\\.", "", name) # remove periods in all initials
  ) %>%
  
  # delete trailing whitespace
  mutate(name = sub(" *$| $", "", name)
  ) %>%

  # set up vectors for fname and lname
  mutate(fname = NA,
         lname = NA
  ) %>%  
  
  filter(!is.na(name)) 

name.list <- str_split(officer_names$name, " ")
for(i in 1:length(name.list)) {
  if(length(name.list[[i]]) > 1) {
    officer_names$fname[i] <- name.list[[i]][1]
    officer_names$lname[i] <- name.list[[i]][length(name.list[[i]])]
  } else { # only have last name 
    officer_names$lname[i] <- name.list[[i]][1]
  }
}

# Officer names WA ----

officer_names_wa <- officer_names %>% 
  filter(st == "WA")

officer_name_table_wa <- officer_names %>%
  filter(st == "WA") %>%
  group_by(lname, fname) %>%
  count() %>%
  arrange(desc(n))


# Save clean data ----

scrape_date_mpv <- Sys.Date()
last_date_mpv <- max(MPV1_clean$date)
last_date_eoy_mpv <- ifelse(
  last_date_mpv == ymd(paste0(year(last_date_mpv),"-12-31")), 1, 0)

last_complete_mo_mpv <- ifelse(month(last_date_mpv)==1, 
                           12, 
                           month(last_date_mpv)-1)
last_complete_yr_mpv <- ifelse(last_date_eoy_mpv == 1,
                           year(last_date_mpv), 
                           year(last_date_mpv)-1)

mpv_clean <- MPV1_clean
mpv_clean_wa <- MPV1_clean %>% filter(st == "WA")

save(list = c("mpv_clean", "mpv_clean_wa", 
              "officer_names", "officer_names_wa", 
              "officer_name_table_wa",
              "scrape_date_mpv", "last_date_mpv", "last_date_eoy_mpv",
              "last_complete_mo_mpv", "last_complete_yr_mpv"),
     file = here::here("data-outputs", "MPVData_Clean.rda"))

