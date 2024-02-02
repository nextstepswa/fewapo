# MPV cleaning script ----

## input:  mpv_temp (from fixes_preclean_mpv.R)
## interim: mpv_draft
## output:  mpv_clean

## Until we merge this in completely, we clean and save RDA files out here
## Clean WA df will be used to check for post 2021 WA updates only

num <- 1:51

mpv_draft <- mpv_temp %>%
  
  rename_with(.fn = ~sub(" \\(Source.*\\)", "", .), .cols = contains("Source:")) %>% 
  rename_with(.fn = ~sub(" \\(https.*\\)", "", .), .cols = contains("https")) %>% 
  rename_with(.fn = ~sub(" \\(DRAFT\\)", "", .), .cols = contains("DRAFT")) %>% 
  
  # Victim info ----

mutate(name = case_when(
  name == "Name withheld by police" ~ "Unknown",
  TRUE ~ name),
  name = str_remove(name, " Jr"),
  name = str_remove(name, " Sr"),
  name = str_remove(name, " III"),
  name = str_remove(name, " II"),
  name = str_remove(name, " IV$"),
  name = str_remove(name, " V$"),
  name = str_remove(name, " aka.*"),
  name = str_replace_all(name, "-"," "),
  name = str_replace_all(name, "\\.|,",""),
  
  # preserve aka names as "name2"
  name2 = if_else(grepl(" or ", name), str_remove(name, ".* or "), ""),
  # and remove the aka from the primary name
  name = str_remove(name, " or .*"),
  
  fname = "NA", # prep for assignment below
  lname = "NA"
) %>%
  
  ## Age 
  ## uses "Unknown" for NA which turns this into a character
  
  mutate(age = as.numeric(`Victim's age`),
         age = replace_na(age, 999)
  ) %>%
  
  mutate(gender = str_to_sentence(`Victim's gender`),
         gender = fct_relevel(gender, "Unknown", after = Inf)
  ) %>%
  
  ## Race - not sure what's happening with imputation
  mutate(race = `Victim's race`,
         
         race = case_when(
           race == "Asian" ~ "API",
           race == "Black" ~ "BAA",
           race == "Hispanic" ~ "HL",
           race == "Native American" ~ "NAA",
           race == "Pacific Islander" ~ "PI",
           race == "White" ~ "WEA",
           grepl("Unknown", race) ~ "Unknown",
           TRUE ~ race),
         race = fct_relevel(race, "Unknown", after = Inf)
  ) %>%
  
  mutate(month = month(date, label=T),
         day = day(date),
         year = year(date)
  ) %>%
  
  # Location info ----

mutate(city = City,
       st = State,
       state = state_fullname_fn(st),
       state.num = num[match(st, st.abb51)],
       zip = as.numeric(Zipcode),
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
  
  ## Description ----
mutate(description = `A brief description of the circumstances surrounding the death`) %>%
  
  ## Case disposition  ----

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
       
       case.charges = `Criminal Charges?`
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
  
  mutate(threat.level = `Alleged Threat Level`,
         threat.description = `Threat Level Description`,
         fleeing = Fleeing
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

mutate(offduty = `Off-Duty Killing?`,
       offduty = ifelse(grepl("duty", offduty), "Yes", "Unknown"),
       offduty = fct_relevel(offduty, "Unknown", after = Inf),
       
       officer.names = `Names of Officers Involved`,
       officer.races = `Race of Officers Involved`,
       officer.previous = `Known Past Shootings of Officer(s)`
) %>%  
  
  
  ## Context info ----

mutate(context = `Encounter Type`,
       
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
       
       context = fct_relevel(context, "None or unknown", after = Inf),
       context.violent = fct_relevel(context.violent, "None or unknown", after = Inf),
       
       context.detail = `Initial Reported Reason for Encounter`,
       
       call4svc = `Call for Service?`,
       call4svc = case_when(
         call4svc=="yes" ~ "Yes",
         call4svc == "Unavailable" ~ "Unknown",
         TRUE ~ call4svc),
       call4svc = fct_relevel(call4svc, "Unknown", after = Inf)
) %>%
  
  # Geographic-related info
  
  mutate(census.tract = `Census Tract Code`,
         community.hud = `HUD UPSAI Geography`,
         community.nchs = mpv_temp[, grep("NCHS", names(mpv_temp))][[1]],
         medhhinc.acs = `Median household income ACS Census Tract`,
         latitude = Latitude,
         longitude = Longitude,
         census.pop = `Total Population of Census Tract 2019 ACS 5-Year Estimates`,
         census.nhw = `White Non-Hispanic Percent of the Population ACS`,
         census.nhb = `Black Non-Hispanic Percent of the Population ACS`,
         census.na = `Native American Percent of the Population ACS`,
         census.asian = `Asian Percent of the Population ACS`,
         census.pi = `Pacific Islander Percent of the Population ACS`,
         census.om = `Other/Two or More Race Percent of the Population ACS`,
         census.hisp = `Hispanic Percent of the Population ACS`,
         congress.dist = `Congressional District`,
         congress.rep = `Congressional Representative Full Name`,
         congress.rep.party = `Congressional Representative Party`
  ) %>%
  
  ## Prosecutor info ----
mutate(chief.prosecutor = `Officer Prosecuted by (Chief Prosecutor)`,
       prosecutor.race = `Prosecutor Race`,
       prosecutor.gender = `Prosecutor Gender`,
       prosector.party = `Chief Prosecutor Political Party`,
       prosecutor.term = `Chief Prosecutor Term`,
       court.prosecutor = `Officer Prosecuted by (Prosecutor in Court)`,
       prosecutor.special = `Special Prosecutor?`,
       independent.investigation = `Independent Investigation?`,
       url.prosecutor = `Prosecutor Source Link`
) %>%
  
  ## Media links ----

mutate(url_pic = `URL of image of victim`,
       url_info = `Link to news article or photo of official document`,
) %>%
  
  ## Create date/state/city-sorted rowID for cleaning ----
arrange(date, st, city) %>%
  mutate(rowID = row_number()) %>%
  
  ## Select the cleaned variables into the new df ----
select(rowID, mpvID:date, name2:cod,
       case.disposition:url_info, description) 


# Create clickable source url for Rpubs reports ----

mpv_draft$url_click <- sapply(mpv_draft$url_info, make_url_fn)

# Victim first and last name processing ----

name.list <- str_split(mpv_draft$name, " ")
for(i in 1:length(name.list)) {
  mpv_draft$fname[i] <- name.list[[i]][1]
  mpv_draft$lname[i] <- name.list[[i]][length(name.list[[i]])]
}

# Officer name data ----
## Create tidy officer name dataframe with all IDs
## one record per officer name (NAs stripped at the end)
## This is saved as a separate df

# temp adds for WA
mpv_draft$officer.names[mpv_draft$name == "Ethan Austin Murray"] <- "Deputy Joseph Wallace"

officer_names <- mpv_draft %>% 
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


# Final datasets ----

mpv_clean <- mpv_draft
mpv_clean_wa <- mpv_clean %>% filter(st == "WA")

