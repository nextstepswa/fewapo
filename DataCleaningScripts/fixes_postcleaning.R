# Post-cleaning edits when we know more than FE and WaPo
# Executed immediately before name split and merging

# Amost all of these are for WA state
# should be kept up to date and commented out when no longer relevant
# ---------------------------------------------------------------------

# NOTE: WAPO 4568 is the only case since 2015 not found in FE.  From the news coverage
# I can't tell if the victim died.  Have reported the case to FE.
# https://www.kiro7.com/news/local/police-investigating-officer-involved-shooting-in-federal-way/930686522/


# Peter Collins 8/28/2022

if (wapo_clean$name[wapo_clean$wapoID == 8367] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 8367] <- "Peter T Collins"
  wapo_clean$age[wapo_clean$wapoID == 8367] <- 40
  wapo_clean$gender[wapo_clean$wapoID == 8367] <- "Male"
  print("Peter Collins fix for WaPo")
} else {
  print("WaPo Collins fix not needed anymore")
}

# Ayala 8/6/2022

wapo_clean$city[wapo_clean$wapoID==8313] <- "Zillah"


# Mauricio Martinez Yanez 3/16/2022

if (wapo_clean$name[wapo_clean$wapoID == 7830] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 7830] <- "Mauricio Martinez Yanez"
  wapo_clean$age[wapo_clean$wapoID == 7830] <- 19
  print("Mauricio Martinez Yanez fix for WaPo")
} else {
  print("WaPo Yanez fix not needed anymore")
}

# William Michael Stephens 3/5/2022
if (wapo_clean$name[wapo_clean$wapoID == 7783] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 7783] <- "William Michael Stephens"
  wapo_clean$age[wapo_clean$wapoID == 7783] <- 39
  print("Fixed William Michael Stephens for WaPo")
} else {
  print("WaPo Stephens fix not needed anymore")
}

# Joseph Sanchez 2/11/2022

if (wapo_clean$name[wapo_clean$wapoID == 7735] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 7735] <- "Joseph Sanchez"
  wapo_clean$age[wapo_clean$wapoID == 7735] <- 36
  print("Joseph Sanchez fix for WaPo")
} else {
  print("WaPo Joseph Sanchez fix not needed anymore")
}

# Sorin Ardelean 12/27/2021
if (fe_clean$name[fe_clean$feID == 31462] == "Unknown"){
  fe_clean$name[fe_clean$feID == 31462] <- "Sorin Ardelean"
  fe_clean$age[fe_clean$feID == 31462] <- 43
  fe_clean$COD[fe_clean$feID == 31462] <- "Gunshot"
  fe_clean$agency[fe_clean$feID == 31462] <- "Algona Police Department"
  fe_clean$url_info[fe_clean$feID == 31462] <- "https://www.auburn-reporter.com/news/fatal-police-shooting-of-suspect-in-algona-hostage-taking-incident-is-subject-of-investigation/"
  fe_clean$url_click[fe_clean$feID == 31462] <- make_url_fn(fe_clean$url_info[fe_clean$feID == 31462])
  print("Fixed Sorin Ardelean for FE")
} else {
  print("FE Ardelean fix not needed anymore")
}

if (wapo_clean$name[wapo_clean$wapoID == 7518] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 7518] <- "Sorin Ardelean"
  wapo_clean$age[wapo_clean$wapoID == 7518] <- 43
  print("Fixed Sorin Ardelean for WaPo")
} else {
  print("WaPo Ardelean fix not needed anymore")
}

# Werner Anderson 2018, died after injection of ketamine while held down
# FE classifies Intended use of force as "Undetermined", 
# we will recode as "Other force"
fe_clean$circumstances[fe_clean$feID==22065] <- "Other force"
fe_clean$homicide[fe_clean$feID==22065] <- 1
fe_clean$hotPursuit[fe_clean$feID==22065] <- "Other"


# Duplicate case: Alex Martinez 2/27/2011, remove ID 9914 and correct date for ID 9917
# reported to FE, it's now labeled as a dupe but the date hasn't been fixed
fe_clean$date[fe_clean$ID==9917] <- fe_clean$date[fe_clean$ID==9914]
fe_clean$day[fe_clean$ID==9917] <- fe_clean$day[fe_clean$ID==9914]
fe_clean <- fe_clean %>% filter(feID != 9914)

# correct a taser death, see url
fe_clean$cod[fe_clean$feID==25484] <- "Taser"
fe_clean$url_info[fe_clean$feID==25484] <- "https://apnews.com/article/5c4465da24a4493085b0bd622a61e253"

# Nancy King
fe_clean$foreknowledge[fe_clean$feID==29260] <- "Mental Illness"
fe_clean$url_info[fe_clean$feID==29260] <- 
  "https://www.washingtonpost.com/nation/2020/12/09/spokane-jail-shooting-nancy-king/"


# better article for Tofte
fe_clean$url_info[fe_clean$feID==28883] <- 
  "https://www.kptv.com/news/investigators-id-suspect-shot-killed-by-officer-during-foot-chase-in-longview/article_8fef5874-076f-11eb-8ba3-833f16033386.html"

# correct city and or state
wapo_clean$st[wapo_clean$wapoID==6469] <- "ID"
wapo_clean$state[wapo_clean$wapoID==6469] <- "Idaho"

wapo$city[wapo$id==4655] <- "Kalama"
wapo$st[wapo$id==4655] <- "WA"
wapo$state[wapo$id==4655] <- "Washington"
wapo$state.num[wapo$id==4655] <- 47

# Lat Long input errors ----------------

## WA fixes ----
fe_clean$longitude[fe_clean$feID == 30825] <- -119.896698
fe_clean$latitude[fe_clean$feID == 12731] <- 40.640428
fe_clean$latitude[fe_clean$feID == 28891] <- 42.167834


## CA fixes ----
fe_clean <- fe_clean %>%
  mutate(longitude = case_when(feID==23985 ~ -122.4808521,
                               feID==28133 ~ -118.3790706,
                               feID==24121 ~ -117.211268,
                               feID==24104 ~ -117.924074,
                               feID==27309 ~ -118.120616,
                               feID==24114 ~ -121.5532966,
                               TRUE ~ longitude),
         latitude = case_when(feID==23985 ~ 38.2982414,
                              feID==28133 ~ 34.1866902,
                              feID==24121 ~ 34.0483588,
                              feID==24104 ~ 33.8333398,
                              feID==27309 ~ 33.8975278,
                              feID==24114 ~ 39.4473354,
                              TRUE ~ latitude)
  )

## LA fixes -----
fe_clean <- fe_clean %>%
  mutate(longitude = case_when(feID==24002 ~ -93.2850266,
                               feID==24001 ~ -90.193529,
                               feID==28136 ~ -90.1141944,
                               TRUE ~ longitude),
         latitude = case_when(feID==24002 ~ 31.1143869,
                              feID==24001 ~ 30.4955241,
                              feID==28136 ~ 29.8974833,
                              TRUE ~ latitude)
  )



