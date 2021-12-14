# edits when we know more than FE, 
# should be kept up to date and commented out when no longer relevant

# fe_clean[!is.na(fe_clean$feID) & fe_clean$feID == 29054,] <- 
#   fe_clean[!is.na(fe_clean$feID) & fe_clean$feID == 29054,] %>%
#   mutate(name = "Darren Butrick",
#          date = lubridate::ymd("2020-11-04"),
#          month = "Nov",
#          day = 4,
#          year = 2020)

# wapo_clean[!is.na(wapo_clean$wapoID) & wapo_clean$wapoID == 6290,] <- 
#   wapo_clean[!is.na(wapo_clean$wapoID) & wapo_clean$wapoID == 6290,] %>%
#   mutate(name = "Darren Butrick",
#          gender = "Male")

# NOTE: WAPO 4568 is the only case since 2015 not found in FE.  From the news coverage
# I can't tell if the victim died.  Have reported the case to FE.
# https://www.kiro7.com/news/local/police-investigating-officer-involved-shooting-in-federal-way/930686522/

# Werner Anderson 2018, died after injection of ketamine while held down
# FE classifies Intended use of force as "Undetermined", 
# we will recode as "Less-than-lethal force"
fe_clean$circumstances[fe_clean$feID==22065] <- "Less-than-lethal force"
fe_clean$homicide[fe_clean$feID==22065] <- 1
fe_clean$hotPursuit[fe_clean$feID==22065] <- "Other"

# Lotsa problems:  Shawn Michael Roy Montoya Duplicate
# one is from Beaverton OR
fe_clean$st[fe_clean$feID==30444] <- "OR"
fe_clean$state[fe_clean$feID==30444] <- "Oregon"
fe_clean$latitude[fe_clean$feID==30444] <- 45.477210
fe_clean$longitude[fe_clean$feID==30444] <- -122.796570

# the other is not Montoya
# name retrieved from Sno Co Med Examiner
# https://snohomishcountywa.gov/ArchiveCenter/ViewFile/Item/6705
fe_clean$name[fe_clean$feID==30454] <- "Thomas B. Murroni"
fe_clean$description[fe_clean$feID==30454] <- 
  sub("Shawn Michael Roy Montoya", "Murroni", 
       fe_clean$description[fe_clean$feID==30454])
fe_clean$description[fe_clean$feID==30454] <- 
  sub("Montoya", "Murroni", 
      fe_clean$description[fe_clean$feID==30454])


# Duplicate case: Alex Martinez 2/27/2011, remove ID 9914 and correct date for ID 9917
# reported to FE
fe_clean$date[fe_clean$ID==9917] <- fe_clean$date[fe_clean$ID==9914]
fe_clean$day[fe_clean$ID==9917] <- fe_clean$day[fe_clean$ID==9914]
fe_clean <- fe_clean %>% filter(feID != 9914)

# fix Lat Long input errors
fe_clean$longitude[fe_clean$feID == 30825] <- -119.896698
fe_clean$latitude[fe_clean$feID == 12731] <- 40.640428
fe_clean$latitude[fe_clean$feID == 28891] <- 42.167834
                  
# Use Jenoah Donald as name: Donald D. Jonah aka Jenoah Donald
fe_clean$name[fe_clean$feID==29605] <- "Jenoah Donald"

# Shawn McCoy
# fe_clean$name[fe_clean$feID==29442] <- "Shawn McCoy"

# correct a taser death, see url
fe_clean$cod[fe_clean$feID==25484] <- "Tasered"
fe_clean$url_info[fe_clean$feID==25484] <- "https://apnews.com/article/5c4465da24a4493085b0bd622a61e253"


# Nancy King
fe_clean$foreknowledge[fe_clean$feID==29260] <- "Mental Illness"
fe_clean$url_info[fe_clean$feID==29260] <- 
  "https://www.washingtonpost.com/nation/2020/12/09/spokane-jail-shooting-nancy-king/"


# better article for Tofte
fe_clean$url_info[fe_clean$feID==28883] <- 
  "https://www.kptv.com/news/investigators-id-suspect-shot-killed-by-officer-during-foot-chase-in-longview/article_8fef5874-076f-11eb-8ba3-833f16033386.html"

# correct state, ID not WA
wapo_clean$st[wapo_clean$wapoID==6469] <- "ID"
wapo_clean$state[wapo_clean$wapoID==6469] <- "Idaho"

# Add name to wapo case
wapo_clean$name[wapo_clean$wapoID==6989] <- "Dwayne Michael Fields"

# # Force the right match since wapo name is NA
# wapo_clean$name[wapo_clean$wapoID==6548] <- "John Eric Ostbye"


## Permanent fixes ------------------------------------------------

fe_clean$name[fe_clean$feID==23531] <- "Joshua Spottedhorse"
fe_clean$name[fe_clean$feID==18509] <- "Bruce R. Johnson"
fe_clean$name[fe_clean$feID==16382] <- "Samuel Toshiro Smith"
wapo_clean$name[wapo_clean$wapoID==1778] <- "Jeffrey Martelli"
wapo_clean$name[wapo_clean$wapoID==3198] <- "Kyle Gray"

wapo_clean$city[wapo_clean$wapoID==3164] <- "Tieton"


## CA fixes -------------------------------------------------
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

## LA fixes -------------------------------------------------
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



