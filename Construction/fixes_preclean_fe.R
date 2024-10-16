# Pre-cleaning fixes for all cases -------

## input: fe_temp
## output fe_temp

# The cleaning script creates new variables based on these fields
# so the fixes need to be made before the cleaning, otherwise they
# create merge errors. Merging is typically where these problems 
# are first identified, but fixes from other sources are noted

# Goal is to report to the dataset maintainer

# FE variable names already modified in scraping:  
# "feID", "age" and "date"

# Age corrections, using pre-corrected variable "fe_temp$age" ----

fe_temp$age[fe_temp$feID==23382] <- 26
fe_temp$age[fe_temp$feID==30890] <- 53
fe_temp$age[fe_temp$feID==18537] <- 41 # WTSC
fe_temp$age[fe_temp$feID==26981] <- 24 # WTSC par 3796019
fe_temp$age[fe_temp$feID==28776] <- 56 # WTSC par 3876377
fe_temp$age[fe_temp$feID==28777] <- 56 # WTSC par 3876377
fe_temp$age[fe_temp$feID==29763] <- 32 # WTSC par EB12826

# KCME
fe_temp$age[fe_temp$feID==18537] <- 41 # George Alexander
fe_temp$age[fe_temp$feID==23500] <- 47 # Curtis Elroy Tade
fe_temp$age[fe_temp$feID==25615] <- 51 # Peter Morgan
fe_temp$age[fe_temp$feID==29500] <- 25 # Abdulahi Aroni
fe_temp$age[fe_temp$feID==31223] <- 28 # Alexander Whittall

# MPV matching
fe_temp$age[fe_temp$feID == 28476] <- 34 


# Race corrections ----

fe_temp$Race[fe_temp$feID==31345] <- "" # Christopher Anthony Alexander name in race field
fe_temp$Race[fe_temp$feID==25367] <- "Asian/Pacific Islander" # Iosia Faletogo
fe_temp$Race[fe_temp$feID==28883] <- "Native American/Alaskan" # Justin Lee Aguilar Tofte, from FB post https://www.facebook.com/lastrealindians/posts/pfbid02vkG5oNWeMVdbrCVce5qF5aBTNMxjj1nTQ1barwYbCS15TXF2fPJC2rkMennMqZ4ol?__cft__[0]=AZVxzrdMUO_GpvTTZLXVjMSUrZBYklPZ641hzmhgZQ0wVPWTg4XuUUCtzVYCE612cYgc3sOqAned7ikDHBO5uIai4k039t42LlSky8rkYpaRCOwf-g4Wq0MdRBLZahxxQ2w&__tn__=%2CO%2CP-R
fe_temp$Race[fe_temp$feID==28501] <- "Hispanic/Latino" # Juan Rene Hummel, from media photo https://www.seattletimes.com/seattle-news/eastside/family-of-man-killed-by-bothell-police-officer-says-they-still-dont-have-answers-2-months-after-fatal-shooting/
fe_temp$Race[fe_temp$feID==15865] <- "European-American/White" # Jamison Edward Childress, from media photo https://www.ctvnews.ca/canada/border-patrol-agent-justified-in-fatal-shooting-of-b-c-man-prosecutor-1.2381699


# KCME
fe_temp$Race[fe_temp$feID==18537] <- "Native American/Alaskan" # George Alexander
fe_temp$Race[fe_temp$feID==23500] <- "European-American/White" # Curtis Elroy Tade
fe_temp$Race[fe_temp$feID==25615] <- "African-American/Black" # Peter Morgan
fe_temp$Race[fe_temp$feID==29500] <- "African-American/Black" # Abdulahi Aroni
fe_temp$Race[fe_temp$feID==31223] <- "European-American/White" # Alexander Whittall

# Gender corrections ----

fe_temp$Gender[fe_temp$feID==26981] <- "male" # WTSC par 3796019

fe_temp$Gender[fe_temp$feID %in% 
            c(12617, 15664, 29700)] <- "Female"

fe_temp$Gender[fe_temp$feID %in%
            c(14053, 16077, 20388, 24130, 25285, 27067, 28476,
              28857, 29393, 29577, 29808, 29434, 30741,
              25490, 28892)] <- "Male"

fe_temp$Gender[fe_temp$feID == 22438] <- "Nonbinary"

fe_temp$Gender[fe_temp$feID %in%
            c(17219, 18658, 28146, 30298)] <- "Transgender man"

fe_temp$Gender[fe_temp$feID %in%
            c(15916, 22098, 24948, 25568, 25822,
              28017)] <- "Transgender woman"


# Name corrections ----

fe_temp$Name[fe_temp$feID==8584] <- "Rodrick Jones aka Roderick Jones" # consistency
fe_temp$Name[fe_temp$feID==30271] <- "Raul Rosas Zarose"
fe_temp$Name[fe_temp$feID==30775] <- "Steven Dean Primm"
fe_temp$Name[fe_temp$feID==30280] <- "Zaekwon Malik Gullatte"
fe_temp$Name[fe_temp$feID==30410] <- "Christopher M. VanKleeck"
fe_temp$Name[fe_temp$feID==30487] <- "Hunter Brittain"
fe_temp$Name[fe_temp$feID==30619] <- "Gerardo Chavez Martinez"
fe_temp$Name[fe_temp$feID==30718] <- "Aler Ronald Velasquez Escobar"
fe_temp$Name[fe_temp$feID==30748] <- "Luis Manuel Arias Garcia"
fe_temp$Name[fe_temp$feID==30865] <- "Alexander Domina"
fe_temp$Name[fe_temp$feID==30886] <- "Jose Angel Francisco Baca"
fe_temp$Name[fe_temp$feID==31002] <- "Irlin Marcloni Cabal Paz"
fe_temp$Name[fe_temp$feID==30875] <- "Omar Lazano Hernandez"
fe_temp$Name[fe_temp$feID==31069] <- "Nurgazy Mamyrov"
fe_temp$Name[fe_temp$feID==29183] <- "Javon Brice"
fe_temp$Name[fe_temp$feID==28915] <- "John Hare"
fe_temp$Name[fe_temp$feID==30798] <- "Setha Phangdy"
fe_temp$Name[fe_temp$feID==29557] <- "Steven Emmet Crosby"

fe_temp$Name[fe_temp$feID==29605] <- "Jenoah Donald"
fe_temp$Name[fe_temp$feID==23531] <- "Joshua Spottedhorse"
fe_temp$Name[fe_temp$feID==16382] <- "Samuel Toshiro Smith"

# after wapo changes 6/2023
fe_temp$Name[fe_temp$feID == 12617] <- "Stacey Stout"
fe_temp$Name[fe_temp$feID == 15664] <- "Jessica Hernandez"
fe_temp$Name[fe_temp$feID == 28476] <- "Matthew Blake Dixon"
fe_temp$Name[fe_temp$feID == 29577] <- "Eric Kessler"
fe_temp$Name[fe_temp$feID == 29808] <- "Charles Ray Phillips"
fe_temp$Name[fe_temp$feID == 30741] <- "Michael Adams"
fe_temp$Name[fe_temp$feID == 25568] <- "Joshua Rembert Williams"

fe_temp$Name[fe_temp$feID==27067] <- "Shawn Wilcox"

# KCME info
fe_temp$Name[fe_temp$feID==18537] <- "George Alexander"
fe_temp$Name[fe_temp$feID==23500] <- "Curtis Elroy Tade"
fe_temp$Name[fe_temp$feID==25615] <- "Peter Morgan"
fe_temp$Name[fe_temp$feID==29500] <- "Abdulahi Aroni"
fe_temp$Name[fe_temp$feID==31223] <- "Alexander Whittall"


# Date corrections (name, format already fixed) ----

fe_temp$date[fe_temp$feID==29416] <- as.Date("2020-12-31") 
fe_temp$date[fe_temp$feID==29677] <- as.Date("2021-02-15") 
fe_temp$date[fe_temp$feID==30299] <- as.Date("2021-05-23") 
fe_temp$date[fe_temp$feID==26985] <- as.Date("2019-10-11") # Obits say 10/10, but media & WTSC says 10/11
fe_temp$date[fe_temp$feID==30199] <- as.Date("2021-05-08") # Everton Garfield Brown
fe_temp$date[fe_temp$feID==12757] <- as.Date("2013-05-07") # Danny Valdes
fe_temp$date[fe_temp$feID==14290] <- as.Date("2014-04-03") # Dustin Glover


# State fixes ----
fe_temp$State[fe_temp$feID==28896] <- "FL"


# City fixes ----

fe_temp$`Location of death (city)`[fe_temp$feID==18333] <- "Muckelshoot Reservation" # Renee Davis
fe_temp$`Location of death (city)`[fe_temp$feID==19583] <- "Des Moines" # William Stokes
# google maps is wrong, Fredrickson is a city
# https://zipmap.net/Washington/Pierce_County/Frederickson.htm
fe_temp$`Location of death (city)`[fe_temp$feID==19831] <- "Frederickson" # Charles Shands
fe_temp$`Location of death (city)`[fe_temp$feID==21885] <- "Frederickson" # Eduardo Navarrete
fe_temp$`Location of death (city)`[fe_temp$feID==23934] <- "Elk Plain" # William Langfitt, EP verified as Census designated place
fe_temp$`Location of death (city)`[fe_temp$feID==27321] <- "Spokane" # Clando Anitok
fe_temp$`Location of death (city)`[fe_temp$feID==27830] <- "Eatonville" # Brandon Stokes
fe_temp$`Location of death (city)`[fe_temp$feID==27954] <- "Seattle" # Shaun Fuhr
fe_temp$`Location of death (city)`[fe_temp$feID==29038] <- "Hazel Dell" # Kevin Peterson
fe_temp$`Location of death (city)`[fe_temp$feID==31114] <- "Geary" # Unknown

fe_temp$`Location of death (city)`[fe_temp$feID==28439] <- "Bothell"

# Highest level of force (becomes cod) ----
fe_temp$`Highest level of force`[fe_temp$feID==22065] <- "Asphyxiated/Restrained" # ketamine
fe_temp$`Highest level of force`[fe_temp$feID==30613] <- "Asphyxiated/Restrained" # ketamine
fe_temp$`Highest level of force`[fe_temp$feID==28269] <- "Asphyxiated/Restrained" # ketamine

# Intended use of force (becomes circumstances -> homicide, suicide, vpursuit) ----
fe_temp$`Intended use of force (Developing)`[fe_temp$feID==28269] <- "Less-than-lethal force"
fe_temp$`Intended use of force (Developing)`[fe_temp$feID==22065] <- "Less-than-lethal force"

  
# Better url ----
fe_temp$`Supporting document link`[fe_temp$feID==18333] <- "https://www.seattleweekly.com/news/seattle-man-fatally-shot-by-kent-police-identified/" # William Stokes
fe_temp$`Supporting document link`[fe_temp$feID==25899] <- "https://katu.com/news/local/sheriffs-office-identifies-man-brian-butts-suspected-of-shooting-killing-cowlitz-county-deputy
" # Brian Butts
fe_temp$`Supporting document link`[fe_temp$feID==31069] <- "https://cdllife.com/2021/disturbing-new-details-released-about-truck-driver-fatally-shot-by-pennsylvania-troopers-at-i-80-rest-stop/" #Nurgazy Mamyrov

fe_temp$`Supporting document link`[fe_temp$feID==28833] <- "https://kimatv.com/news/local/wrong-way-crash-in-i-82-leaves-2-dead-1-injured-in-yakima" #Seth Button
fe_temp$`Supporting document link`[fe_temp$feID==28834] <- "https://kimatv.com/news/local/wrong-way-crash-in-i-82-leaves-2-dead-1-injured-in-yakima" #Neil Sartain
fe_temp$`Supporting document link`[fe_temp$feID==29763] <- "https://www.spokesman.com/stories/2021/mar/09/sheriff-passenger-killed-as-driver-flees-in-grant-/" #Danielle Shockey

fe_temp$`Supporting document link`[fe_temp$feID==14053] <- "https://www.theadvertiser.com/story/news/local/2014/02/08/lincoln-deputy-shoots-kills-23-year-old/5293441/" #Johnny Rico Richardson

# Curtis Elroy Tade (KCME ID, use ST article on KC inquest)
fe_temp$`Supporting document link`[fe_temp$feID==23500] <- "http://komonews.com/news/local/police-man-wielding-assault-rifle-shot-and-killed-by-officer-in-kirkland
"
fe_temp$additional_url[fe_temp$feID==23500] <-"https://www.theadvertiser.com/story/news/local/2014/02/08/lincoln-deputy-shoots-kills-23-year-old/5293441/" #Johnny Rico Richardson

  
# Description ----

## 2 pursuit deaths
fe_temp$`Brief description`[fe_temp$feID==17518] <- "Three men allegedly stole a Honda at gunpoint near 15th Avenue South and South State Street, police said. Three hours later, a Seattle police officer tried to pull over the vehicle at 21st Avenue Southwest and Southwest Webster Street. The driver took off and was speeding until reaching Highland Park Drive Southwest, where the car crossed the centerline and struck an Acura, killing both drivers."
fe_temp$`Brief description`[fe_temp$feID==17519] <- "Three men allegedly stole a Honda at gunpoint near 15th Avenue South and South State Street, police said. Three hours later, a Seattle police officer tried to pull over the vehicle at 21st Avenue Southwest and Southwest Webster Street. The driver took off and was speeding until reaching Highland Park Drive Southwest, where the car crossed the centerline and struck an Acura, killing both drivers."


# Lat Long input errors ----

## WA fixes ----
fe_temp$longitude[fe_temp$feID == 30825] <- -119.896698
fe_temp$latitude[fe_temp$feID == 12731] <- 40.640428
fe_temp$latitude[fe_temp$feID == 28891] <- 42.167834

## CA fixes ----
fe_temp <- fe_temp %>%
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
fe_temp <- fe_temp %>%
  mutate(longitude = case_when(feID==24002 ~ -93.2850266,
                               feID==24001 ~ -90.193529,
                               feID==28136 ~ -90.1141944,
                               TRUE ~ longitude),
         latitude = case_when(feID==24002 ~ 31.1143869,
                              feID==24001 ~ 30.4955241,
                              feID==28136 ~ 29.8974833,
                              TRUE ~ latitude)
  )



# Specific WA fixes with details ----

# Articles say these are suicides, tho officers are there
for(i in c(17338, 26168, 27840)) {
  fe_temp$`Intended use of force (Developing)`[fe_temp$feID==i] <- "Suicide"
}

# Werner Anderson 2018, died after injection of ketamine while held down
# FE classifies Intended use of force as "Undetermined", 
# we will recode as "Other force"
fe_temp$`Intended use of force (Developing)`[fe_temp$feID== 22065] <- "Less lethal force"

# Duplicate case: Alex Martinez 2/27/2011, remove ID 9914 and correct date for ID 9917
# reported to FE, it's now labeled as a dupe but the date hasn't been fixed
fe_temp$date[fe_temp$feID==9917] <- fe_temp$date[fe_temp$feID==9914]
fe_temp$day[fe_temp$feID==9917] <- fe_temp$day[fe_temp$feID==9914]
fe_temp <- fe_temp %>% filter(feID != 9914)

# correct a taser death, see url
fe_temp$`Highest level of force`[fe_temp$feID==25484] <- "Taser"
fe_temp$`Supporting document link`[fe_temp$feID==25484] <- 
  "https://apnews.com/article/5c4465da24a4493085b0bd622a61e253"

# Nancy King
fe_temp$`Foreknowledge of mental illness? INTERNAL USE, NOT FOR ANALYSIS`[fe_temp$feID==29260] <- "Mental Illness"
fe_temp$`Supporting document link`[fe_temp$feID==29260] <- 
  "https://www.washingtonpost.com/nation/2020/12/09/spokane-jail-shooting-nancy-king/"

# better article for Tofte
fe_temp$`Supporting document link`[fe_temp$feID==28883] <- 
  "https://www.kptv.com/news/investigators-id-suspect-shot-killed-by-officer-during-foot-chase-in-longview/article_8fef5874-076f-11eb-8ba3-833f16033386.html"

# Correct agency for Brian Delaan Butts
fe_temp$`Agency or agencies involved`[fe_temp$feID==25899] <- "Kelso Police Department"

