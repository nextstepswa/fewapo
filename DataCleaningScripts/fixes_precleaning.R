# Pre-cleaning fixes for all cases ---------------------------------------------

# These create merge errors if not fixed, 
# and merging is typically where these problems are first identified
# The cleaning often creates new variables based on these
# fields, so the fixes should be made before the cleaning

# Errors are reported to the dataset maintainers (so should be checked periodically to see if no longer needed)
# Other differences are not reported, but are fixed to facilitate merging
# Most were identified during 2021 EOY merge, still need to do 2015-2020


# Age corrections ----
## reported

wapo$age[wapo$id==6646] <- 42
wapo$age[wapo$id==6712] <- 36
wapo$age[wapo$id==6837] <- 50 #both ages reported, obit says 50
wapo$age[wapo$id==6840] <- 34
wapo$age[wapo$id==6478] <- 59
wapo$age[wapo$id==6806] <- 36


wapo$age[wapo$id==1289] <- 47
wapo$age[wapo$id==3198] <- 24
wapo$age[wapo$id==4351] <- 37
wapo$age[wapo$id==4545] <- 43
wapo$age[wapo$id==4967] <- 38
wapo$age[wapo$id==7007] <- 45
wapo$age[wapo$id==7270] <- 39
wapo$age[wapo$id==7299] <- 54
wapo$age[wapo$id==7063] <- 39
wapo$age[wapo$id==7249] <- 41
wapo$age[wapo$id==7366] <- 40
wapo$age[wapo$id==7354] <- 24
wapo$age[wapo$id==7377] <- 28
wapo$age[wapo$id==7415] <- 18
wapo$age[wapo$id==6873] <- 50
wapo$age[wapo$id==6882] <- 25
wapo$age[wapo$id==6900] <- 49
wapo$age[wapo$id==6893] <- 29
wapo$age[wapo$id==6966] <- 74
wapo$age[wapo$id==6989] <- 34
wapo$age[wapo$id==7022] <- 61
wapo$age[wapo$id==7021] <- 32
wapo$age[wapo$id==7009] <- 23
wapo$age[wapo$id==7042] <- 36
wapo$age[wapo$id==7065] <- 38
wapo$age[wapo$id==7309] <- 34
wapo$age[wapo$id==7313] <- 35
wapo$age[wapo$id==7105] <- 26
wapo$age[wapo$id==7128] <- 22
wapo$age[wapo$id==7166] <- 38
wapo$age[wapo$id==7183] <- 50
wapo$age[wapo$id==7186] <- 32
wapo$age[wapo$id==7244] <- 24
wapo$age[wapo$id==7247] <- 45
wapo$age[wapo$id==7221] <- 20
wapo$age[wapo$id==7254] <- 37
wapo$age[wapo$id==7263] <- 52
wapo$age[wapo$id==7268] <- 38
wapo$age[wapo$id==7308] <- 48
wapo$age[wapo$id==7336] <- 33
wapo$age[wapo$id==7355] <- 36
wapo$age[wapo$id==7410] <- 45

fe$Age[fe$`Unique ID`==23382] <- 26
fe$Age[fe$`Unique ID`==31256] <- 24
fe$Age[fe$`Unique ID`==31274] <- 28
fe$Age[fe$`Unique ID`==30149] <- 57
fe$Age[fe$`Unique ID`==30297] <- 39
fe$Age[fe$`Unique ID`==30351] <- 27
fe$Age[fe$`Unique ID`==30466] <- 31
fe$Age[fe$`Unique ID`==30550] <- 40
fe$Age[fe$`Unique ID`==30837] <- 53
fe$Age[fe$`Unique ID`==30890] <- 53
fe$Age[fe$`Unique ID`==31127] <- 40
fe$Age[fe$`Unique ID`==31180] <- 37
fe$Age[fe$`Unique ID`==31265] <- 27
fe$Age[fe$`Unique ID`==31263] <- 36





# Gender corrections ----
## reported
fe$Gender[fe$`Unique ID`==29700] <- "Female"
fe$Gender[fe$`Unique ID`==30780] <- "Female"

wapo$gender[wapo$id==7007] <- "Male"
wapo$gender[wapo$id==7107] <- "Male"
wapo$gender[wapo$id==7299] <- "Male"
wapo$gender[wapo$id==7249] <- "Male"
wapo$gender[wapo$id==7354] <- "Male"
wapo$gender[wapo$id==7415] <- "Male"
wapo$gender[wapo$id==8649] <- "Male"

## not reported
wapo$gender[wapo$id==8510] <- "Male" # Jeffrey Smith
wapo$gender[wapo$id==6887] <- "Transgender"
fe$Gender[fe$`Unique ID`==27067] <- "Male"


# Race corrections ----
## reported
wapo$race[wapo$id==7063] <- "B"
wapo$race[wapo$id==7249] <- "H"
wapo$race[wapo$id==7252] <- "H"
wapo$race[wapo$id==7207] <- "B"
wapo$race[wapo$id==7366] <- "H"
wapo$race[wapo$id==7377] <- "H"
wapo$race[wapo$id==7415] <- "B"

wapo$race[wapo$id==6870] <- "H"
wapo$race[wapo$id==6893] <- "W"
wapo$race[wapo$id==7021] <- "H"
wapo$race[wapo$id==7009] <- "H"
wapo$race[wapo$id==7084] <- "H"
wapo$race[wapo$id==7313] <- "W"
wapo$race[wapo$id==7166] <- "B"
wapo$race[wapo$id==7156] <- "W"
wapo$race[wapo$id==7277] <- "W"
wapo$race[wapo$id==7268] <- "W"
wapo$race[wapo$id==7244] <- "W"
wapo$race[wapo$id==7247] <- "W"
wapo$race[wapo$id==7221] <- "H"
wapo$race[wapo$id==7216] <- "W"
wapo$race[wapo$id==7254] <- "B"
wapo$race[wapo$id==7263] <- "W"
wapo$race[wapo$id==7308] <- "W"
wapo$race[wapo$id==7336] <- "B"
wapo$race[wapo$id==7355] <- "B"
wapo$race[wapo$id==7372] <- "H"
wapo$race[wapo$id==7381] <- "W"
wapo$race[wapo$id==7410] <- "H"

fe$Race[fe$`Unique ID`==29443] <- "European-American/White"
fe$`Race with imputations`[fe$`Unique ID`==29443] <- "European-American/White"
fe$Race[fe$`Unique ID`==30199] <- "African-American/Black"
fe$`Race with imputations`[fe$`Unique ID`==30199] <- "African-American/Black"
fe$Race[fe$`Unique ID`==31117] <- "African-American/Black"
fe$`Race with imputations`[fe$`Unique ID`==31117] <- "African-American/Black"
fe$Race[fe$`Unique ID`==30271] <- "Hispanic/Latino"
fe$`Race with imputations`[fe$`Unique ID`==30271] <- "Hispanic/Latino"
fe$Race[fe$`Unique ID`==30348] <- "European-American/White"
fe$`Race with imputations`[fe$`Unique ID`==30348] <- "European-American/White"
fe$Race[fe$`Unique ID`==30751] <- "Hispanic/Latino"
fe$`Race with imputations`[fe$`Unique ID`==30751] <- "Hispanic/Latino"



# Name corrections ----
## reported

wapo$name[wapo$id==6483] <- "Isaac Matheney"
wapo$name[wapo$id==6643] <- "Frederick Hight"
wapo$name[wapo$id==6806] <- "Brian DeLeon"


wapo$name[wapo$id==6870] <- "Raul Rosas Zarsosa"
wapo$name[wapo$id==6882] <- "Zaekwon Gullatte"
wapo$name[wapo$id==7007] <- "Edward Daniel Santana"
wapo$name[wapo$id==7179] <- "Matthew Joseph Wilbanks"
wapo$name[wapo$id==7249] <- "Adrian Zarate Cervantes"
wapo$name[wapo$id==7277] <- "Craig Allen Knutson"
wapo$name[wapo$id==7366] <- "Wendy Carolina Flores De Roque"
wapo$name[wapo$id==7377] <- "Christean Ann Dimas"
wapo$name[wapo$id==7415] <- "Donta R. Stewart"
wapo$name[wapo$id==7009] <- "Arcadio Castillo"
wapo$name[wapo$id==7042] <- "Quention Cantreal Bogard"
wapo$name[wapo$id==7084] <- "Johan Alexis Salazar"
wapo$name[wapo$id==7308] <- "Gary Wayne Bressler"
wapo$name[wapo$id==7381] <- "Noah Douglas Kelley"



fe$Name[fe$`Unique ID`==29496] <- "Kershawn Geyger"
#fe$Name[fe$`Unique ID`==31117] <- "Jermaine Anthony Harris"
#fe$Name[fe$`Unique ID`==30271] <- "Raul Rosas Zarose"
#fe$Name[fe$`Unique ID`==30625] <- "Quention Cantreal Bogard"
fe$Name[fe$`Unique ID`==30829] <- "Kevin Victor Johnson"
fe$Name[fe$`Unique ID`==30775] <- "Steven Dean Primm"
 


## not reported
wapo$name[wapo$id==8447] <- "Terris Vincent Hetland"
wapo$name[wapo$id==8346] <- "Timothy Green"
wapo$name[wapo$id==8310] <- "Dominic A. Shears"
wapo$name[wapo$id==7765] <- "Murdock J. Phillips"
wapo$name[wapo$id==6689] <- "James Wright"
wapo$name[wapo$id==6790] <- "Ma'Khia Bryant"

fe$Name[fe$`Unique ID`==29443] <- "Jacob Ryan Macduff"
fe$Name[fe$`Unique ID`==30280] <- "Zaekwon Malik Gullatte"
fe$Name[fe$`Unique ID`==30410] <- "Christopher M. VanKleeck"
fe$Name[fe$`Unique ID`==30487] <- "Hunter Brittain"
fe$Name[fe$`Unique ID`==30619] <- "Gerardo Chavez Martinez"
fe$Name[fe$`Unique ID`==30718] <- "Aler Ronald Velasquez Escobar"
fe$Name[fe$`Unique ID`==30748] <- "Luis Manuel Arias Garcia"
fe$Name[fe$`Unique ID`==30865] <- "Alexander Domina"
fe$Name[fe$`Unique ID`==30886] <- "Jose Angel Francisco Baca"
fe$Name[fe$`Unique ID`==31002] <- "Irlin Marcloni Cabal Paz"
fe$Name[fe$`Unique ID`==30875] <- "Omar Lazano Hernandez"
fe$Name[fe$`Unique ID`==31069] <- "Nurgazy Mamyrov"




# Date corrections

## not reported
fe$`Date of injury resulting in death (month/day/year)`[fe$`Unique ID`==29416] <- "12/31/2020"
fe$`Date of injury resulting in death (month/day/year)`[fe$`Unique ID`==29677] <- "2/15/2021"
fe$`Date of injury resulting in death (month/day/year)`[fe$`Unique ID`==30299] <- "5/23/2021"

wapo$date[wapo$id==6589] <- lubridate::ymd("2020-08-14") # Aleksandr Rusanovskiy



## reported 

wapo$date[wapo$id==6478] <- lubridate::ymd("2021-01-03") # James Reising
wapo$date[wapo$id==6817] <- lubridate::ymd("2021-05-01") # Hanad A. Abdiaziz
wapo$date[wapo$id==6853] <- lubridate::ymd("2017-09-17") # Demilo Hodge
wapo$date[wapo$id==7030] <- lubridate::ymd("2021-07-16") # Gerardo Martinez

wapo$date[wapo$id==6900] <- lubridate::ymd("2021-05-30") # Roberto Zielinski
wapo$date[wapo$id==7309] <- lubridate::ymd("2021-08-06") # Hayden  McIlvain

wapo$date[wapo$id==5224] <- lubridate::ymd("2019-11-25") # Anthony Chilcott

wapo$date[wapo$id==1964] <- lubridate::ymd("2016-10-12") # Patrick D. Reddeck
wapo$date[wapo$id==1156] <- lubridate::ymd("2016-01-09") # David Jay Kent

fe$`Date of injury resulting in death (month/day/year)`[fe$`Unique ID`==30199] <- "05/08/2021" # Everton Garfield Brown
fe$`Date of injury resulting in death (month/day/year)`[fe$`Unique ID`==12757] <- "05/07/2013" # Danny Valdes
fe$`Date of injury resulting in death (month/day/year)`[fe$`Unique ID`==14290] <- "04/03/2014" # Dustin Glover



# COD fixes
fe$`Highest level of force`[fe$`Unique ID`==30199] <- "Gunshot"

# State fixes
fe$State[fe$`Unique ID`==30388] <- "GA"



# County fixes
fe$`Location of death (county)`[fe$`Unique ID`==30388] <- "Muscogee"
wapo$county[wapo$id==1145] <- "Thurston"
wapo$county[wapo$id==3026] <- "Thurston"



# City fixes
wapo$city[wapo$id==8766] <- "Olympia" # Neil Costin
wapo$city[wapo$id==8594] <- "Sunnydale" # ?
wapo$city[wapo$id==8510] <- "Loon Lake" # Jeffrey Smith
wapo$city[wapo$id==1084] <- "Olympia" # Nephi  Leiataua
wapo$city[wapo$id==1145] <- "Tumwater" # Joel Nelson
wapo$city[wapo$id==1623] <- "Ridgefield" # Kenneth Pointer

wapo$city[wapo$id==1988] <- "Muckelshoot Reservation" # Renee Davis
fe$`Location of death (city)`[fe$`Unique ID`==18333] <- "Muckelshoot Reservation" # Renee Davis

fe$`Location of death (city)`[fe$`Unique ID`==19583] <- "Des Moines" # William Stokes


wapo$city[wapo$id==2502] <- "Frederickson" # Charles Shands
fe$`Location of death (city)`[fe$`Unique ID`==19831] <- "Frederickson" # Charles Shands


wapo$city[wapo$id==2750] <- "West Richland" # Douglas West

# google maps is wrong, Fredrickson is a city
# https://zipmap.net/Washington/Pierce_County/Frederickson.htm
fe$`Location of death (city)`[fe$`Unique ID`==21885] <- "Frederickson" # Eduardo Navarrete

wapo$city[wapo$id==3170] <- "Spokane" # James Danforth, Hillyard verified a neighborhood of Spokane
wapo$city[wapo$id==3332] <- "Everett" # Ilkka  Hiironen, Ebey Island verified in Everett

fe$`Location of death (city)`[fe$`Unique ID`==23934] <- "Elk Plain" # William Langfitt, EP verified as Census designated place

wapo$city[wapo$id==3891] <- "Kent" # Jesus Murillo
wapo$city[wapo$id==3980] <- "Spokane Valley" # Bryan Bayne
wapo$city[wapo$id==4133] <- "Martha Lake" # Nickolas Peters
wapo$city[wapo$id==4545] <- "Hazel Dell South" # Carlos Hunter
wapo$city[wapo$id==4708] <- "Spokane Valley" # Ethan Murray
wapo$city[wapo$id==5194] <- "Mesa" # Dante Jones
wapo$city[wapo$id==5224] <- "Enumclaw" # Anthony Chilcott

fe$`Location of death (city)`[fe$`Unique ID`==27321] <- "Spokane" # Clando Anitok
fe$`Location of death (city)`[fe$`Unique ID`==27830] <- "Eatonville" # Brandon Stokes
fe$`Location of death (city)`[fe$`Unique ID`==27954] <- "Seattle" # Shaun Fuhr

wapo$city[wapo$id==6175] <- "Spokane Valley" # Joshua Brant
wapo$city[wapo$id==6199] <- "Spokane" # Erik Mahoney

fe$`Location of death (city)`[fe$`Unique ID`==29038] <- "Hazel Dell" # Kevin Peterson
fe$`Location of death (city)`[fe$`Unique ID`==31114] <- "Geary" # Unknown


wapo$city[wapo$id==6305] <- "Woodinville" # Ronny Dunning


# Latitude (removes trailing comma)
fe$Latitude[fe$`Unique ID`==28891] <- 42.167834

# Highest level of force (becomes cod)
fe$`Highest level of force`[fe$`Unique ID`==22065] <- "Asphyxiated/Restrained" # ketamine
fe$`Highest level of force`[fe$`Unique ID`==30613] <- "Asphyxiated/Restrained" # ketamine
fe$`Highest level of force`[fe$`Unique ID`==28269] <- "Asphyxiated/Restrained" # ketamine

# Intended use of force (becomes circumstances -> homicide, suicide, vpursuit)
fe$`Intended use of force (Developing)`[fe$`Unique ID`==28269] <- "Less-than-lethal force"
fe$`Intended use of force (Developing)`[fe$`Unique ID`==22065] <- "Less-than-lethal force"

  
# Better url
fe$`Supporting document link`[fe$`Unique ID`==18333] <- "https://www.seattleweekly.com/news/seattle-man-fatally-shot-by-kent-police-identified/" # William Stokes
fe$`Supporting document link`[fe$`Unique ID`==25899] <- "https://katu.com/news/local/sheriffs-office-identifies-man-brian-butts-suspected-of-shooting-killing-cowlitz-county-deputy
" # Brian Butts
fe$`Supporting document link`[fe$`Unique ID`==31069] <- "https://cdllife.com/2021/disturbing-new-details-released-about-truck-driver-fatally-shot-by-pennsylvania-troopers-at-i-80-rest-stop/" #Nurgazy Mamyrov

fe$`Supporting document link`[fe$`Unique ID`==28833] <- "https://kimatv.com/news/local/wrong-way-crash-in-i-82-leaves-2-dead-1-injured-in-yakima" #Seth Button
fe$`Supporting document link`[fe$`Unique ID`==28834] <- "https://kimatv.com/news/local/wrong-way-crash-in-i-82-leaves-2-dead-1-injured-in-yakima" #Neil Sartain
fe$`Supporting document link`[fe$`Unique ID`==29763] <- "https://www.spokesman.com/stories/2021/mar/09/sheriff-passenger-killed-as-driver-flees-in-grant-/" #Danielle Shockey



# Description
fe$fe$`Brief description` <- "Three men allegedly stole a Honda at gunpoint near 15th Avenue South and South State Street, police said. Three hours later, a Seattle police officer tried to pull over the vehicle at 21st Avenue Southwest and Southwest Webster Street. The driver took off and was speeding until reaching Highland Park Drive Southwest, where the car crossed the centerline and struck an Acura, killing both drivers."






# WA fixes ------------------------------------------------
## Identified for all years, we can incorporate above when all
## cases have been merged

fe$Name[fe$`Unique ID`==29605] <- "Jenoah Donald"
fe$Name[fe$`Unique ID`==23531] <- "Joshua Spottedhorse"
fe$Name[fe$`Unique ID`==18509] <- "Bruce R. Johnson"
fe$Name[fe$`Unique ID`==16382] <- "Samuel Toshiro Smith"
fe$Name[fe$`Unique ID`==28826] <- "Andrea Churna"
fe$`Location of death (city)`[fe$`Unique ID`==28439] <- "Bothell"

#fe$`Intended use of force (Developing)`[fe$`Unique ID`==15941] <- "Veh/Purs"

# Articles say these are suicides, tho officers are there
for(i in c(17338, 26168, 27840)) {
  fe$`Intended use of force (Developing)`[fe$`Unique ID`==i] <- "Suicide"
}

wapo$name[wapo$id==1778] <- "Jeffrey Martelli" # not reported

wapo$name[wapo$id==3198] <- "Kyle Gray" # reported
wapo$name[wapo$id==6989] <- "Dwayne Michael Fields" #reported
wapo$name[wapo$id==4967] <- "Collin Osborn" # reported
                
wapo$date[wapo$id==5778] <- as.Date("2020-04-29") # reported
wapo$date[wapo$id==9021] <- as.Date("2023-03-11")

wapo$city[wapo$id==3164] <- "Tieton"
