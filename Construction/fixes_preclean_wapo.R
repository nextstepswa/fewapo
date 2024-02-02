# Pre-cleaning fixes for WaPo ---------------------------------------------

## input: wapo_temp
## output wapo_temp

# The cleaning script creates new variables based on these fields
# so the fixes need to be made before the cleaning

# These create merge errors if not fixed, 
# and merging is typically where these problems are first identified

# Many errors were reported to the dataset maintainers,
# but WaPo does not fix

# Errors for all cases (most identified during 2021 EOY merge) fixed first
# Some errors for WA 2015- fixed at end


# Age corrections ----

wapo_temp$age[wapo_temp$wapoID==6646] <- 42
wapo_temp$age[wapo_temp$wapoID==6712] <- 36
wapo_temp$age[wapo_temp$wapoID==6837] <- 50 #both ages reported, obit says 50
wapo_temp$age[wapo_temp$wapoID==6840] <- 34
wapo_temp$age[wapo_temp$wapoID==6478] <- 59
wapo_temp$age[wapo_temp$wapoID==6806] <- 36
wapo_temp$age[wapo_temp$wapoID==1289] <- 47
wapo_temp$age[wapo_temp$wapoID==3198] <- 24
wapo_temp$age[wapo_temp$wapoID==4351] <- 37
wapo_temp$age[wapo_temp$wapoID==4545] <- 43
wapo_temp$age[wapo_temp$wapoID==4967] <- 38
wapo_temp$age[wapo_temp$wapoID==7007] <- 45
wapo_temp$age[wapo_temp$wapoID==7270] <- 39
wapo_temp$age[wapo_temp$wapoID==7299] <- 54
wapo_temp$age[wapo_temp$wapoID==7063] <- 39
wapo_temp$age[wapo_temp$wapoID==7249] <- 41
wapo_temp$age[wapo_temp$wapoID==7366] <- 40
wapo_temp$age[wapo_temp$wapoID==7354] <- 24
wapo_temp$age[wapo_temp$wapoID==7377] <- 28
wapo_temp$age[wapo_temp$wapoID==7415] <- 18
wapo_temp$age[wapo_temp$wapoID==6873] <- 50
wapo_temp$age[wapo_temp$wapoID==6882] <- 25
wapo_temp$age[wapo_temp$wapoID==6900] <- 49
wapo_temp$age[wapo_temp$wapoID==6893] <- 29
wapo_temp$age[wapo_temp$wapoID==6966] <- 74
wapo_temp$age[wapo_temp$wapoID==6989] <- 34
wapo_temp$age[wapo_temp$wapoID==7022] <- 61
wapo_temp$age[wapo_temp$wapoID==7021] <- 32
wapo_temp$age[wapo_temp$wapoID==7009] <- 23
wapo_temp$age[wapo_temp$wapoID==7042] <- 36
wapo_temp$age[wapo_temp$wapoID==7065] <- 38
wapo_temp$age[wapo_temp$wapoID==7309] <- 34
wapo_temp$age[wapo_temp$wapoID==7313] <- 35
wapo_temp$age[wapo_temp$wapoID==7105] <- 26
wapo_temp$age[wapo_temp$wapoID==7128] <- 22
wapo_temp$age[wapo_temp$wapoID==7166] <- 38
wapo_temp$age[wapo_temp$wapoID==7183] <- 50
wapo_temp$age[wapo_temp$wapoID==7186] <- 32
wapo_temp$age[wapo_temp$wapoID==7244] <- 24
wapo_temp$age[wapo_temp$wapoID==7247] <- 45
wapo_temp$age[wapo_temp$wapoID==7221] <- 20
wapo_temp$age[wapo_temp$wapoID==7254] <- 37
wapo_temp$age[wapo_temp$wapoID==7263] <- 52
wapo_temp$age[wapo_temp$wapoID==7268] <- 38
wapo_temp$age[wapo_temp$wapoID==7308] <- 48
wapo_temp$age[wapo_temp$wapoID==7336] <- 33
wapo_temp$age[wapo_temp$wapoID==7355] <- 36
wapo_temp$age[wapo_temp$wapoID==7410] <- 45
wapo_temp$age[wapo_temp$wapoID==9725] <- 27 # WP added late with no age info
wapo_temp$age[wapo_temp$wapoID==9831] <- 43 # article says "born in 1980"

# KCME
wapo_temp$age[wapo_temp$wapoID==8403] <- 54 
wapo_temp$age[wapo_temp$wapoID==3225] <- 47 # KCME Tade


# Race corrections ----
## reported
wapo_temp$race[wapo_temp$wapoID==7063] <- "B"
wapo_temp$race[wapo_temp$wapoID==7249] <- "H"
wapo_temp$race[wapo_temp$wapoID==7252] <- "H"
wapo_temp$race[wapo_temp$wapoID==7207] <- "B"
wapo_temp$race[wapo_temp$wapoID==7366] <- "H"
wapo_temp$race[wapo_temp$wapoID==7377] <- "H"
wapo_temp$race[wapo_temp$wapoID==7415] <- "B"

wapo_temp$race[wapo_temp$wapoID==6870] <- "H"
wapo_temp$race[wapo_temp$wapoID==6893] <- "W"
wapo_temp$race[wapo_temp$wapoID==7021] <- "H"
wapo_temp$race[wapo_temp$wapoID==7009] <- "H"
wapo_temp$race[wapo_temp$wapoID==7084] <- "H"
wapo_temp$race[wapo_temp$wapoID==7313] <- "W"
wapo_temp$race[wapo_temp$wapoID==7166] <- "B"
wapo_temp$race[wapo_temp$wapoID==7156] <- "W"
wapo_temp$race[wapo_temp$wapoID==7277] <- "W"
wapo_temp$race[wapo_temp$wapoID==7268] <- "W"
wapo_temp$race[wapo_temp$wapoID==7244] <- "W"
wapo_temp$race[wapo_temp$wapoID==7247] <- "W"
wapo_temp$race[wapo_temp$wapoID==7221] <- "H"
wapo_temp$race[wapo_temp$wapoID==7216] <- "W"
wapo_temp$race[wapo_temp$wapoID==7254] <- "B"
wapo_temp$race[wapo_temp$wapoID==7263] <- "W"
wapo_temp$race[wapo_temp$wapoID==7308] <- "W"
wapo_temp$race[wapo_temp$wapoID==7336] <- "B"
wapo_temp$race[wapo_temp$wapoID==7355] <- "B"
wapo_temp$race[wapo_temp$wapoID==7372] <- "H"
wapo_temp$race[wapo_temp$wapoID==7381] <- "W"
wapo_temp$race[wapo_temp$wapoID==7410] <- "H"

# KCME
wapo_temp$race[wapo_temp$wapoID==3225] <- "W" # Curtis Tade
wapo_temp$race[wapo_temp$wapoID==7341] <- "W" # Alexander Whittall
wapo_temp$race[wapo_temp$wapoID==8403] <- "H" # Vince Torres


# Gender corrections ----
# note orig wapo v2 gender varname is not capitalized

wapo_temp$gender[wapo_temp$wapoID %in% 
              c(9904, 8208, 8119)] <- "Female"

wapo_temp$gender[wapo_temp$wapoID %in% 
              c(7007, 7107, 7299, 7249,
                7354, 7415, 8649, 8510, 
                8574, 8103, 8109, 7947,
                8871, 9296, 8563, 7683, 
                7970, 7965, 8006, 8324, 
                8343, 8459, 8494, 8503, 
                8629, 8712, 8746, 8769, 
                8801, 8881, 9148, 9363,
                9725)] <- "Male"

wapo_temp$gender[wapo_temp$wapoID == 2956] <- "Nonbinary"

wapo_temp$gender[wapo_temp$wapoID %in% 
              c(9057, 6887, 8202, 1236, 2197, 5843)] <- "Transgender man"

wapo_temp$gender[wapo_temp$wapoID %in% 
              c(5798, 4453, 2887, 263, 4608, 8042)] <- "Transgender woman"


# Name corrections ----

wapo_temp$name[wapo_temp$wapoID==1778] <- "Jeffrey Martelli" # not reported
wapo_temp$name[wapo_temp$wapoID==3198] <- "Kyle Gray" # reported
wapo_temp$name[wapo_temp$wapoID==6989] <- "Dwayne Michael Fields" #reported
wapo_temp$name[wapo_temp$wapoID==4967] <- "Collin Osborn" # reported

wapo_temp$name[wapo_temp$wapoID==6483] <- "Isaac Matheney"
wapo_temp$name[wapo_temp$wapoID==6643] <- "Frederick Hight"
wapo_temp$name[wapo_temp$wapoID==6806] <- "Brian DeLeon"

wapo_temp$name[wapo_temp$wapoID==6870] <- "Raul Rosas Zarsosa"
wapo_temp$name[wapo_temp$wapoID==6882] <- "Zaekwon Gullatte"
wapo_temp$name[wapo_temp$wapoID==7007] <- "Edward Daniel Santana"
wapo_temp$name[wapo_temp$wapoID==7179] <- "Matthew Joseph Wilbanks"
wapo_temp$name[wapo_temp$wapoID==7249] <- "Adrian Zarate Cervantes"
wapo_temp$name[wapo_temp$wapoID==7277] <- "Craig Allen Knutson"
wapo_temp$name[wapo_temp$wapoID==7366] <- "Wendy Carolina Flores De Roque"
wapo_temp$name[wapo_temp$wapoID==7377] <- "Christean Ann Dimas"
wapo_temp$name[wapo_temp$wapoID==7415] <- "Donta R. Stewart"
wapo_temp$name[wapo_temp$wapoID==7009] <- "Arcadio Castillo"
wapo_temp$name[wapo_temp$wapoID==7042] <- "Quention Cantreal Bogard"
wapo_temp$name[wapo_temp$wapoID==7084] <- "Johan Alexis Salazar"
wapo_temp$name[wapo_temp$wapoID==7308] <- "Gary Wayne Bressler"
wapo_temp$name[wapo_temp$wapoID==7381] <- "Noah Douglas Kelley"
wapo_temp$name[wapo_temp$wapoID==8447] <- "Terris Vincent Hetland"
wapo_temp$name[wapo_temp$wapoID==8346] <- "Timothy Green"
wapo_temp$name[wapo_temp$wapoID==8310] <- "Dominic A. Shears"
wapo_temp$name[wapo_temp$wapoID==7765] <- "Murdock J. Phillips"
wapo_temp$name[wapo_temp$wapoID==6689] <- "James Wright"
wapo_temp$name[wapo_temp$wapoID==6790] <- "Ma'Khia Bryant"
wapo_temp$name[wapo_temp$wapoID==8582] <- "Name Notknown"
wapo_temp$name[wapo_temp$wapoID == 9415] <- "Cody Kuzior"
wapo_temp$name[wapo_temp$wapoID == 6235] <- "Richard Romero"
wapo_temp$name[wapo_temp$wapoID == 7120] <- "Setha Phangdy"
wapo_temp$name[wapo_temp$wapoID == 8927] <- "Steven Emmet Crosby"
wapo_temp$name[wapo_temp$wapoID == 6234] <- "George Ludrou Job"

wapo_temp$name[wapo_temp$wapoID == 4453] <- "Joshua Rembert Williams"


# KCME
wapo_temp$name[wapo_temp$wapoID == 3225] <- "Curtis Elroy Tade"
wapo_temp$name[wapo_temp$wapoID == 7341] <- "Alexander Whittall"
wapo_temp$name[wapo_temp$wapoID == 8403] <- "Vince Torres"


# Date corrections (name, format pre-fixed in ScrapeMerge) ----

wapo_temp$date[wapo_temp$wapoID==5778] <- as.Date("2020-04-29") # reported
wapo_temp$date[wapo_temp$wapoID==9021] <- as.Date("2023-03-11")

wapo_temp$date[wapo_temp$wapoID==6589] <- as.Date("2020-08-14") # Aleksandr Rusanovskiy
wapo_temp$date[wapo_temp$wapoID==6478] <- as.Date("2021-01-03") # James Reising
wapo_temp$date[wapo_temp$wapoID==6817] <- as.Date("2021-05-01") # Hanad A. Abdiaziz
wapo_temp$date[wapo_temp$wapoID==6853] <- as.Date("2017-09-17") # Demilo Hodge
wapo_temp$date[wapo_temp$wapoID==7030] <- as.Date("2021-07-16") # Gerardo Martinez
wapo_temp$date[wapo_temp$wapoID==6900] <- as.Date("2021-05-30") # Roberto Zielinski
wapo_temp$date[wapo_temp$wapoID==7309] <- as.Date("2021-08-06") # Hayden  McIlvain
wapo_temp$date[wapo_temp$wapoID==5224] <- as.Date("2019-11-25") # Anthony Chilcott
wapo_temp$date[wapo_temp$wapoID==1964] <- as.Date("2016-10-12") # Patrick D. Reddeck
wapo_temp$date[wapo_temp$wapoID==1156] <- as.Date("2016-01-09") # David Jay Kent
wapo_temp$date[wapo_temp$wapoID==484] <- as.Date("2015-05-20") # MPV match

# State fixes ----

wapo_temp$state[wapo_temp$wapoID==4934] <- "KS"
wapo_temp$state[wapo_temp$wapoID==4655] <- "WA"
wapo_temp$state[wapo_temp$wapoID==1874] <- "AR"
wapo_temp$state[wapo_temp$wapoID==6469] <- "ID"


# County fixes ----
wapo_temp$county[wapo_temp$wapoID==1145] <- "Thurston"
wapo_temp$county[wapo_temp$wapoID==3026] <- "Thurston"
wapo_temp$county[wapo_temp$wapoID==8313] <- "Yakima"
wapo_temp$county[wapo_temp$wapoID==9332] <- "Adams"


# City fixes ----

wapo_temp$city[wapo_temp$wapoID==8766] <- "Olympia" # Neil Costin
wapo_temp$city[wapo_temp$wapoID==8594] <- "Sunnydale" # ?
wapo_temp$city[wapo_temp$wapoID==8510] <- "Loon Lake" # Jeffrey Smith
wapo_temp$city[wapo_temp$wapoID==1084] <- "Olympia" # Nephi  Leiataua
wapo_temp$city[wapo_temp$wapoID==1145] <- "Tumwater" # Joel Nelson
wapo_temp$city[wapo_temp$wapoID==1623] <- "Ridgefield" # Kenneth Pointer
wapo_temp$city[wapo_temp$wapoID==1988] <- "Muckelshoot Reservation" # Renee Davis
wapo_temp$city[wapo_temp$wapoID==2502] <- "Frederickson" # Charles Shands
wapo_temp$city[wapo_temp$wapoID==2750] <- "West Richland" # Douglas West
wapo_temp$city[wapo_temp$wapoID==3170] <- "Spokane" # James Danforth, Hillyard verified a neighborhood of Spokane
wapo_temp$city[wapo_temp$wapoID==3332] <- "Everett" # Ilkka  Hiironen, Ebey Island verified in Everett
wapo_temp$city[wapo_temp$wapoID==3891] <- "Kent" # Jesus Murillo
wapo_temp$city[wapo_temp$wapoID==3980] <- "Spokane Valley" # Bryan Bayne
wapo_temp$city[wapo_temp$wapoID==4133] <- "Martha Lake" # Nickolas Peters
wapo_temp$city[wapo_temp$wapoID==4545] <- "Hazel Dell South" # Carlos Hunter
wapo_temp$city[wapo_temp$wapoID==4708] <- "Spokane Valley" # Ethan Murray
wapo_temp$city[wapo_temp$wapoID==5194] <- "Mesa" # Dante Jones
wapo_temp$city[wapo_temp$wapoID==5224] <- "Enumclaw" # Anthony Chilcott
wapo_temp$city[wapo_temp$wapoID==6175] <- "Spokane Valley" # Joshua Brant
wapo_temp$city[wapo_temp$wapoID==6199] <- "Spokane" # Erik Mahoney
wapo_temp$city[wapo_temp$wapoID==6305] <- "Woodinville" # Ronny Dunning
wapo_temp$city[wapo_temp$wapoID==4655] <- "Kalama"
wapo_temp$city[wapo_temp$wapoID==1874] <- "West Memphis"
wapo_temp$city[wapo_temp$wapoID==8313] <- "Zillah"
wapo_temp$city[wapo_temp$wapoID==4655] <- "Kalama"
wapo_temp$city[wapo_temp$wapoID==3164] <- "Tieton"
wapo_temp$city[wapo_temp$wapoID==6546] <- "Hemet"
 





                




