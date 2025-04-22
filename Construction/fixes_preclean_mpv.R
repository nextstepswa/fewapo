# Fixes found during match cleaning
# Sections are titled by the cleaning step that identified the issue
# Matching IDs were reported as fixed (via email from SS 1/29/2024)
# But a check revealed not all had been fixed
# Fixed cases have been commented out, unfixed remain

# ## orig FE matches found ----
# 
# mpv_temp$feID[mpv_temp$mpvID == 11283] == 30220
# mpv_temp$feID[mpv_temp$mpvID == 11285] == 28439
# mpv_temp$feID[mpv_temp$mpvID == 11215] == 28781
# mpv_temp$feID[mpv_temp$mpvID == 11292] == 28856
# mpv_temp$feID[mpv_temp$mpvID == 11216] == 29280
# mpv_temp$feID[mpv_temp$mpvID == 11788] == 31144
# mpv_temp$feID[mpv_temp$mpvID == 10841] == 31429
# 
# 
# ## WaPo matches found ----
# 
# mpv_temp$wapoID[mpv_temp$mpvID == 4361	] == 6928
# mpv_temp$wapoID[mpv_temp$mpvID == 6171	] == 8824
# mpv_temp$wapoID[mpv_temp$mpvID == 6962	] == 5422
# mpv_temp$wapoID[mpv_temp$mpvID == 11216	] == 6664
# mpv_temp$wapoID[mpv_temp$mpvID == 11788	] == 7534
# mpv_temp$wapoID[mpv_temp$mpvID == 8361	] == 6089
# mpv_temp$wapoID[mpv_temp$mpvID == 12389	] == 9912
# mpv_temp$wapoID[mpv_temp$mpvID == 12397	] == 9911
# mpv_temp$wapoID[mpv_temp$mpvID == 12414	] == 9909

## FE dupes ----
## we delete dupes, or reassign if match is wrong

mpv_temp$name[mpv_temp$mpvID == 347] <- "Todd Christopher Jones"

#is.na(mpv_temp$feID[mpv_temp$mpvID == 597])
#any(mpv_temp$mpvID == 676)
#mpv_temp$feID[mpv_temp$mpvID == 811] == 13419
# any(mpv_temp$mpvID == 828)
# any(mpv_temp$mpvID == 889) # dupe of 888
# mpv_temp$feID[mpv_temp$mpvID == 1075] == 28662

mpv_temp$name[mpv_temp$mpvID == 1075] <- "Anthony Darnell King"

# any(mpv_temp$mpvID == 1194)
# mpv_temp$name[mpv_temp$mpvID == 1193] == "Anthony Jamal Bartley" middle missing
# any(mpv_temp$mpvID == 1441)
# mpv_temp$name[mpv_temp$mpvID == 1428] == "Steven Travis Goble" middle missing
# mpv_temp$feID[mpv_temp$mpvID == 3132] == 16939 
# is.na(mpv_temp$feID[mpv_temp$mpvID == 7127])

mpv_temp$feID[mpv_temp$mpvID == 1112] <- 15595   

# mpv_temp$feID[mpv_temp$mpvID == 8627] == 29183
# is.na(mpv_temp$feID[mpv_temp$mpvID == 1020])
# mpv_temp$feID[mpv_temp$mpvID == 5953] == 24592
# mpv_temp$feID[mpv_temp$mpvID == 5953] == 24592
# mpv_temp$date[mpv_temp$mpvID == 6373] == as.Date("2018-11-05")
# any(mpv_temp$mpvID == 7190)
# mpv_temp$feID[mpv_temp$mpvID == 8485] == 28915
# mpv_temp$feID[mpv_temp$mpvID == 8495] == 28997
# mpv_temp$feID[mpv_temp$mpvID == 8490] == 28919
# mpv_temp$feID[mpv_temp$mpvID == 8523] == 28973
# mpv_temp$feID[mpv_temp$mpvID == 8865] == 29661

mpv_temp$feID[mpv_temp$mpvID == 9808] <- 29769

# mpv_temp$feID[mpv_temp$mpvID == 8861] == 29752
# mpv_temp$feID[mpv_temp$mpvID == 9487] == 30798


## WaPo dupes ----

# is.na(mpv_temp$wapoID[mpv_temp$mpvID == 5378])
# mpv_temp$wapoID[mpv_temp$mpvID == 8397] == 6114

mpv_temp$date[mpv_temp$mpvID == 8397] <- as.Date("2020-08-26")
mpv_temp$wapoID[mpv_temp$mpvID == 8927] <- 6546

# is.na(mpv_temp$wapoID[mpv_temp$mpvID == 10858])
# mpv_temp$wapoID[mpv_temp$mpvID == 11862] == 9675
# any(mpv_temp$mpvID == 12305)


## MPV-FE state mismatches ----

# mpv_temp$feID[mpv_temp$mpvID == 676] == 13210 is a dupe
# any(mpv_temp$mpvID == 707)
# mpv_temp$feID[mpv_temp$mpvID == 828] == 13457 is a dupe
# mpv_temp$feID[mpv_temp$mpvID == 1148] == 13986
# mpv_temp$feID[mpv_temp$mpvID == 1888] == 15168
# mpv_temp$feID[mpv_temp$mpvID == 2553] == 16132

mpv_temp$date[mpv_temp$mpvID == 2553] <- as.Date("2015-05-20")

# mpv_temp$feID[mpv_temp$mpvID == 2846] == 16539
# mpv_temp$feID[mpv_temp$mpvID == 3131] == 16940
# mpv_temp$feID[mpv_temp$mpvID == 9216] == 28765
# mpv_temp$feID[mpv_temp$mpvID == 8520] == 28968
# mpv_temp$feID[mpv_temp$mpvID == 8660] == 29243
# mpv_temp$feID[mpv_temp$mpvID == 8863] == 29662
# mpv_temp$wapoID[mpv_temp$mpvID == 8863] == 6687
# mpv_temp$State[mpv_temp$mpvID == 8814] == "AR"
# mpv_temp$feID[mpv_temp$mpvID == 8770] == 29837

mpv_temp$feID[mpv_temp$mpvID == 9078] <- 29967

# mpv_temp$feID[mpv_temp$mpvID == 9250] == 30347
# mpv_temp$feID[mpv_temp$mpvID == 9602] == 31021


## MPV-wapo state mismatches ----

# mpv_temp$wapoID[mpv_temp$mpvID == 5350] == 3213
# mpv_temp$wapoID[mpv_temp$mpvID == 5353] == 3212
# mpv_temp$wapoID[mpv_temp$mpvID == 5351] == 3214

## need to change ORI and any other agency specific info
mpv_temp$`Agency responsible for death`[mpv_temp$mpvID == 6861] <- "Kelso Police Department"
mpv_temp$`ORI Agency Identifier (if available)`[mpv_temp$mpvID == 6861] <- "WA0080100"
#mpv_temp$agency.type[mpv_temp$mpvID == 6861] == "Local Police Department" # own type var from agency

# mpv_temp$wapoID[mpv_temp$mpvID == 6880] == 4649
# mpv_temp$wapoID[mpv_temp$mpvID == 6882] == 5413
# mpv_temp$wapoID[mpv_temp$mpvID == 6881] == 4654
# mpv_temp$wapoID[mpv_temp$mpvID == 6887] == 5414
# mpv_temp$wapoID[mpv_temp$mpvID == 6886] == 4681

mpv_temp$wapoID[mpv_temp$mpvID == 6937] <- 4721
mpv_temp$wapoID[mpv_temp$mpvID == 6938] <- 5418

# mpv_temp$wapoID[mpv_temp$mpvID == 6936] ==5227
# is.na(mpv_temp$wapoID[mpv_temp$mpvID == 7451])
# is.na(mpv_temp$wapoID[mpv_temp$mpvID == 8141])
  
mpv_temp$wapoID[mpv_temp$mpvID == 9272] <- 6951
mpv_temp$State[mpv_temp$mpvID == 11738] <- "NY"

# mpv_temp$State[mpv_temp$mpvID == 12118] == "OK"


## MPV-FE sex mismatches ----

# mpv_temp$feID[mpv_temp$mpvID == 1746] == 14946

mpv_temp$name[mpv_temp$mpvID == 277] <- "Stacey Stout"

# mpv_temp$`Victim's gender`[mpv_temp$mpvID == 277] == "Female"

mpv_temp$name[mpv_temp$mpvID == 6672] <- "Joshua Rembert Williams"

mpv_temp$`Victim's gender`[mpv_temp$mpvID %in% 
                             c(646, 1902, 1584, 1973,
                               3141, 3800, 7531, 8615,
                               11798, 10733, 6628, 8474, 10130, 
                               10280)] <- "Male"

mpv_temp$`Victim's gender`[mpv_temp$mpvID == 5092] <- "Nonbinary"

# MPV-FE Date mismatches (only IDs bad matches for now) ----

#mpv_temp$feID[mpv_temp$mpvID == 8384] == 28632

# 2025 cleaning

## 3/27/25 - sent to SS
# mpv_temp$name[mpv_temp$mpvID == 11293] <- "Rigoberto Brambila-Pelayo"
# mpv_temp$date[mpv_temp$mpvID == 9995] <- as.Date("2022-01-16")
# mpv_temp$date[mpv_temp$mpvID == 10185] <- as.Date("2022-03-15")
# mpv_temp$date[mpv_temp$mpvID == 10512] <- as.Date("2022-06-01")
# mpv_temp$name[mpv_temp$mpvID == 10807] <- "Timothy Green"
# mpv_temp$name[mpv_temp$mpvID == 10859] <- "Vince Torres"
# mpv_temp$name[mpv_temp$mpvID == 11098] <- "Jeffrey Webley"
# mpv_temp$date[mpv_temp$mpvID == 11254] <- as.Date("2022-11-15")
# mpv_temp$date[mpv_temp$mpvID == 11445] <- as.Date("2022-12-17")
# mpv_temp$date[mpv_temp$mpvID == 11518] <- as.Date("2023-03-11")
# mpv_temp$name[mpv_temp$mpvID == 12355] <- "Franklin Ross"
# mpv_temp$date[mpv_temp$mpvID == 13079] <- as.Date("2024-05-23")
# mpv_temp$name[mpv_temp$mpvID == 13891] <- "Alecandro Castaneda"
# mpv_temp$name[mpv_temp$mpvID == 13895] <- "Michael Edward Harris"


# if(mpv_temp$State[mpv_temp$mpvID == 13979] == "WA"){
#   mpv_temp$State[mpv_temp$mpvID == 13979] <- "IA"
#   mpv_temp$`ORI Agency Identifier (if available)`[mpv_temp$mpvID == 13979] <- "IA0770300"
#   message("Des Moines city IA fix for MPV")
# } else {
#   message("\n\n MPV Des Moines city IA fix not needed anymore\n\n")
# }

# from MPV extras vs. FE 2013-2021
mpv_temp$feID[mpv_temp$mpvID == 9853] == 31440 #(MPV: Hebert; FE: UNK, updated in preclean)
