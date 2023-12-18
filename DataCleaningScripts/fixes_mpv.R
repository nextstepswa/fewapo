# Fixes found during match cleaning
# Sections are titled by the cleaning step that identified the issue

## orig FE matches found ----

mpv_temp$feID[mpv_temp$mpvID == 11283] <- 30220
mpv_temp$feID[mpv_temp$mpvID == 11285] <- 28439
mpv_temp$feID[mpv_temp$mpvID == 11215] <- 28781
mpv_temp$feID[mpv_temp$mpvID == 11292] <- 28856
mpv_temp$feID[mpv_temp$mpvID == 11216] <- 29280
mpv_temp$feID[mpv_temp$mpvID == 11788] <- 31144
mpv_temp$feID[mpv_temp$mpvID == 10841] <- 31429


## WaPo matches found ----

mpv_temp$wapoID[mpv_temp$mpvID == 4361	] <- 6928
mpv_temp$wapoID[mpv_temp$mpvID == 6171	] <- 8824
mpv_temp$wapoID[mpv_temp$mpvID == 6962	] <- 5422
mpv_temp$wapoID[mpv_temp$mpvID == 11216	] <- 6664
mpv_temp$wapoID[mpv_temp$mpvID == 11788	] <- 7534
mpv_temp$wapoID[mpv_temp$mpvID == 8361	] <- 6089
mpv_temp$wapoID[mpv_temp$mpvID == 12389	] <- 9912
mpv_temp$wapoID[mpv_temp$mpvID == 12397	] <- 9911
mpv_temp$wapoID[mpv_temp$mpvID == 12414	] <- 9909

## FE dupes ----
## we delete dupes, or reassign if match is wrong

mpv_temp$name[mpv_temp$mpvID == 343] <- "Todd Christopher Jones"
mpv_temp <- mpv_temp %>% filter(mpvID != 347)

mpv_temp <- mpv_temp %>% filter(mpvID != 373)

mpv_temp$feID[mpv_temp$mpvID == 597] <- NA

mpv_temp <- mpv_temp %>% filter(mpvID != 676)

mpv_temp$feID[mpv_temp$mpvID == 811] <- 13419

mpv_temp <- mpv_temp %>% filter(mpvID != 828)

mpv_temp <- mpv_temp %>% filter(mpvID != 889) # dupe of 888

mpv_temp$feID[mpv_temp$mpvID == 1075] <- 28662
mpv_temp$name[mpv_temp$mpvID == 343] <- "Anthony Darnell King"

mpv_temp <- mpv_temp %>% filter(mpvID != 1194)
mpv_temp$name[mpv_temp$mpvID == 1193] <- "Anthony Jamal Bartley"

mpv_temp <- mpv_temp %>% filter(mpvID != 1441)
mpv_temp$name[mpv_temp$mpvID == 1428] <- "Steven Travis Goble"

mpv_temp$feID[mpv_temp$mpvID == 3132] <- 16939 

mpv_temp$feID[mpv_temp$mpvID == 7127] <- NA

mpv_temp <- mpv_temp %>% filter(mpvID != 1112)

mpv_temp$feID[mpv_temp$mpvID == 8627] <- 29183

mpv_temp$feID[mpv_temp$mpvID == 1020] <- NA

mpv_temp$feID[mpv_temp$mpvID == 5953] <- 24592
mpv_temp$wapoID[mpv_temp$mpvID == 5953] <- NA

mpv_temp$date[mpv_temp$mpvID == 6373] <- as.Date("2018-11-05")
mpv_temp <- mpv_temp %>% filter(mpvID != 7190)

mpv_temp$feID[mpv_temp$mpvID == 8485] <- 28915

mpv_temp$feID[mpv_temp$mpvID == 8495] <- 28997

mpv_temp$feID[mpv_temp$mpvID == 8490] <- 28919

mpv_temp$feID[mpv_temp$mpvID == 8523] <- 28973

mpv_temp$feID[mpv_temp$mpvID == 8865] <- 29661

mpv_temp$feID[mpv_temp$mpvID == 9808] <- 29769

mpv_temp$feID[mpv_temp$mpvID == 8861] <- 29752

mpv_temp$feID[mpv_temp$mpvID == 9487] <- 30798


## WaPo dupes ----

mpv_temp$wapoID[mpv_temp$mpvID == 5378] <- NA

mpv_temp$wapoID[mpv_temp$mpvID == 8397] <- 6114
mpv_temp$date[mpv_temp$mpvID == 8397] <- as.Date("2020-08-26")

mpv_temp$wapoID[mpv_temp$mpvID == 8927] <- 6546

mpv_temp$wapoID[mpv_temp$mpvID == 10858] <- NA

mpv_temp$wapoID[mpv_temp$mpvID == 11862] <- 9675

mpv_temp <- mpv_temp %>% filter(mpvID != 12346)


## MPV-FE state mismatches ----

mpv_temp$feID[mpv_temp$mpvID == 676] <- 13210

mpv_temp <- mpv_temp %>% filter(mpvID != 707)

mpv_temp$feID[mpv_temp$mpvID == 828] <- 13457

mpv_temp$feID[mpv_temp$mpvID == 1148] <- 13986
mpv_temp$feID[mpv_temp$mpvID == 1888] <- 15168

mpv_temp$feID[mpv_temp$mpvID == 2553] <- 16132
mpv_temp$date[mpv_temp$mpvID == 2553] <- as.Date("2015-05-20")

mpv_temp$feID[mpv_temp$mpvID == 2846] <- 16539

mpv_temp$feID[mpv_temp$mpvID == 3131] <- 16940
mpv_temp$feID[mpv_temp$mpvID == 9216] <- 28765
mpv_temp$feID[mpv_temp$mpvID == 8520] <- 28968
mpv_temp$feID[mpv_temp$mpvID == 8660] <- 29243

mpv_temp$feID[mpv_temp$mpvID == 8863] <- 29662
mpv_temp$wapoID[mpv_temp$mpvID == 8863] <- 6687

mpv_temp$State[mpv_temp$mpvID == 8814] <- "AR"

mpv_temp$feID[mpv_temp$mpvID == 8770] <- 29837

mpv_temp$feID[mpv_temp$mpvID == 9078] <- 29967
mpv_temp$feID[mpv_temp$mpvID == 9250] <- 30347
mpv_temp$feID[mpv_temp$mpvID == 9602] <- 31021


## MPV-wapo state mismatches ----

mpv_temp$wapoID[mpv_temp$mpvID == 5350] <- 3213
mpv_temp$wapoID[mpv_temp$mpvID == 5353] <- 3212
mpv_temp$wapoID[mpv_temp$mpvID == 5351] <- 3214

## need to change ORI and any other agency specific info
mpv_temp$`Agency responsible for death`[mpv_temp$mpvID == 6861] <- "Kelso Police Department"
mpv_temp$`ORI Agency Identifier (if available)`[mpv_temp$mpvID == 6861] <- "WA0080100"
#mpv_temp$agency.type[mpv_temp$mpvID == 6861] <- "Local Police Department" # own type var from agency

mpv_temp$wapoID[mpv_temp$mpvID == 6880] <- 4649
mpv_temp$wapoID[mpv_temp$mpvID == 6882] <- 5413
mpv_temp$wapoID[mpv_temp$mpvID == 6881] <- 4654

mpv_temp$wapoID[mpv_temp$mpvID == 6887] <- 5414
mpv_temp$wapoID[mpv_temp$mpvID == 6886] <- 4681
mpv_temp$wapoID[mpv_temp$mpvID == 6937] <- 4721
mpv_temp$wapoID[mpv_temp$mpvID == 6938] <- 5418
mpv_temp$wapoID[mpv_temp$mpvID == 6936] <-5227
  
mpv_temp$wapoID[mpv_temp$mpvID == 7451] <- NA
mpv_temp$wapoID[mpv_temp$mpvID == 8141] <- NA
  
mpv_temp$wapoID[mpv_temp$mpvID == 9272] <- 6951
  
mpv_temp$State[mpv_temp$mpvID == 11738] <- "NY"
mpv_temp$State[mpv_temp$mpvID == 12118] <- "OK"
mpv_temp$State[mpv_temp$mpvID == 12133] <- "WV"


## MPV-FE sex mismatches ----

mpv_temp$feID[mpv_temp$mpvID == 1746] <- 14946

mpv_temp$name[mpv_temp$mpvID == 277] <- "Stacey Stout"
mpv_temp$`Victim's gender`[mpv_temp$mpvID == 277] <- "Female"

mpv_temp$name[mpv_temp$mpvID == 6672] <- "Joshua Rembert Williams"

mpv_temp$`Victim's gender`[mpv_temp$mpvID %in% 
                             c(646, 1902, 1584, 1973,
                               3141, 3800, 7531, 8615,
                               11604, 11798, 11953, 12232, 
                               10733, 6628, 8474, 10130, 10280, 
                               11817)] <- "Male"

mpv_temp$`Victim's gender`[mpv_temp$mpvID == 5092] <- "Nonbinary"

# MPV-FE Date mismatches (only IDs bad matches for now) ----

mpv_temp$feID[mpv_temp$mpvID == 8384] <- 28632


# unmatched
mpv_temp$date[mpv_temp$mpvID == 11788] <- as.Date("2021-10-19")




