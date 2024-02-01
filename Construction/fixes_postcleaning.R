# Post-cleaning edits
# Executed immediately before name split and merging

# All of these are currently for WA state, for temp info lags
# should be kept up to date and commented out when no longer relevant

# ---------------------------------------------------------------------


# Franklin Ross
if (wapo_clean$name[wapo_clean$wapoID == 9831] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 9831] <- "Franklin O Ross"
  print("Franklin Ross fix for WaPo")
} else {
  print("\n\n WaPo Ross fix not needed anymore\n\n")
}


# Dillion Pugsley 12/30/2022

if (wapo_clean$name[wapo_clean$wapoID == 8786] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 8786] <- "Dillion Pugsley"
  print("Dillion Pugsley fix for WaPo")
} else {
  print("\n\n WaPo Pugsley fix not needed anymore \n\n")
}


# Joseph Sanchez 2/11/2022

if (wapo_clean$name[wapo_clean$wapoID == 7735] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 7735] <- "Joseph Sanchez"
  wapo_clean$age[wapo_clean$wapoID == 7735] <- 36
  print("Joseph Sanchez fix for WaPo")
} else {
  print("\n\n WaPo Sanchez fix not needed anymore \n\n")
}

# Sorin Ardelean 12/27/2021
if (fe_clean$name[fe_clean$feID == 31462] == "Unknown"){
  fe_clean$name[fe_clean$feID == 31462] <- "Sorin Ardelean"
  fe_clean$age[fe_clean$feID == 31462] <- 43
  fe_clean$cod[fe_clean$feID == 31462] <- "Gunshot"
  fe_clean$agency[fe_clean$feID == 31462] <- "Algona Police Department"
  fe_clean$url_info[fe_clean$feID == 31462] <- "https://www.auburn-reporter.com/news/fatal-police-shooting-of-suspect-in-algona-hostage-taking-incident-is-subject-of-investigation/"
  fe_clean$url_click[fe_clean$feID == 31462] <- make_url_fn(fe_clean$url_info[fe_clean$feID == 31462])
  print("Sorin Ardelean fix for FE")
} else {
  print("\n\n FE Ardelean fix not needed anymore \n\n")
}

if (wapo_clean$name[wapo_clean$wapoID == 7518] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 7518] <- "Sorin Ardelean"
  wapo_clean$age[wapo_clean$wapoID == 7518] <- 43
  print("Sorin Ardelean fix for WaPo")
} else {
  print("\n\n WaPo Ardelean fix not needed anymore \n\n")
}


