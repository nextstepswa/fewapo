# Post-cleaning edits
# Executed immediately before name split and merging

# All of these are currently for WA state, for temp info lags
# should be kept up to date and deleted when no longer relevant

# NB: wapo_clean gender var is capitalized (raw was not)

# ---------------------------------------------------------------------

# Jeffrey Webley (name)

if (wapo_clean$name[wapo_clean$wapoID == 8582] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 8582] <- "Jeffrey Webley"
  message("Jeffrey Webley name fix for WaPo")
} else {
  message("\n\n WaPo Jeffrey Webley name fix not needed anymore\n\n")
}


# Jeffrey Kiner (age)

if (wapo_clean$age[wapo_clean$wapoID == 10823] == 999){
  wapo_clean$age[wapo_clean$wapoID == 10823] <- 57
  message("J Kiner age fix for WaPo")
} else {
  message("\n\n WaPo J Kiner age fix not needed anymore\n\n")
}

# Sorin Ardelean 12/27/2021
if (fe_clean$name[fe_clean$feID == 31462] == "Unknown"){
  fe_clean$name[fe_clean$feID == 31462] <- "Sorin Ardelean"
  fe_clean$age[fe_clean$feID == 31462] <- 43
  fe_clean$cod[fe_clean$feID == 31462] <- "Gunshot"
  fe_clean$agency[fe_clean$feID == 31462] <- "Algona Police Department"
  fe_clean$url_info[fe_clean$feID == 31462] <- "https://www.auburn-reporter.com/news/fatal-police-shooting-of-suspect-in-algona-hostage-taking-incident-is-subject-of-investigation/"
  fe_clean$url_click[fe_clean$feID == 31462] <- make_url_fn(fe_clean$url_info[fe_clean$feID == 31462])
  message("Sorin Ardelean fix for FE")
} else {
  message("\n\n FE Ardelean fix not needed anymore \n\n")
}


