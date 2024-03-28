# Post-cleaning edits
# Executed immediately before name split and merging

# All of these are currently for WA state, for temp info lags
# should be kept up to date and deleted when no longer relevant

# ---------------------------------------------------------------------

# Lopeti Aiolupotea-Magalei
if (wapo_clean$name[wapo_clean$wapoID == 10248] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 10248] <- "Lopeti Aiolupotea Magalei"
  wapo_clean$race[wapo_clean$wapoID == 10248] <- "A"
  message("Lopeti Aiolupotea-Magalei fix for WaPo")
} else {
  message("\n\n WaPo Lopeti Aiolupotea-Magalei fix not needed anymore\n\n")
}


# Franklin Ross
if (wapo_clean$name[wapo_clean$wapoID == 9831] == "Unknown"){
  wapo_clean$name[wapo_clean$wapoID == 9831] <- "Franklin O Ross"
  message("Franklin Ross fix for WaPo")
} else {
  message("\n\n WaPo Ross fix not needed anymore\n\n")
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


