#----------------------------------------------------------------- 
# This child script creates the common datafiles and variables used in
# WA since 940 and 2015 analyses.
# It relies on the params set in the yaml header of the parent .Rmd files 
# for each of those reports
#-----------------------------------------------------------------

# Select homicides and use age and cod from FE

homicides <- merged_data %>% 
  filter(homicide==1) %>%
  mutate(age = ifelse(age.fe==999, NA_real_, age.fe),
         cod = cod.fe) %>%
  arrange(date)

all.cases <- nrow(merged_data)

# For trend plotting -- remove partial months
start_yr <- params$startyr
start_mo <- 1

# Current date ----

curr_mo <- month(Sys.Date())
curr_yr <- year(Sys.Date())

# Most recent case info ----

last.case <- nrow(homicides)

last.date <- max(homicides$date)
last.name <- ifelse(homicides$lname[last.case] == "Unknown", 
                    "(Name not released)",        
                    paste(homicides$fname[last.case],
                          homicides$lname[last.case]))
last.age <- ifelse(is.na(homicides$age[last.case]), 
                   "not released",
                   homicides$age[last.case])
last.agency <- homicides$agency[last.case]

last.cod <- homicides$cod[last.case]
last.url <- homicides$url_click[last.case]

tot.by.yr <- table(homicides$year)
tot.this.yr <- tot.by.yr[[length(tot.by.yr)]]
tot.yr.is <- max(homicides$year)

num.suffix <- case_when(tot.this.yr == 1 ~ "st",
                        tot.this.yr == 2 ~ "nd",
                        tot.this.yr == 3 ~ "rd", 
                        TRUE ~ "th")

# Indices for plotting by time ----

## starts with first complete month
## ends with last complete month
## we use "mo" for numeric month and "mon" for alpha month abb

start_date <- as.Date(paste0(start_yr, "-", start_mo, "-01"))
numyrs <- last_complete_yr - start_yr + 1

## set up first year
mon <- month.abb[start_mo:12]
year <- rep(start_yr, length(mon))

## generate middle months/years
for(i in 1:(numyrs-1)) {
  #print(i)
  thisyr = start_yr+i
  #print(thisyr)
  year <- c(year, rep(thisyr, 12))
  mon <- c(mon, month.abb)
}

## tack on last months/year for current year if
## last complete month is not December (of prev yr)
if (last_complete_mo != 12) {
  year <- c(year, rep(curr_yr, last_complete_mo))
  mon <- c(mon, month.abb[1:last_complete_mo])
}

## Sequential complete mo/yr index for date plotting

index <- data.frame(year = year,
                    mon = mon,
                    mon.yr = paste0(mon, ".", year),
                    index.date = lubridate::ymd(
                      paste0(year, "-", mon, "-01")
                    ))

# Significant dates ----

## Legislation
date.940.WAC <- lubridate::ymd("2020-01-06")
date.firstbillpass <- lubridate::ymd("2021-03-09")
date.firstbillsign <- lubridate::ymd("2021-04-20")
date.lastbillsign <- lubridate::ymd("2021-04-26")

## Prosecution
date.sarey.charged <- lubridate::ymd("2020-08-20")
date.ellis.ago <- lubridate::ymd("2020-06-23")
date.ellis.charged <- lubridate::ymd("2021-05-27")
