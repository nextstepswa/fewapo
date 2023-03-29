#----------------------------------------------------------------- 
# This child script creates the common datafiles and variables used in
# WA since 940 and 2015 analyses.
# It relies on the params set in the yaml header of the parent .Rmd files 
# for each of those reports
#-----------------------------------------------------------------

# Select homicides by police and use age from FE

homicides <- merged_data %>% 
  filter(homicide==1 & not.kbp == 0) %>%
  mutate(age = ifelse(age.fe==999, NA_real_, age.fe)) %>%
  mutate(
    cause.of.death = case_when(
      cod == "Vehicle" & pursuit.type == "Pursuit" ~ "accident during a police vehicular pursuit",
      cod == "Vehicle" & pursuit.type == "Attempted stop" ~ "vehicle accident following an attempted stop by police",
      cod == "Vehicle" & pursuit.type == "Vehicle accident" ~ "vehicle accident by an on-duty officer",
      cod == "Vehicle" & pursuit.type == "Involved pursuit" ~ "accident involving a police pursuit", # 2 odd cases, 26719 and 90019
      pursuit.type == "Involved pursuit" ~ paste(cod, "after a police vehicular pursuit")),
    cause.of.death = if_else(is.na(cause.of.death), as.character(cod), cause.of.death)
  ) %>%
  arrange(date)

all.cases <- nrow(merged_data)
all.homicides <- nrow(homicides)


# Dates and years ----

## Remember for trend plotting later -- remove partial months
start_yr <- params$startyr
start_mo <- 1
start_date <- as.Date(paste0(start_yr, "-", start_mo, "-01"))

curr_mo <- month(Sys.Date())
curr_yr <- year(Sys.Date())

## Calendar years (all)
cal.yrs <- start_yr:curr_yr
num.cal.yrs <- length(cal.yrs)

## Legislative years (all)
leg.yrs <- unique(homicides$leg.year)
num.leg.yrs <- length(leg.yrs)

# Most recent case info ----
# Assumes homicide df is sorted by date

last.case <- nrow(homicides)
last.case.info <- homicides[last.case,] 

last.date <- last.case.info$date
last.name <- ifelse(last.case.info$name == "Unknown", 
                    "(Name not released)",        
                    last.case.info$name)
last.age <- ifelse(is.na(last.case.info$age), 
                   "(age not released)",
                   paste(last.case.info$age, "years old"))
last.agency <- last.case.info$agency

last.cod <- last.case.info$cause.of.death



last.url <- last.case.info$url_click

# Summary info ----
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
