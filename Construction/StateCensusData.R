##################################################
# Population data by State: 2000-2019
# Source:  Census Data
# Overall and Males only

# Input: scraped from online sources
# Output: StatePop.rda
##################################################

rm(list=ls())

library(dplyr)
library(here)

# function to create state abbreviations
state_abb <- function(x){
  abbs <- c(state.abb, "DC")
  names(abbs) <- c(state.name, "District of Columbia")
  abbs[x]
}

# Read in Data ----

# 2000-10
# Five-Year Age Groups, Sex, Race and Hispanic Origin for States and the United States: 
# April 1, 2000 to July 1, 2010
# NOTE: Race is 6 groups, 5 alone + multiple
# https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-state.html

pop.st.ars_00_10 <- rio::import("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/state/st-est00int-alldata.csv")
                               
# 2010-19
# Single Yr Age, Sex, Race, and Hispanic Origin - 6 race groups for States only
# (5 Race Alone Groups and Two or More Races) (SC-EST2019-ALLDATA6)
# https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-detail.html

pop.st.ars_10_19 <- rio::import("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/asrh/sc-est2019-alldata6.csv")

# 2020-22
# Single Yr Age, Sex, Race, and Hispanic Origin - 6 race groups for States only
# (5 Race Alone Groups and Two or More Races) (SC-EST2022-ALLDATA6)
# https://www.census.gov/data/datasets/time-series/demo/popest/2020s-state-detail.html

pop.st.ars_20_22 <- rio::import("https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/state/asrh/sc-est2022-alldata6.csv")


# State totals and components of change, can be used for projections to 2020-1?
#pop.st_growth <- rio::import("http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv")

# Filter to 15-69 yo and combine decades ----

## There are some differences in file structure that must be addressed

# p2000 <- pop.st.ars_00_10 %>%
#   filter(REGION !=0 & DIVISION !=0 & STATE !=0 & 
#            SEX != 0 & ORIGIN != 0 & RACE !=0) %>% # remove totals
#   filter(AGEGRP > 3 & AGEGRP < 15) %>% # 15-69yo
#   select(-c(ESTIMATESBASE2000, CENSUS2010POP, POPESTIMATE2010))  %>%
#   group_by(STATE, NAME, REGION, DIVISION, SEX, ORIGIN, RACE) %>%
#   summarize(across(starts_with("POP"), ~sum(.x, na.rm = TRUE)))

p2000 <- pop.st.ars_00_10 %>%
  filter(REGION !=0 & DIVISION !=0 & STATE !=0 & 
           SEX != 0 & ORIGIN != 0 & RACE !=0 & AGEGRP !=0) %>% # remove totals
  mutate(age.grp = factor(case_when(AGEGRP < 4 ~ "young",
                                    AGEGRP > 13 ~ "old",
                                    TRUE ~ "middle"),
                          levels = c("young", "middle", "old"))) %>%
  select(-c(ESTIMATESBASE2000, CENSUS2010POP, POPESTIMATE2010, AGEGRP))  %>%
  group_by(STATE, NAME, REGION, DIVISION, SEX, ORIGIN, RACE, age.grp) %>%
  summarize(across(starts_with("POP"), ~sum(.x, na.rm = TRUE)))

## For 2010s and 2020s we will construct a proto object first to allow for an age
## distribution table
## Drop 2022 since FE stopped updating in 2021

p2010_2021_tmp <- full_join(pop.st.ars_10_19, pop.st.ars_20_22) %>%
  filter(REGION !=0 & DIVISION !=0 & STATE !=0 & 
           SEX != 0 & ORIGIN != 0 & RACE !=0) %>% # remove totals
  mutate(age.grp = factor(case_when(AGE<15 ~ "young",
                                    AGE>64 ~ "old",
                                    TRUE ~ "middle"),
                          levels = c("young", "middle", "old"))) %>%
  select(-c(SUMLEV, ESTIMATESBASE2010, CENSUS2010POP, 
            ESTIMATESBASE2020, POPESTIMATE2022)) 

## For the rest of the analysis, we conform to the p2000 object.
p2010_2021 <- p2010_2021_tmp %>%
  group_by(STATE, NAME, REGION, DIVISION, SEX, ORIGIN, RACE, age.grp) %>%
  summarize(across(starts_with("POP"), ~sum(.x, na.rm = TRUE))) 

#-------------------------------------------------------------------------------------
  
# Overall US age distribution in the 2010s ----
pop.agedist.2010_2021 <- p2010_2021_tmp %>%
  group_by(AGE) %>%
  summarize(across(starts_with("POP"), ~sum(.x, na.rm = TRUE))) %>%
  rowwise() %>%
  mutate(average = rowMeans(across(starts_with("POP")), na.rm = T)) %>%
  ungroup() %>%
  mutate(pct = average/sum(average)) %>%
  select(c(age=AGE, average, pct))

# State Population by race, sex and age.grp ----
pop.st.rsa <- p2000 %>% 
  left_join(p2010_2021) %>%
  mutate(st = state_abb(NAME),
         sex = if_else(SEX == 1, "male", "female"),
         hispanic = if_else(ORIGIN == 2,
                            "Hispanic", "Not Hispanic"),
         race = case_when(RACE == 1 ~ "White",
                          RACE == 2 ~ "Black",
                          RACE == 3 ~ "Native American",
                          RACE == 4 ~ "Asian",
                          RACE == 5 ~ "Hawaiian_PI",
                          RACE == 6 ~ "Multiple"),
         race.eth = if_else(hispanic == "Hispanic", "HL", race))

# State Population by race and sex ----
pop.st.rs <- p2000 %>% 
  left_join(p2010_2021) %>%
  filter(age.grp == "middle") %>%
  select(-age.grp) %>%
  mutate(st = state_abb(NAME),
         sex = if_else(SEX == 1, "male", "female"),
         hispanic = if_else(ORIGIN == 2,
                            "Hispanic", "Not Hispanic"),
         race = case_when(RACE == 1 ~ "White",
                          RACE == 2 ~ "Black",
                          RACE == 3 ~ "Native American",
                          RACE == 4 ~ "Asian",
                          RACE == 5 ~ "Hawaiian_PI",
                          RACE == 6 ~ "Multiple"),
         race.eth = if_else(hispanic == "Hispanic", "HL", race))

# State info (name, region, division) ----
# note that "STATE" is the FIPS
st.info <- pop.st.rsa %>% 
  group_by(STATE, NAME, REGION, DIVISION, st) %>% 
  count() %>%
  select(-n) %>%
  ungroup()

# State population totals ----

pop.st.age.yr <- pop.st.rsa %>%
  group_by(st, age.grp) %>%
  summarize(across(starts_with("POP"), ~sum(.x, na.rm = TRUE))) %>%
  tidyr::pivot_longer(cols = starts_with("POP"),
                      names_to = "year",
                      values_to = "pop") %>%
  mutate(year = as.numeric(gsub("POPESTIMATE", "", year)))

pop.st.yr <- pop.st.rs %>%
  group_by(st) %>%
  summarize(across(starts_with("POP"), ~sum(.x, na.rm = TRUE))) %>%
  tidyr::pivot_longer(cols = starts_with("POP"),
                      names_to = "year",
                      values_to = "pop") %>%
  mutate(year = as.numeric(gsub("POPESTIMATE", "", year)))

# State male population totals ----

males.st.yr <- pop.st.rs %>%
  filter(sex == "male") %>%
  group_by(st) %>%
  summarize(across(starts_with("POP"), ~sum(.x, na.rm = TRUE))) %>%
  tidyr::pivot_longer(cols = starts_with("POP"),
                      names_to = "year",
                      values_to = "pop") %>%
  mutate(year = as.numeric(gsub("POPESTIMATE", "", year)))

# State population by race.eth ----
## For consistency with FE, we collapse Asian and PI

pop.st.race.yr  <- pop.st.rs %>%
  mutate(race.eth.fe = case_when(
    race.eth == "White" ~ "WEA",
    race.eth == "Black" ~ "BAA",
    race.eth == "Hispanic" ~ "HL",
    race.eth == "Asian" | race.eth == "Hawaiian_PI" ~ "API",
    race.eth == "Native American" ~ "NAA",
    race.eth == "Multiple" ~ "Mult")) %>%
  group_by(st, race.eth.fe) %>%
  summarize(across(starts_with("POP"), ~sum(.x, na.rm = TRUE))) %>%
  tidyr::pivot_longer(cols = starts_with("POP"),
                      names_to = "year",
                      values_to = "pop") %>%
  mutate(year = as.numeric(gsub("POPESTIMATE", "", year)))

males.st.race.yr  <- pop.st.rs %>%
  filter(sex == "male") %>%
  mutate(race.eth.fe = case_when(
    race.eth == "White" ~ "WEA",
    race.eth == "Black" ~ "BAA",
    race.eth == "Hispanic" ~ "HL",
    race.eth == "Asian" | race.eth == "Hawaiian_PI" ~ "API",
    race.eth == "Native American" ~ "NAA",
    race.eth == "Multiple" ~ "Mult")) %>%
  group_by(st, race.eth.fe) %>%
  summarize(across(starts_with("POP"), ~sum(.x, na.rm = TRUE))) %>%
  tidyr::pivot_longer(cols = starts_with("POP"),
                      names_to = "year",
                      values_to = "pop") %>%
  mutate(year = as.numeric(gsub("POPESTIMATE", "", year)))

popdata <- list("st.info" = st.info,
                "pop.agedist.2010_2021" = pop.agedist.2010_2021,
                "pop.st.rs"= pop.st.rs, "pop.st.rsa"=pop.st.rsa,
                "pop.st.yr"=pop.st.yr, "pop.st.age.yr"=pop.st.age.yr, 
                "males.st.yr"=males.st.yr, 
                "pop.st.race.yr"=pop.st.race.yr, 
                "males.st.race.yr"=males.st.race.yr)

save(popdata, 
     file = here::here("Data", "Clean", "AllStatePops2000-2021.rda"))




