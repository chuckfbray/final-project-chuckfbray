# Final Project
# For your final project we ask that you turn in a 4-6 page report using data to answer a public health related question:
# 
# Based on state-level data, how effective were vaccines against SARS-CoV-2 reported cases, hospitalizations, and deaths;
# vaccination rates?


### PART 1: setting up libraries and useful functions
rm(list=ls())
gc()

library(jsonlite)
library(magrittr)
library(tidyverse)
library(httr2)
library(purrr)
library(ggplot2)
library(readxl)

# write data pull function called `get_cdc_data` to request CDC hospitalization, provisional COVID deaths, and vaccine data.
get_cdc_data <- function(api){
  cases_raw <- request(api) |> 
    req_url_query("$limit" = 10000000) |>
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
}


### PART 2: import state-level data for 2020 through present...

# request population data... the key is defined by the source code census-key.R.
source("census-key.R")

## Read-in population
api <- "https://api.census.gov/data/2021/pep/population"
request <- request(api) |>
  req_url_query(get = I("POP_2020,POP_2021,NAME"),
                `for` = I("state:*"),
                key = census_key)
response <- request |> req_perform()


## parse and manipulate dataframe from data request
population <- resp_body_json(response, simplifyVector = TRUE) |>
  janitor::row_to_names(1) |>
  as_tibble() |>
  select(-state) |>
  rename(state_name = NAME) |>
  pivot_longer(-state_name, names_to = "year", values_to = "population") |>
  mutate(year = str_remove(year, "POP_")) |>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]) |>
  mutate(state = case_when(
    state_name == "District of Columbia" ~ "DC",
    state_name == "Puerto Rico" ~ "PR",
    .default = state))

## what about later years? 2022, 2023, 2024? Obtain from a downloaded file from https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html
laterpops <-
  readxl::read_excel("../data/NST-EST2023-POP.xlsx") %>%
  janitor::row_to_names(row_number = 3)

names(laterpops) <- c("state","basepop","pop2020","pop2021","pop2022","pop2023","pop2024")

laterpops %<>%
  ## clean state names
  filter(substring(state,1,1) == ".") %>%
  mutate(state = gsub("\\.","",state),
         state_name = state) %>%
  mutate(state = state.abb[match(state_name, state.name)]) %>%
  mutate(state = case_when(state_name == "District of Columbia" ~ "DC",
                           state_name == "Puerto Rico" ~ "PR",
                           .default = state))

## impute 2024 by looking at rate of change from 2022-2023...
laterpops %<>%
  mutate(pop2024 = (pop2023/pop2022)*pop2023)

### >>> these population estimates are both from the Census bureau, but are discordant, so take the full set from the later source!


## import regions info
url <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"

regions <- fromJSON(url, simplifyDataFrame = FALSE) |>
  map_df(function(x){
    data.frame(state_name = x$states, region = x$region, region_name =
                 x$region_name)
  }) |>
  mutate(region = factor(as.numeric(region))) |>
  mutate(region_name = ifelse(nchar(region_name) > 50, "NY,NJ,PR,USVI",
                              region_name))


### merge population and regions data
laterpops <- left_join(laterpops, regions, by = "state_name")

## finally, reshape long so that each observation is a state-year...
laterpops_long <-
  laterpops %>% mutate(pop2020=as.double(pop2020)) %>%
  pivot_longer(cols=c("pop2020","pop2021","pop2022","pop2023","pop2024"), names_to="year", values_to="population") %>%
  mutate(year=as.double(gsub("pop","",year)))


## use the `get_cdc_data` function to request COVID-19 data w/different endpoints form the CDC API
casesAPI = "https://data.cdc.gov/resource/pwn4-m3yp.json"             # https://data.cdc.gov/Case-Surveillance/Weekly-United-States-COVID-19-Cases-and-Deaths-by-/pwn4-m3yp/about_data
hospitalizationsAPI = "https://data.cdc.gov/resource/39z2-9zu6.json"  # 
deathsAPI = "https://data.cdc.gov/resource/r8kw-7aab.json"            # https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Week-Ending-D/r8kw-7aab/about_data
vaccinationsAPI = "https://data.cdc.gov/resource/rh2h-3yt2.json"      # 

# read in absolute SARS-COV2 case counts per week
cases_raw <- get_cdc_data(casesAPI)

# read in daily related hospitalization counts
hosp_raw <- get_cdc_data(hospitalizationsAPI)

# read in related death counts per week
deaths_raw <- get_cdc_data(deathsAPI)

# read in daily COVID-19 vaccination counts
vax_raw <- get_cdc_data(vaccinationsAPI)



## describe dataframes obtained: what are the relevant variable fields, and how must I harmonize them?

# 1) the name of the column with state abbreviations,
# 2) if the it's yearly, monthly, or weekly, daily data,
# 3) all the column names that provide date information.


# Outcome | Jurisdiction var name |     Rate      | time variable names    |
#---------|-----------------------|---------------|------------------------|
# cases   |        state          |    weekly     |`start_date`, `end_date`|
# hosps   |    jurisdiction       |    daily      |   `collection_date`    |
# deaths  |        state          |    weekly     |`start_date`, `end_date`|
# vax     |     `location`        |     daily     |         `date`         |



## in `cases_raw`, keep state, MMWR year, MMWR week, and the total number of cases for that week in that state
cases <- cases_raw |> mutate(cases = parse_number(new_cases),
                             date = as_date(ymd_hms(end_date))) |>
  filter(state %in% population$state) |>
  mutate(mmwr_week = epiweek(date), mmwr_year = epiyear(date)) |>
  select(state, mmwr_year, mmwr_week, cases) |>
  arrange(state, mmwr_year, mmwr_week)


## clean and standardize hospitalizations data

hosp <- hosp_raw |>
  filter(jurisdiction %in% population$state) |>
  rename(hosp = new_covid_19_hospital, state = jurisdiction) |>
  mutate(hosp = parse_number(hosp),
         date = as_date(ymd_hms(collection_date)),
         mmwr_week = epiweek(date), mmwr_year = epiyear(date)) |>
  select(state, mmwr_year, mmwr_week, hosp) |>
  group_by(state, mmwr_year, mmwr_week) |>
  summarize(hosp = sum(hosp), n = n(), .groups = "drop") |>
  filter(n == 7) |>
  select(-n) |>
  arrange(mmwr_year, mmwr_week)


## clean and standardize deaths data
deaths <- deaths_raw |>
  filter(state %in% population$state_name) |>
  mutate(end_date = as_date(end_date),
         mmwr_year = epiyear(end_date)) |>
  rename(deaths_prov = covid_19_deaths,
         flu = influenza_deaths) |>
  mutate(mmwr_week = parse_number(mmwr_week),
         deaths = parse_number(deaths_prov)) |>
  select(state, mmwr_week, mmwr_year, deaths)


## clean and standardize vaccinations data
vax <- vax_raw |> filter(date_type == "Admin" & location %in%
                           population$state) |>
  rename(state = location, series_complete = series_complete_cumulative,
         booster = booster_cumulative) |>
  mutate(date = as_date(ymd_hms(date)),
         mmwr_week = as.numeric(mmwr_week), mmwr_year = epiyear(date),
         series_complete = parse_number(series_complete),
         booster = parse_number(booster)) |>
  select(state, date, mmwr_week, mmwr_year, series_complete, booster) |>
  group_by(state, mmwr_week, mmwr_year) |>
  summarize(series_complete = max(series_complete),
            booster = max(booster),
            .groups = "drop") |>
  arrange(state, mmwr_year, mmwr_week)

# Keep the variables `series_complete` and `booster` along with state and date.


### join the tables
all_dates <- data.frame(date = seq(make_date(2020, 1, 25),
                                   make_date(2024, 12, 1),
                                   by = "week")) |>
  mutate(date = ceiling_date(date, unit = "week", week_start = 7) - days(1)) |>
  mutate(mmwr_year = epiyear(date), mmwr_week = epiweek(date))

dates_and_pop <- cross_join(all_dates, data.frame(state =
                                                    unique(laterpops_long$state))) |>
  left_join(laterpops_long, by = c("state", "mmwr_year" = "year"))

dat <- dates_and_pop |>
  left_join(cases, by = c("state", "mmwr_week", "mmwr_year")) |>
  left_join(hosp, by = c("state", "mmwr_week", "mmwr_year")) |>
  left_join(deaths, by = c("state_name" = "state", "mmwr_week",
                           "mmwr_year")) |>
  left_join(vax, by = c("state", "mmwr_week", "mmwr_year")) |>
  arrange(state, date)


## clean NA values 
dat %<>%
  mutate(series_complete = ifelse(is.na(series_complete) & mmwr_year==2020, 0, series_complete),
         booster = ifelse(is.na(booster) & mmwr_year==2020, 0, booster),
         deaths = ifelse(is.na(deaths), 0, deaths),
         cases = ifelse(is.na(deaths), 0, cases))

## latest weeks where data reported by states?
dat %>% filter(!is.na(hosp) & mmwr_year==2024) %>% summarize(latestweek=max(mmwr_week,na.rm=T))             # hosps:   17 (2024)
dat %>% filter(!is.na(series_complete) & mmwr_year==2023) %>% summarize(latestweek=max(mmwr_week,na.rm=T))  # series:  19 (2023)
dat %>% filter(!is.na(booster) & mmwr_year==2023) %>% summarize(latestweek=max(mmwr_week,na.rm=T))          # booster: 19 (2023)
dat %>% filter(!is.na(deaths) & mmwr_year==2024) %>% summarize(latestweek=max(mmwr_week,na.rm=T))           # deaths:  48 (2024)


# save as RDA to data directory...
saveRDS(dat,
        "../data/cleandat.RDS")

