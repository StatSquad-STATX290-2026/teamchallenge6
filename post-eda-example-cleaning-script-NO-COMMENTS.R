## STATX290: US Presidential Polling Data 2012/2016
## Example cleaning script
## Notes: 
## 1. Line-by-line commenting deliberately removed. 
## 2. Checking intermediate cleaning steps is not performed here but highly
##    recommended

# Load packages 
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

## CLEANING 1.1: Clean 2012 data
polls_2012_raw <- read_csv("state_polls_2012.csv") # import 2012 raw polling dataset 

polls_2012_clean <- polls_2012_raw |> 
  separate_wider_delim(col = "poll_info", delim = "-president-", names = c("year_state", "major_party_candidates")) |>
  mutate(state = str_remove(year_state, pattern = "2012-"), year = 2012) |>
  select(-year_state) |>
  mutate(Other = as.numeric(na_if(Other, "Not included in poll"))) |> 
  mutate(sample_size = na_if(sample_size, -1)) |> 
  relocate(major_party_candidates, year, state)
  

## CLEANING 1.2: Clean 2016 data
polls_2016_raw <- read_csv("state_polls_2016.csv")

polls_2016_clean <- polls_2016_raw |> 
  separate_wider_delim(col = "poll_info", delim = "-president", names = c("year_state", "major_party_candidates")) |>
  mutate(major_party_candidates = "trump-vs-clinton") |>
  mutate(state = str_remove(year_state, pattern = "2016-"), year = 2016) |>
  select(-year_state) |>
  mutate(Other = as.numeric(na_if(Other, "Not included in poll"))) |> 
  mutate(sample_size = na_if(sample_size, -1)) |> 
  relocate(major_party_candidates, year, state)

## CLEANING 1.3: Join 2012 and 2016

polls_2016_standardised <- polls_2016_clean |> 
  rename(Dem = Clinton, Rep = Trump) |>
  mutate(Minor = Johnson + McMullin, minor_party_candidates = "aggregate of johnson and mcmullin") |>
  select(-Johnson, -McMullin) |>
  mutate(days_to_election_start = as.Date("2016-11-08") - start_date,
         days_to_election_end = as.Date("2016-11-08") - end_date) |>
  select(-start_date,-end_date) |>
  mutate(state = replace_when(state, state == "washington-d-c" ~ "washington-dc"))

polls_2012_standardised <- polls_2012_clean |> 
  rename(Dem = Obama, Rep = Romney) |>
  mutate(Minor = NA_real_, minor_party_candidates = NA_character_) |>
  mutate(days_to_election_start = as.Date("2012-11-06") - start_date,
         days_to_election_end = as.Date("2012-11-06") - end_date) |>
  select(-start_date,-end_date)

polls_merged <- bind_rows(polls_2012_standardised, polls_2016_standardised) |>
  relocate(year, state, Dem, Rep, Undecided, Other, Minor,days_to_election_start,days_to_election_end ) |> 
  relocate(major_party_candidates, minor_party_candidates, .after = last_col()) |>
  mutate(Dem = Dem / 100, Rep = Rep / 100, Undecided = Undecided / 100, Other = Other / 100, Minor = Minor / 100)





