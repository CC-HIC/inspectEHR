# Write sample data
# This script helps write some fake data for testing.
# It should have structually sound data for 1000 patients

library(tidyverse)
library(lubridate)
#library(inspectEHR)
library(devtools); load_all()

## PROVENANCE TABLE ====

provenance <- tibble(
  filename = paste0("simulated_files/", 1:5, ".xml"),
  file_id = rep(1:5),
  date_created = sample(
    seq.POSIXt(
      ymd_hms("2019-01-01 00:00:00"),
      ymd_hms("2019-06-01 00:00:00"),
      by = "hour"),
    size = 5),
  version = "v8.3.2",
  date_parsed = lubridate::now(),
  site = LETTERS[1:5],
  theme = "ICU",
  notes = as.character(NA))

## EPISODES TABLE ====

# Sample size
ss <- 1000

episodes <- tibble(
  episode_id = 1:ss,
  nhs_number = generate_nhs(ss),
  start_date = sample(
    seq.POSIXt(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2019-01-01 00:00:00"), by = "hour"),
    size = ss, replace = TRUE),
  provenance = rep(1:5, each = 200))

## EVENTS TABLE ====

gp_codes <- read_csv("./data-raw/resources/gp_codes.csv") %>%
  filter(address4 %in% c("LONDON", "CAMBRIDGESHIRE", "OXFORDSHIRE")) %>%
  select(org_code) %>%
  pull()

load("./data-raw/resources/treatment_fx_codes.RData")

# Set up some of the most important base features
dfs <- episodes %>%
  left_join(provenance, by = c("provenance" = "file_id")) %>%
  select(episode_id, nhs_number, start_date, site) %>%
  rename(NIHR_HIC_ICU_0073 = nhs_number,
         NIHR_HIC_ICU_0411 = start_date,
         NIHR_HIC_ICU_0002 = site)

dfs <- dfs %>%
  group_by(NIHR_HIC_ICU_0002) %>%
  mutate(
    NIHR_HIC_ICU_0001 = paste0(NIHR_HIC_ICU_0002, stringr::str_pad(1:n(), width = 6, pad = "0"))) %>%
  ungroup() %>%
  mutate(
    NIHR_HIC_ICU_0003 = sample(gp_codes, size = ss, replace = TRUE),
    NIHR_HIC_ICU_0004 = sample(tfc$code, size = ss, replace = TRUE),
    NIHR_HIC_ICU_0005 = NIHR_HIC_ICU_0001,
    # 0006 is depricated,
    NIHR_HIC_ICU_0017 = rnorm(ss, 168, 20),
    NIHR_HIC_ICU_0018 = rnorm(ss, 75, 10),
    NIHR_HIC_ICU_0033 = sample(seq.Date(ymd("1935-01-01"), ymd("1990-01-01"), by = 1), ss, TRUE),
    NIHR_HIC_ICU_0073 = generate_nhs(ss),
    NIHR_HIC_ICU_0093 = sample(c("M", "F"), ss, TRUE),
    NIHR_HIC_ICU_0399 = generate_icnarc(ss),
    NIHR_HIC_ICU_0088 = generate_icnarc(ss),
    NIHR_HIC_ICU_0409 = as.integer(rbeta(ss, 2, 8)*100))

# Add in end points
dfs <- dfs %>%
  mutate(NIHR_HIC_ICU_0412 = NIHR_HIC_ICU_0411 + days(rpois(ss, 3))) %>%
  mutate(
    NIHR_HIC_ICU_0412 = if_else(
      NIHR_HIC_ICU_0411 == NIHR_HIC_ICU_0412,
      NIHR_HIC_ICU_0411 + hours(sample(6:18, 1)),
      NIHR_HIC_ICU_0412
    )) %>%
  mutate(NIHR_HIC_ICU_0081 = sample(c("D", "A"), size = ss, prob = c(0.1, 0.9), replace = TRUE)) %>%
  mutate(NIHR_HIC_ICU_0095 = if_else(
    NIHR_HIC_ICU_0081 == "D", "D",
      sample(c("D", "A"), size = 1, prob = c(0.1, 0.9), replace = TRUE))) %>%
  mutate(NIHR_HIC_ICU_0042 = if_else(NIHR_HIC_ICU_0081 == "D",
                                     as.Date(NIHR_HIC_ICU_0412), as.Date(NA)),
         NIHR_HIC_ICU_0043 = if_else(NIHR_HIC_ICU_0081 == "D",
                                     hms::as.hms(NIHR_HIC_ICU_0412), hms::as.hms(ymd_hms(NA))))

## Now lets do some longitudinal data
## Creating the basic time cadance
dfl <- dfs %>%
  select(episode_id, NIHR_HIC_ICU_0411, NIHR_HIC_ICU_0412) %>%
  nest(NIHR_HIC_ICU_0411, NIHR_HIC_ICU_0412) %>%
  mutate(data = map(data, ~seq.POSIXt(.x$NIHR_HIC_ICU_0411, .x$NIHR_HIC_ICU_0412, by = "hour"))) %>%
  unnest(data) %>%
  rename(datetime = data)

dfl <- dfl %>%
  group_by(episode_id) %>%
  arrange(episode_id, datetime) %>%
  mutate(NIHR_HIC_ICU_0108 = as.integer(rnorm(n = n(), mean = 90, sd = 10)),
         NIHR_HIC_ICU_0110 = as.integer(rnorm(n = n(), mean = 65, sd = 10)),
         NIHR_HIC_ICU_0116 = round(rnorm(n = n(), mean = 5, sd = 5), digits = 2),
         NIHR_HIC_ICU_0122 = sample(
           c(round(rgamma(1, 2), digits = 2), as.numeric(NA)),
           size = n(), replace = TRUE, prob = c(0.2, 0.8)),
         NIHR_HIC_ICU_0122 = sample(
           c("E", "N", "T", as.character(NA)),
           size = n(), replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.7)))

dfs <- dfs %>% group_by(episode_id)

## I'm sure there is a better way to do this bit
str_var <- select_if(dfs, is.character) %>%
  na.omit() %>%
  gather(key = "code_name", value = "string", -episode_id)
int_var <- select_if(dfs, is.integer) %>%
  na.omit() %>%
  gather(key = "code_name", value = "integer", -episode_id)
dbl_var <- select_if(dfs, is.numeric) %>%
  na.omit() %>%
  gather(key = "code_name", value = "real", -episode_id)
date_var <- select_if(dfs, is.Date) %>%
  na.omit() %>%
  gather(key = "code_name", value = "date", -episode_id)
dttm_var <- select_if(dfs, is.POSIXct) %>%
  na.omit() %>%
  gather(key = "code_name", value = "datetime", -episode_id)
time_var <- select_if(dfs, hms::is.hms) %>%
  na.omit() %>%
  gather(key = "code_name", value = "time", -episode_id)

dbs <- bind_rows(str_var, int_var, dbl_var, date_var, dttm_var, time_var)

dfl <- dfl %>% group_by(episode_id, datetime)
## Group so episode_id and datetime are brought along for the ride

str_var <- select_if(dfl, is.character) %>%
  na.omit() %>%
  gather(key = "code_name", value = "string", -episode_id, -datetime)
int_var <- select_if(dfl, is.integer) %>%
  na.omit() %>%
  gather(key = "code_name", value = "integer", -episode_id, -datetime)
dbl_var <- select_if(dfl, is.numeric) %>%
  na.omit() %>%
  gather(key = "code_name", value = "real", -episode_id, -datetime)

dbl <- bind_rows(str_var, int_var, dbl_var)
dbf <- bind_rows(dbs, dbl)

# Final modifications since we don't carry metadata at present.

events <- dbf %>%
  ungroup() %>%
  mutate(event_id = 1:n()) %>%
  add_column(string2 = as.character(NA),
             string3 = as.character(NA),
             integer2 = as.integer(NA)) %>%
  select(code_name, string, string2, string3, datetime, date, time, real,
         integer, integer2, episode_id, event_id)

## VARIABLES (metadata) TABLE ====
# Easiest just to pull this from the database directly
load("./data-raw/resources/metadata_tbl.RData")
variables <- variables_tbl

## Write to DB ====
cchic <- DBI::dbConnect(RSQLite::SQLite(), "./data-raw/synthetic_db.sqlite3")

copy_to(cchic, episodes, temporary = FALSE)
copy_to(cchic, events, temporary = FALSE)
copy_to(cchic, provenance, temporary = FALSE)
copy_to(cchic, variables, temporary = FALSE)

db_list_tables(cchic)
DBI::dbDisconnect(cchic)
