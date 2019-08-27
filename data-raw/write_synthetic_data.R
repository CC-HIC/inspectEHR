# Write sample data ====
# This script helps write some fake data for testing.
# It should have structually sound data 2 Hospitals, each with 2 ICUs, with
# between 0 and 10 admissins per day for 2 years.

# - [] Physiology is nonsense
# - [] Coincident observations (like blood pressure) don't line up yet
# - [] not 100% is AMP and CMP numbers are accurate portrayals.
# - [] No meta data

library(tidyverse)
library(lubridate); library(hms)
# library(inspectEHR)
library(devtools)
load_all()

set.seed(20190816)

## PROVENANCE TABLE ====

provenance <- tibble(
  filename = paste0("simulated_files/", 1:2, ".xml"),
  file_id = 1:2,
  date_created = lubridate::now(),
  version = "v8.3.2",
  date_parsed = lubridate::now(),
  site = LETTERS[1:2],
  theme = "ICU",
  notes = as.character(NA)
)

icus <- tibble(
  site = rep(LETTERS[1:2], each = 2),
  icu = c("A-1", "A-2", "B-1", "B-2")
)

## EPISODES TABLE ====
df <- tibble(admission_date = ymd(NULL),
       site = as.character(NULL),
       icu = as.character(NULL))

for (t in seq.Date(ymd("2016-01-01"), ymd("2017-12-31"), "day")) {
  for (site in provenance$site) {
    for (icu in 1:2) {
      new_admissions <- rpois(1, 5)
      df <- tibble(admission_date = rep(as.Date(t, origin = "1970-01-01"), new_admissions),
             site = site,
             icu = paste0(site, "-", icu)) %>%
        bind_rows(df)
    }
  }
}

episodes <- df %>%
  mutate(
    episode_id = 1:n(),
    nhs_number = generate_nhs(nrow(df)),
    start_time = hms(
      sample(0:59, size = nrow(df), replace = TRUE),
      sample(0:59, size = nrow(df), replace = TRUE),
      sample(0:23, size = nrow(df), replace = TRUE)
    )) %>%
  mutate(start_date = ymd_hms(paste(
    format(admission_date), start_time
  ))) %>%
  select(-admission_date, -start_time)

## Remember, to drop the ICU label -> needs to go to the CMP number in the
## events table.

## EVENTS TABLE ====

gp_codes <- read_csv("./data-raw/resources/gp_codes.csv") %>%
  filter(address4 %in% c("LONDON", "CAMBRIDGESHIRE", "OXFORDSHIRE")) %>%
  select(org_code) %>%
  pull()

load("./data-raw/resources/treatment_fx_codes.RData")

# Set up some of the most important base features
dfs <- episodes %>%
  rename(
    NIHR_HIC_ICU_0073 = nhs_number,
    NIHR_HIC_ICU_0411 = start_date,
    NIHR_HIC_ICU_0002 = icu
  ) %>%
  select(-site)

dfs <- dfs %>%
  mutate(
    # Steal the NHS number for PAS number, both are just unique IDs anyway
    NIHR_HIC_ICU_0001 = NIHR_HIC_ICU_0073,
    NIHR_HIC_ICU_0003 = sample(gp_codes, size = nrow(dfs), replace = TRUE),
    NIHR_HIC_ICU_0004 = sample(tfc$code, size = nrow(dfs), replace = TRUE),
    NIHR_HIC_ICU_0005 = paste0("CMP-", episode_id),
    # 0006 is depricated,
    NIHR_HIC_ICU_0010 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0011 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0013 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0015 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.7, 0.3)),
    NIHR_HIC_ICU_0016 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0017 = rnorm(nrow(dfs), 168, 20),
    NIHR_HIC_ICU_0018 = rnorm(nrow(dfs), 75, 10),
    NIHR_HIC_ICU_0024 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0025 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0026 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0029 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0033 = sample(seq.Date(ymd("1935-01-01"), ymd("1990-01-01"), by = 1), nrow(dfs), TRUE),
    NIHR_HIC_ICU_0055 = sample(c("A", "J", "N", "T"), nrow(dfs), TRUE),
    NIHR_HIC_ICU_0058 = sample(c("A", "B", "C", "D", "E", "F", "G", "H", "J",
                                 "K", "L", "M", "N", "P", "R", "S", "Z"), nrow(dfs), TRUE),
    NIHR_HIC_ICU_0060 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0062 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0063 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0070 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0071 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0075 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0080 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0082 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0092 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0093 = sample(c("M", "F"), nrow(dfs), TRUE),
    NIHR_HIC_ICU_0099 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0107 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0399 = generate_icnarc(nrow(dfs)),
    NIHR_HIC_ICU_0088 = generate_icnarc(nrow(dfs)),
    NIHR_HIC_ICU_0409 = as.integer(rbeta(nrow(dfs), 2, 8) * 100)
  )

# Add in end points
dfs <- dfs %>%
  mutate(
    NIHR_HIC_ICU_0412 = NIHR_HIC_ICU_0411 + seconds(rgamma(nrow(dfs), 2, 0.5))*86400) %>%
  # 97 -> unit mortality
  mutate(NIHR_HIC_ICU_0097 = sample(c("D", "A"), size = nrow(dfs), prob = c(0.1, 0.9), replace = TRUE)) %>%
  # 95 -> hospital mortality
  mutate(NIHR_HIC_ICU_0095 = if_else(
    NIHR_HIC_ICU_0097 == "D", "D",
    sample(c("D", "A"), size = 1, prob = c(0.1, 0.9), replace = TRUE)
  )) %>%
  # Make the datetime of death the same as the discharge
  mutate(
    NIHR_HIC_ICU_0042 = if_else(NIHR_HIC_ICU_0097 == "D",
      as.Date(NIHR_HIC_ICU_0412), as.Date(NA)
    ),
    NIHR_HIC_ICU_0043 = if_else(NIHR_HIC_ICU_0097 == "D",
      hms::as_hms(NIHR_HIC_ICU_0412), hms::as_hms(ymd_hms(NA))
    )
  )

## Now lets do some longitudinal data
## Creating the basic time cadance
dfl <- dfs %>%
  select(episode_id, NIHR_HIC_ICU_0411, NIHR_HIC_ICU_0412) %>%
  nest(NIHR_HIC_ICU_0411, NIHR_HIC_ICU_0412) %>%
  mutate(data = map(data, ~ seq.POSIXt(.x$NIHR_HIC_ICU_0411, .x$NIHR_HIC_ICU_0412, by = "hour"))) %>%
  unnest(data) %>%
  rename(datetime = data)

dfl <- dfl %>%
  group_by(episode_id) %>%
  arrange(episode_id, datetime) %>%
  mutate(
    # HR
    NIHR_HIC_ICU_0108 = as.integer(rnorm(n = n(), mean = 90, sd = 10)),
    # H-Rhythm
    NIHR_HIC_ICU_0109 = sample(1:31, n(), TRUE),
    # MAP-Inv
    NIHR_HIC_ICU_0110 = as.integer(rnorm(n = n(), mean = 75, sd = 10)),
    # Map-NIBP (Add some missingness)
    NIHR_HIC_ICU_0111 = sample(
      c(as.integer(rnorm(1, mean = 75, sd = 10)), as.integer(NA)),
      n(), replace = TRUE, prob = c(0.2, 0.8)),
    # Sys-Inv
    NIHR_HIC_ICU_0112 = as.integer(rnorm(n = n(), mean = 110, sd = 10)),
    # Sys-NIBP (Add some missingness)
    NIHR_HIC_ICU_0113 = sample(
      c(as.integer(rnorm(1, mean = 110, sd = 10)), as.integer(NA)),
      n(), replace = TRUE, prob = c(0.2, 0.8)),
    # Dia-Inv
    NIHR_HIC_ICU_0114 = as.integer(rnorm(n = n(), mean = 60, sd = 10)),
    # Dia-NIBP (add some missingness)
    NIHR_HIC_ICU_0115 = sample(
      c(as.integer(rnorm(1, mean = 60, sd = 10)), as.integer(NA)),
      n(), replace = TRUE, prob = c(0.2, 0.8)),
    NIHR_HIC_ICU_0116 = round(rnorm(n = n(), mean = 5, sd = 5), digits = 2),
    NIHR_HIC_ICU_0122 = sample(
      c(round(rgamma(1, 2), digits = 2), as.numeric(NA)),
      size = n(), replace = TRUE, prob = c(0.2, 0.8)
    ),
    NIHR_HIC_ICU_0126 = sample(
      c("E", "N", "T", as.character(NA)),
      size = n(), replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.7)
    )
  )

dfs <- dfs %>% group_by(episode_id)

spells <- dfs %>%
  ungroup() %>%
  filter(NIHR_HIC_ICU_0097 == "A",
         NIHR_HIC_ICU_0095 == "A") %>%
  sample_n(size = as.integer(0.1*n())) %>%
  mutate(NIHR_HIC_ICU_0411 = NIHR_HIC_ICU_0412 + seconds(300)) %>%
  mutate(NIHR_HIC_ICU_0412 = NIHR_HIC_ICU_0411 + seconds(rgamma(n(), 2, 0.5))*86400) %>%
  mutate(NIHR_HIC_ICU_0097 = sample(c("D", "A"), size = n(), prob = c(0.1, 0.9), replace = TRUE)) %>%
  mutate(NIHR_HIC_ICU_0095 = if_else(
    NIHR_HIC_ICU_0097 == "D", "D",
    sample(c("D", "A"), size = 1, prob = c(0.1, 0.9), replace = TRUE)
  )) %>%
  mutate(
    NIHR_HIC_ICU_0042 = if_else(NIHR_HIC_ICU_0097 == "D",
                                as.Date(NIHR_HIC_ICU_0412), as.Date(NA)
    ),
    NIHR_HIC_ICU_0043 = if_else(NIHR_HIC_ICU_0097 == "D",
                                hms::as_hms(NIHR_HIC_ICU_0412), hms::as_hms(ymd_hms(NA))
    )
  ) %>%
  mutate(episode_id = (max(dfs$episode_id)+1):(max(dfs$episode_id)+n()))

dfs <- bind_rows(dfs, spells)

spells_l <- spells %>%
  select(episode_id, NIHR_HIC_ICU_0411, NIHR_HIC_ICU_0412) %>%
  nest(NIHR_HIC_ICU_0411, NIHR_HIC_ICU_0412) %>%
  mutate(data = map(data, ~ seq.POSIXt(.x$NIHR_HIC_ICU_0411, .x$NIHR_HIC_ICU_0412, by = "hour"))) %>%
  unnest(data) %>%
  rename(datetime = data)

spells_l <- spells_l %>%
  group_by(episode_id) %>%
  arrange(episode_id, datetime) %>%
  mutate(
    # HR
    NIHR_HIC_ICU_0108 = as.integer(rnorm(n = n(), mean = 90, sd = 10)),
    # H-Rhythm
    NIHR_HIC_ICU_0109 = sample(1:31, n(), TRUE),
    # MAP-Inv
    NIHR_HIC_ICU_0110 = as.integer(rnorm(n = n(), mean = 75, sd = 10)),
    # Map-NIBP (Add some missingness)
    NIHR_HIC_ICU_0111 = sample(
      c(as.integer(rnorm(1, mean = 75, sd = 10)), as.integer(NA)),
      n(), replace = TRUE, prob = c(0.2, 0.8)),
    # Sys-Inv
    NIHR_HIC_ICU_0112 = as.integer(rnorm(n = n(), mean = 110, sd = 10)),
    # Sys-NIBP (Add some missingness)
    NIHR_HIC_ICU_0113 = sample(
      c(as.integer(rnorm(1, mean = 110, sd = 10)), as.integer(NA)),
      n(), replace = TRUE, prob = c(0.2, 0.8)),
    # Dia-Inv
    NIHR_HIC_ICU_0114 = as.integer(rnorm(n = n(), mean = 60, sd = 10)),
    # Dia-NIBP (add some missingness)
    NIHR_HIC_ICU_0115 = sample(
      c(as.integer(rnorm(1, mean = 60, sd = 10)), as.integer(NA)),
      n(), replace = TRUE, prob = c(0.2, 0.8)),
    NIHR_HIC_ICU_0116 = round(rnorm(n = n(), mean = 5, sd = 5), digits = 2),
    NIHR_HIC_ICU_0122 = sample(
      c(round(rgamma(1, 2), digits = 2), as.numeric(NA)),
      size = n(), replace = TRUE, prob = c(0.2, 0.8)
    ),
    NIHR_HIC_ICU_0126 = sample(
      c("E", "N", "T", as.character(NA)),
      size = n(), replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.7)
    )
  )

dfl <- bind_rows(dfl, spells_l)

## I'm sure there is a better way to do this bit
str_var <- select_if(dfs, is.character) %>%
  gather(key = "code_name", value = "string", -episode_id) %>%
  na.omit()
int_var <- select_if(dfs, is.integer) %>%
  gather(key = "code_name", value = "integer", -episode_id) %>%
  na.omit()
# Custom select to ensure we are only getting the stuff we want
dbl_var <- select_if(dfs, function(x) {
  is.double(x) && !is.POSIXct(x) && !hms::is_hms(x) && !lubridate::is.Date(x)
}) %>%
  gather(key = "code_name", value = "real", -episode_id) %>%
  na.omit()
date_var <- select_if(dfs, is.Date) %>%
  gather(key = "code_name", value = "date", -episode_id) %>%
  na.omit()
dttm_var <- select_if(dfs, is.POSIXct) %>%
  gather(key = "code_name", value = "datetime", -episode_id) %>%
  na.omit()
time_var <- select_if(dfs, hms::is.hms) %>%
  gather(key = "code_name", value = "time", -episode_id) %>%
  na.omit()

dbs <- bind_rows(str_var, int_var, dbl_var, date_var, dttm_var, time_var)

dfl <- dfl %>% group_by(episode_id, datetime)
## Group so episode_id and datetime are brought along for the ride

str_var <- select_if(dfl, is.character) %>%
  gather(key = "code_name", value = "string", -episode_id, -datetime) %>%
  na.omit()
int_var <- select_if(dfl, is.integer) %>%
  gather(key = "code_name", value = "integer", -episode_id, -datetime) %>%
  na.omit()
dbl_var <- select_if(dfl, function(x) {
  is.double(x) && !is.POSIXct(x) && !hms::is_hms(x) && !lubridate::is.Date(x)
}) %>%
  gather(key = "code_name", value = "real", -episode_id, -datetime) %>%
  na.omit()

dbl <- bind_rows(str_var, int_var, dbl_var)
dbf <- bind_rows(dbs, dbl)

# Final modifications since we don't carry metadata at present.

events <- dbf %>%
  ungroup() %>%
  mutate(event_id = 1:n()) %>%
  add_column(
    string2 = as.character(NA),
    string3 = as.character(NA),
    integer2 = as.integer(NA)
  ) %>%
  select(
    code_name, string, string2, string3, datetime, date, time, real,
    integer, integer2, episode_id, event_id
  )

## VARIABLES (metadata) TABLE ====
# Easiest just to pull this from the database directly
load("./data-raw/resources/metadata_tbl.RData")
variables <- variables_tbl

## Write to DB ====

# Change dates, datetimes and times into a string as sqlite3 doesn't have these as
# native types.
episodes <- episodes %>% mutate_if(is.POSIXct, format) %>%
  mutate(provenance = if_else(site == "A", 1, 2)) %>%
  select(-site, -icu)

## This takes an annoyingly long time.
## We could just store this data in numeric form, but I prefer to have the SQLite
## datetimes in string format so they are readable without parsing.

events <- events %>%
  mutate_if(lubridate::is.Date, format) %>%
  mutate_if(is.POSIXct, format) %>%
  mutate_if(hms::is.hms, function(x) if_else(is.na(x), as.character(NA), format(x)))

provenance <- provenance %>% mutate_if(is.POSIXct, format)

episode_demo <- episodes %>%
  arrange(start_date) %>%
  `[`(1:1000,)

events_demo <- events %>%
  filter(episode_id %in% episode_demo$episode_id)

## DBI cannot send multi-line SQL statements
## System is not working, so annoyingly we need to leave this session
## to run `sqlite3 ./data-raw/synthetic_db.sqlite3 --init ./data-raw/db_setup.sql`
## and `sqlite3 ~/_data/hic/public/full_synthetic_db.sqlite3 --init ./data-raw/db_setup.sql`

cchic_demo <- DBI::dbConnect(RSQLite::SQLite(), "./data-raw/synthetic_db.sqlite3")
cchic_full <- DBI::dbConnect(RSQLite::SQLite(), "~/_data/hic/public/full_synthetic_db.sqlite3")

## Write tables in the following order, as foreign key constraints
## have to be set up at the time of writing the tables in sqlite

## Variables
## Provenance
## Episodes
## Events

DBI::dbListTables(cchic_demo)
DBI::dbListTables(cchic_full)

## Variables ====

DBI::dbWriteTable(
  conn = cchic_demo,
  name = "variables",
  value = variables, append = TRUE)

DBI::dbWriteTable(
  conn = cchic_full,
  name = "variables",
  value = variables, append = TRUE)

## Provenance

DBI::dbWriteTable(
  conn = cchic_demo,
  name = "provenance",
  value = provenance, append = TRUE)

DBI::dbWriteTable(
  conn = cchic_full,
  name = "provenance",
  value = provenance, append = TRUE)

## Episodes ====

DBI::dbWriteTable(
  conn = cchic_demo,
  name = "episodes",
  value = episode_demo, append = TRUE)

DBI::dbWriteTable(
  conn = cchic_full,
  name = "episodes",
  value = episodes, append = TRUE)

## Events ====

DBI::dbWriteTable(
  conn = cchic_demo,
  name = "events",
  value = events_demo, append = TRUE)

DBI::dbWriteTable(
  conn = cchic_full,
  name = "events",
  value = events, append = TRUE)

DBI::dbDisconnect(cchic_demo)
DBI::dbDisconnect(cchic_full)
rm(cchic_demo, cchic_full)
