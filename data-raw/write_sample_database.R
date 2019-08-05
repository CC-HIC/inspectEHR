load("~/Documents/_data/hic/dummy_data.RData")

events <- events %>% select(-row.names)
events <- events %>%
  mutate(episode_id = str_sub(episode_id, 1, 8))
events <- events %>%
  select(code_name, string, string2, string3, datetime, date, time, real, integer, integer2, episode_id) %>%
  mutate(event_id = row_number())

episode_ids <- events %>% select(episode_id) %>% distinct() %>% pull()
nhs <- c("0083041737",
         "2401060723",
         "6023887112",
         "6744424270",
         "3397862965",
         "3635728896",
         "4710683492",
         "9807779731",
         "2925091350",
         "7101653952",
         "4122566339",
         "4801154492",
         "1309780056",
         "8276746381",
         "5900102912",
         "1293242950",
         "3434140808",
         "6806486851",
         "5176929511",
         "0571638384",
         "1760779016",
         "5108629648",
         "4653111359",
         "3795104742",
         "7203112438",
         "6257926130",
         "9000498899",
         "2928547401",
         "4968276753",
         "3680531052",
         "8914673170",
         "7633351039",
         "6255597423",
         "2634075944",
         "2032009714",
         "4681033006",
         "2231714051",
         "3365830898",
         "0414688287",
         "4059197262",
         "2579555315",
         "4099396002",
         "2735997510",
         "2318355754",
         "0441768180",
         "2655537424",
         "6381091266",
         "5287279361",
         "7097681586",
         "6788082847",
         "2579555315",
         "4099396002",
         "2735997510",
         "2318355754",
         "0441768180",
         "2655537424",
         "6381091266",
         "5287279361",
         "7097681586",
         "6788082847",
         "5044610224",
         "2851586475",
         "4628372500",
         "6022622689",
         "6346713299",
         "1055815147",
         "0448897032",
         "4558869428",
         "4880417149",
         "5011757226",
         "0381296970",
         "9758651986",
         "8794300038",
         "5880868192",
         "3436431338",
         "3775000526",
         "3697116010",
         "8899056846",
         "4226207734",
         "6078924559",
         "9470540689",
         "4507857368",
         "6763733852",
         "1630133035",
         "8543050081",
         "4336381364",
         "1758015942",
         "7814781191",
         "8971603712",
         "4531143450",
         "4151184066",
         "0271027673",
         "4392710335",
         "7299704897",
         "3133184997",
         "8386144033",
         "8469792326",
         "4664596642",
         "3311471644",
         "0094349290")

provenance <- tibble(filename = c("/data/site_a.xml",
                                      "/data/site_b.xml",
                                      "/data/site_c.xml",
                                      "/data/site_d.xml",
                                      "/data/site_e.xml"),
                       file_id = 1L:5L,
                       date_created = Sys.time(),
                       version = "8.3.2",
                       site = c("site_a", "site_b", "site_c", "site_d", "site_e"),
                       theme =  "ICU",
                       notes = as.character(NA))

episodes <- tibble(episode_id = episode_ids,
                   nhs_number = nhs,
                   start_date = ymd_hms("2000-01-01 00:00:00"),
                   provenance = rep(1:5, each = 20))

# Now go and write a new test database with the sql file here.
ctn <- connect(system = "sqlite", file = "./data/test_database.sqlite3")

provenance <- provenance %>%
  mutate(date_created = 17744.64)

episodes <- episodes %>%
  mutate(start_date = as.numeric(julian.POSIXt(start_date)))

events <- events %>%
  mutate(time = if_else(!is.na(time), paste0("1970-01-01 ", time), as.character(NA))) %>%
  mutate(time = if_else(!is.na(time), as.numeric(julian.POSIXt(ymd_hms(time))), as.numeric(NA))) %>%
  mutate(date = if_else(!is.na(date), as.numeric(julian.Date(date)), as.numeric(NA))) %>%
  mutate(datetime = if_else(!is.na(datetime), as.numeric(julian.POSIXt(datetime)), as.numeric(NA)))

ctn <- connect(system = "sqlite", file = "./data/test_database.sqlite3")

dbWriteTable(ctn, "provenance", provenance, append = TRUE)
dbWriteTable(ctn, "episodes", episodes, append = TRUE)
dbWriteTable(ctn, "events", events, append = TRUE)
dbWriteTable(ctn, "variables", variables, append = TRUE)

# check it went ok
tbls <- retrieve_tables(ctn)

write_csv(episodes, path = "./episodes.csv")
