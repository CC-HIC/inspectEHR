test_episode_length <- episode_length[1:10,]

test_episode_ids <- test_episode_length$episode_id

test_date_1d <- test_date_1d %>% filter(episode_id %in% test_episode_ids)
test_datetime_1d <- test_datetime_1d %>% filter(episode_id %in% test_episode_ids)
test_integer_1d <- test_integer_1d %>% filter(episode_id %in% test_episode_ids)
test_integer_2d <- test_integer_2d %>% filter(episode_id %in% test_episode_ids)
test_real_1d <- test_real_1d %>% filter(episode_id %in% test_episode_ids)
test_real_2d <- test_real_2d %>% filter(episode_id %in% test_episode_ids)
test_string_1d <- test_string_1d %>% filter(episode_id %in% test_episode_ids)
test_string_2d <- test_string_2d %>% filter(episode_id %in% test_episode_ids)
test_time_1d <- test_time_1d %>% filter(episode_id %in% test_episode_ids)

anti_join(test_episode_length, test_real_2d)
anti_join(test_episode_length, test_string_2d)
anti_join(test_episode_length, test_time_1d)

test_episode_length$epi_start_dttm <- ymd_hms("2018-01-01 12:00:00")
test_episode_length$epi_end_dttm <- c(
  ymd_hms("2018-01-02 12:00:00"),
  ymd_hms("2018-01-02 12:00:00"),
  ymd_hms("2018-01-02 12:00:00"),
  ymd_hms("2018-01-02 12:00:00"),
  ymd_hms("2018-01-02 12:00:00"),
  ymd_hms("2018-01-03 12:00:00"),
  ymd_hms("2018-01-03 12:00:00"),
  NA,
  ymd_hms("2017-12-31 12:00:00"),
  ymd_hms("2018-01-01 12:00:00")
)

test_episode_length$los <- difftime(test_episode_length$epi_end_dttm, test_episode_length$epi_start_dttm, units = "days")
test_episode_length$validity <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 2L, 2L)

test_episode_length

test_date_1d$value <- c(
  ymd("1800-01-01"), #too low
  ymd("2020-08-18"), #too high
  ymd("1900-01-01"), #borderline case
  ymd("1987-08-18"),
  ymd("1987-08-18"),
  ymd("1987-08-18"),
  ymd("1987-08-18"),
  ymd("1987-08-18"),
  ymd("1987-08-18"),
  ymd("1987-08-18")
)

test_datetime_1d$value <- test_episode_length$epi_start_dttm

test_integer_1d$value <- c(370L, -1L, 0L, 4L:10L)

# test_integer_2d

test_integer_2d[1, "value"] <- 350L
test_integer_2d[2, "value"] <- -1L
test_integer_2d[3, "value"] <- 5L

test_integer_2d$datetime <- seq(from=as.POSIXct("2018-01-01 12:00:01", format="%Y-%m-%d %H:%M:%OS",tz="GMT"),
    to=as.POSIXct("2018-01-01 12:16:02", format="%Y-%m-%d %H:%M:%OS", tz="GMT"), by=1)

test_integer_2d[4, "datetime"] <- as.POSIXct("2018-01-05 12:16:02", format="%Y-%m-%d %H:%M:%OS", tz="GMT")
test_integer_2d[5, "datetime"] <- as.POSIXct("2017-12-31 12:16:02", format="%Y-%m-%d %H:%M:%OS", tz="GMT")
test_integer_2d[6, "datetime"] <- as.POSIXct("2018-01-01 12:00:00", format="%Y-%m-%d %H:%M:%OS", tz="GMT")

# test_real_1d

test_real_1d$value <- c(1.838394, 1.661010, 1.764674, 1.793699, 1.766515, 1.585913, 1.482639, 2.080445, 1.365278, 1.916585)

test_real_1d[1, "value"] <- 3.5
test_real_1d[2, "value"] <- 0.4
test_real_1d[3, "value"] <- 0.8

# test_real_2d

temp_id <- anti_join(test_episode_length, test_real_2d) %>% select(episode_id) %>% pull
temp_2d <- test_real_2d[1:8,]
temp_2d$episode_id <- temp_id
test_real_2d <- bind_rows(test_real_2d, temp_2d)

test_real_2d$datetime <- seq(from=as.POSIXct("2018-01-01 12:00:01", format="%Y-%m-%d %H:%M:%OS",tz="GMT"),
                                to=as.POSIXct("2018-01-01 12:00:48", format="%Y-%m-%d %H:%M:%OS", tz="GMT"), by=1)

test_real_2d[1, "value"] <- 100
test_real_2d[2, "value"] <- 0
test_real_2d[3, "value"] <- 0.01
test_real_2d[4, "datetime"] <- as.POSIXct("2018-01-05 12:16:02", format="%Y-%m-%d %H:%M:%OS", tz="GMT")
test_real_2d[5, "datetime"] <- as.POSIXct("2017-12-31 12:16:02", format="%Y-%m-%d %H:%M:%OS", tz="GMT")
test_real_2d[6, "datetime"] <- as.POSIXct("2018-01-01 12:00:00", format="%Y-%m-%d %H:%M:%OS", tz="GMT")

# test_string_1d

test_string_1d[1, "value"] <- "Q"

# test_string_2d

temp_id <- anti_join(test_episode_length, test_string_2d) %>% select(episode_id) %>% pull
temp_2d <- test_string_2d[1,]

test_string_2d <- bind_rows(test_string_2d, test_string_2d, test_string_2d, temp_2d)

test_string_2d$episode_id <- test_episode_length$episode_id

test_string_2d$value <- c("E", "E", "E", "N", "N", "N", "T", "Q", NA, "E")

test_string_2d$datetime <- as.POSIXct("2018-01-01 13:00:00", format="%Y-%m-%d %H:%M:%OS", tz="GMT")
test_string_2d[4, "datetime"] <- as.POSIXct("2018-01-05 12:16:02", format="%Y-%m-%d %H:%M:%OS", tz="GMT")
test_string_2d[5, "datetime"] <- as.POSIXct("2017-12-31 12:16:02", format="%Y-%m-%d %H:%M:%OS", tz="GMT")
test_string_2d[6, "datetime"] <- as.POSIXct("2018-01-01 12:00:00", format="%Y-%m-%d %H:%M:%OS", tz="GMT")

test_time_1d <- bind_rows(test_time_1d, test_time_1d, test_time_1d, test_time_1d, test_time_1d)

test_time_1d$value <- "12:00:00"
test_time_1d$episode_id <- test_episode_length$episode_id

# rounding it off

test_string_1d$internal_id <- 1L:10L
test_string_2d$internal_id <- 1L:10L

test_real_1d$internal_id <- 1L:10L
test_real_2d$internal_id <- 1L:length(test_real_2d$internal_id)
test_integer_1d$internal_id <- 1L:10L
test_integer_2d$internal_id <- 1L:length(test_integer_2d$internal_id)

test_date_1d$internal_id <- 1L:10L
test_datetime_1d$internal_id <- 1L:10L
test_time_1d$internal_id <- 1L:10L

class(test_date_1d) <- append(class(test_date_1d), "date_1d", after = 0)
attr(test_date_1d, "code_name") <- "NIHR_HIC_ICU_0033"
class(test_datetime_1d) <- append(class(test_datetime_1d), "datetime_1d", after = 0)
attr(test_datetime_1d, "code_name") <- "NIHR_HIC_ICU_0411"
class(test_integer_1d) <- append(class(test_integer_1d), "integer_1d", after = 0)
attr(test_integer_1d, "code_name") <- "NIHR_HIC_ICU_0087"
class(test_integer_2d) <- append(class(test_integer_2d), "integer_2d", after = 0)
attr(test_integer_2d, "code_name") <- "NIHR_HIC_ICU_0108"
class(test_real_1d) <- append(class(test_real_1d), "real_1d", after = 0)
attr(test_real_1d, "code_name") <- "NIHR_HIC_ICU_0017"
class(test_real_2d) <- append(class(test_real_2d), "real_2d", after = 0)
attr(test_real_2d, "code_name") <- "NIHR_HIC_ICU_0132"
class(test_string_1d) <- append(class(test_string_1d), "string_1d", after = 0)
attr(test_string_1d, "code_name") <- "NIHR_HIC_ICU_0058"
class(test_string_2d) <- append(class(test_string_2d), "string_2d", after = 0)
attr(test_string_2d, "code_name") <- "NIHR_HIC_ICU_0126"
class(test_time_1d) <- append(class(test_time_1d), "time_1d", after = 0)
attr(test_time_1d, "code_name") <- "NIHR_HIC_ICU_0043"


save(test_episode_length, test_integer_1d, test_integer_2d, test_real_1d, test_real_2d, test_string_1d,
     test_string_2d, test_time_1d, test_date_1d, test_datetime_1d, file = "./tests/test_data.RData")

#load("./tests/test_data.RData")



