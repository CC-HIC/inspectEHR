# Lets make some sample data

# categorical
df_1d <- tibble(
  internal_id = 1:5000,
  episode_id = sample(1:250, 5000, replace = TRUE),
  site = rep(c("UCL", "Imperial", "Oxford", "GSTT", "Cambridge"), each = 1000),
  code_name = "NHIC_demo",
  value = rbinom(5000, prob = 0.8, size = 1),
  out_of_bounds = rep(0L, 5000),
  duplicate = rep(0L, 5000),
  range_error = rep(0L, 5000)
)

attr(df_1d, "code_name") <- "NIHR_HIC_ICU_0026"
class(df_1d) <- append(class(df_1d), "string_1d", after = 0)

#=======

# date
df_date <- tibble(
  internal_id = 1:5000,
  episode_id = sample(1:250, 5000, replace = TRUE),
  site = rep(c("UCL", "Imperial", "Oxford", "GSTT", "Cambridge"), each = 1000),
  code_name = "NIHR_HIC_ICU_0406",
  value = rep(
    seq.Date(from = ymd("2014-01-01"), to = ymd("2016-09-26"), by = "day"), 5),
  out_of_bounds = rep(0L, 5000),
  duplicate = rep(0L, 5000),
  range_error = rep(0L, 5000)
)

attr(df02, "code_name") <- "NIHR_HIC_ICU_0406"
class(df02) <- append(class(df02), "date_1d", after = 0)



#=======

# numeric 2d
df_numeric_2d <- tibble(
  internal_id = 1:63000,
  episode_id = rep(1:500, each = 126),
  site = rep(c("UCL", "RYJ", "Oxford", "GSTT", "RGT"), each = 12600),
  code_name = "NIHR_HIC_ICU_0108",
  date = rep(
    seq.POSIXt(from = ymd_hms("2014-01-01 00:00:00",
                              tz = "Europe/London"),
               to = ymd_hms("2014-01-06 05:00:00",
                            tz = "Europe/London"),
               by = "hour"),
    500),
  value = replicate(n = 500, expr = diffinv(rnorm(125)) + rnorm(mean = 100, sd = 10, n = 1)) %>% as.vector %>% as.integer()
)

attr(df_numeric_2d, "code_name") <- "NIHR_HIC_ICU_0108"
class(df_numeric_2d) <- append(class(df_numeric_2d), "integer_2d", after = 0)

df_numeric_2d %>%
  ggplot(aes(group = episode_id,
             y = value,
             x = date,
             colour = episode_id)) + geom_line(alpha = 0.5)



length(rep(0:500, each = 125))
125*500

plot(diffinv(rnorm(1000)))


plot(x)

x <- filter(rnorm(1000), filter=rep(1,3), circular=TRUE)
?diffinv
plot(x)
