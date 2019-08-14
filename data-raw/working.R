library(devtools)
library(rlang)
load_all()

filter_func <- function(x, filter_term) {
    filter_term <- enquo(filter_term)
    filter_exp_enq <- enquo(hp == filter_term)
    x %>%
      filter(!!filter_exp_enq)
  }

filter_func(mtcars, 93)

ctn <- connect(sqlite_file = "./data-raw/synthetic_db.sqlite3")
ltb <- extract_timevarying(ctn, "NIHR_HIC_ICU_0108")

col_name <- "NIHR_HIC_ICU_0108"
quo_name <- ensym(col_name)
rlang::qq_show(
rng <- qref %>%
    filter(code_name == !!quo_name) %>%
    select(range_min, range_max) %>%
    unlist()
)

ltb <- ltb %>%
  mutate(NIHR_HIC_ICU_0108 = as.integer(rnorm(nrow(ltb), 0, 100)))

ltb %>%
  filter(NIHR_HIC_ICU_0108 < 0)


col_name <- enquo(NIHR_HIC_ICU_0108)

rlang::qq_show(
ltb <- ltb %>%
  mutate(new = if_else(
    !!col_name < rng["range_min"] | !!col_name > rng["range_max"],
    1, 0))
)

tt
tt <- clean_me(ltb, "NIHR_HIC_ICU_0108", "NA")

tt

tt %>%
  filter(NIHR_HIC_ICU_0108 < 0)

cleaning_helper <- function(ltb, col_name, action = "NA") {

  rng <- qref %>%
    filter(.data$code_name == col_name) %>%
    select(.data$range_min, .data$range_max) %>%
    unlist()

  type_change <- qref %>%
    filter(.data$code_name == col_name) %>%
    select(.data$primary_column) %>%
    pull()

  if (type_change == "integer") {
    type_change <- as.integer(NA)
  } else if (type_change == "real") {
    type_change <- as.numeric(NA)
  } else {
    rlang::abort("Methods are not yet defined for this data type")
  }

  if (action == "NA") {
    ltb <- ltb %>%
      mutate(new = if_else(
        .data[[col_name]] < rng["range_min"] | .data[[col_name]] > rng["range_max"],
        type_change, .data[[col_name]]))
  } else if (action == "limits") {
    ltb <- ltb %>%
      mutate(new = case_when(
        .data[[col_name]] < rng["range_min"] ~ rng["range_min"],
        .data[[col_name]] > rng["range_max"] ~ rng["range_max"],
        TRUE ~ .data[[col_name]]
      )
      )
  }

  return(ltb)

}

sample(c(0:1), size = 1000, replace = TRUE)
