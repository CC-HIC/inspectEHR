
#' XML Parsing Errors
#'
#' Interrogates and summarises XML parsing errors
#'
#' @param importstats uncollected importstats postGres table
#'
#' @return a tibble containing summary of XML errors
#' @export
#'
#' @examples
#' xml_stats(tbls[["importstats"]])
xml_stats <- function(importstats) {

  collect(importstats) %>%
  left_join(provenance %>%
              select(file_id, site), by = c("provenance_id" = "file_id")) %>%
  group_by(site) %>%
  summarise(episodes_read = sum(episodes_read),
     episodes_missing_key = sum(episodes_missing_key),
           events_dropped = sum(events_dropped))

}



#' Checks validation for XML files and returns a summary
#'
#' @param xml_file the xml file you wish to validate
#' @param schema the xml schema against which you are validating
#'
#' @return a character vector of linting errors
#' @export
#'
#' @importFrom xml2 read_xml xml_validate
#' @importFrom magrittr %>%
#' @importFrom stringr str_sub
#'
#' @examples
#' xml_validation(xml, schema)
xml_validation <- function(xml_file, schema) {

  xml_file_x <- xml2::read_xml(xml_file)

  validation <- xml2::xml_validate(xml_file_x, schema)

  if (validation == TRUE) {
    return(TRUE)
  } else {

  errors <- validation %>%
      base::attr(which = "errors") %>%
      stringr::str_sub(start = 54L, end = -1L)

   return(errors)

  }

}


#' Summarise Event Level Errors
#'
#' returns a summary table grouped by site with a breakdown of the errors seen.
#' This is for overview purposes, so scope out the magnitude of problems and help
#' direct xml fixes
#'
#' @param errors error table
#' @param provenance provenance table
#'
#' @return a tibble with summary details of errors
#' @export
#'
#' @import dplyr
#'
#' @examples
#' error_summary(xml_validation)
error_summary <- function(xml_validation) {

  xml_validation %>%
    dplyr::left_join(provenance %>% select(file_id, site), by = c("provenance_id" = "file_id")) %>%
    dplyr::mutate(message_type = dplyr::case_when(
      grepl("clock change", message) ~ "Daylight saving bug",
      grepl("nhs number", message) ~ "Missing NHS Number",
      grepl("empty tags|Empty tags", message) ~ "Empty XML Tags",
      grepl("primary value", message) ~ "Missing Primary Value",
      grepl("Rejected patient", message) ~ "Patient Rejected (Other)",
      grepl("integer value is out of range", message) ~ "Numeric Out of Range",
      grepl("Got float, expected int", message) ~ "Wrong Data Type",
      grepl("not a valid value of the atomic type \'xs:integer\'", message) ~ "Wrong Data Type",
      grepl("could not convert string to float", message) ~ "Wrong Data Type",
      grepl("is not an element of the set", message) ~ "Wrong Data Type",
      TRUE ~ "Other")) %>%
    dplyr::group_by(site, message_type) %>%
    dplyr::summarise(count = n())

}





