#' Validate NHS Numbers
#'
#' This function will validate nhs numbers according to the checksum process
#' outlined by UK Government Data Standards Catalogue (GDSC), Version 2.0, Agreed 01.01.02
#' availible as reference here: https://www.datadictionary.nhs.uk/version2/data_dictionary/data_field_notes/n/nhs_number_de.asp?shownav=0
#'
#' @param nhs_numbers as character vector
#'
#' @return a logical vector
#' @export
#'
#' @examples
#' validate_nhs("6744424270") # expect TRUE
validate_nhs <- function(nhs_numbers = NULL) {

  if (class(nhs_numbers) != "character") stop("Please enter NHS Numbers as characters")

  response <- vector(mode = "logical", length = length(nhs_numbers))
  nhs <- as.character(nhs_numbers)

  for (entry in seq_along(nhs)) {

    if (is.na(nhs[entry])) {

      response[entry] <- as.logical(NA)

    } else if (nchar(nhs[entry]) != 10 | !(grepl("^[0-9]+$", nhs[entry]))) {

      response[entry] <- FALSE

    } else if (nhs[entry] == "0000000000") {

      response[entry] <- FALSE

    } else {

      test_sequence <- as.integer(strsplit(nhs[entry], NULL)[[1]])
      store <- vector(mode = "integer", length = 9)

      for (i in 1:9) store[i] <- test_sequence[i]*(11-i)

      store <- sum(store)
      remainder <- store %% 11
      check <- 11 - remainder

      if (check == 11) {

        response[entry] <- TRUE

      } else {

        response[entry] <- (test_sequence[10] == check)

      }
  }
  }

  return(response)

}



validate_post_code <- function(post_code = NULL) {

  test_pattern <- paste0("^((([A-Za-z][0-9]{1,2})|",
                          "(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|",
                          "(([A-Za-z][0-9][A-Za-z])|",
                          "([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z]))))\\s?[0-9][A-Za-z]{2})|",
                          "([A-Za-z][0-9]{1,2})$")

  result <- grepl(pattern = test_pattern, x = post_code)

  return(result)

}


validate_icnarc <- function(icnarc_code = NULL) {

  test_pattern <- "([1-2].[1-12].[1-13].[1-54].[1-17])"
  result <- grepl(pattern = test_pattern, x = icnarc_code)

  return(result)

}

