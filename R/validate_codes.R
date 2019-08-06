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


generate_nhs <- function(size = 1) {

  nhs_numbers <- vector(mode = "character", length = size)

  for (j in seq_len(size)) {

    valid <- FALSE

    while (!valid) {

      start_seq <- sample(x = 1:9, size = 9, replace = TRUE)
      response <- vector(mode = "integer", length = 9)
      multiplier <- 10

      for (i in seq_along(response)) {
        response[i] <- start_seq[i] * multiplier
        multiplier <- multiplier - 1
      }

      totals <- sum(response)
      residual <- totals %% 11
      last_digit <- 11 - residual

      if (last_digit == 11) {
        last_digit <- 0
      } else if (last_digit == 10) {
        next
      }

    valid <- TRUE
    }

    nhs_numbers[j] <- paste0(c(start_seq, last_digit), collapse = "")

    }

    return(nhs_numbers)
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

  icnarc <- stringr::str_split(icnarc_code, fixed(".")) %>%
    purrr::map(as.integer) %>%
    purrr::map(function(x) {
      if (length(x) != 5) {
        return(FALSE)
      } else {
        pos_a <- x[1] %in% c(1:2)
        pos_b <- x[2] %in% c(1:12)
        pos_c <- x[3] %in% c(1:13)
        pos_d <- x[4] %in% c(1:54)
        pos_e <- x[5] %in% c(1:17)
        result <- all(c(pos_a, pos_b, pos_c, pos_d, pos_e))
        return(result)
      }
    }) %>%
    unlist()

  return(icnarc)

}


generate_icnarc <- function(size = 1) {

  icnarc <- vector(mode = "character", length = size)

  for (i in seq_len(size)) {

    x <- vector(mode = "integer", length = 5)

    x[1] <- sample(1:2, size = 1)
    x[2] <- sample(1:12, size = 1)
    x[3] <- sample(1:13, size = 1)
    x[4] <- sample(1:54, size = 1)
    x[5] <- sample(1:17, size = 1)

    x <- paste0(str_pad(x, width = 2, side = "left", pad = "0"), collapse = ".")
    icnarc[i] <- x

  }

  return(icnarc)

}

