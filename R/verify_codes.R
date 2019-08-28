#' Varify NHS Numbers
#'
#' This function will varify nhs numbers according to the checksum process
#' outlined by UK Government Data Standards Catalogue (GDSC), Version 2.0,
#' Agreed 01.01.02 availible as reference here:
#' https://www.datadictionary.nhs.uk/version2/data_dictionary/data_field_notes/n/nhs_number_de.asp?shownav=0
#'
#' Please enter the NHS number as characters (i.e. "6744424270" and not
#' 6744424270). Remove all formatting, including spaces and dashes.
#'
#' @param nhs_numbers as character vector
#'
#' @return a logical vector
#' @export
#'
#' @importFrom rlang abort
#'
#' @examples
#' validate_nhs("6744424270") # expect TRUE
verify_nhs <- function(nhs_numbers = NULL) {
  if (class(nhs_numbers) != "character") {
    rlang::abort("Please enter NHS Numbers as characters")
  }

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

      for (i in 1:9) store[i] <- test_sequence[i] * (11 - i)

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


#' Generate NHS Numbers
#'
#' Generate random codes that fit the NHS number formatting specification.
#' This is useful when generating synthetic patients or testing the varification
#' functions.
#'
#' @param size the number of codes you wish to generate
#'
#' @return a character vector of length \code{size} with NHS Numbers
#' @export
#'
#' @examples
#' generate_nhs()
#' generate_nhs(5)
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


#' Varify Post Code
#'
#' Varify that a particular string conforms to a UK Post Code. Note, this does
#' not actually check a database (i.e. validation) to see if the post code
#' exists. It only checks that the Post Code meets the correct technical
#' specification for a postcode. There are two reasons why validation isn't a
#' good fit for cc-hic. First, the use of an API invariably means internet
#' access and sending our postcodes out of the secure system (bad). Second, the
#' use of the royal mail database for this purpose isn't free and would make
#' inspectEHR larger than is necessary.
#'
#' @param post_code character vector of post codes
#'
#' @return a logical vector
#' @export
#'
#' @examples
#' varify_post_code("AL5 3HE")
verify_post_code <- function(post_code = NULL) {
  test_pattern <- paste0(
    "^((([A-Za-z][0-9]{1,2})|",
    "(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|",
    "(([A-Za-z][0-9][A-Za-z])|",
    "([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z]))))\\s?[0-9][A-Za-z]{2})|",
    "([A-Za-z][0-9]{1,2})$"
  )

  result <- grepl(pattern = test_pattern, x = post_code)

  return(result)
}


#' Generate Post Code
#'
#' Generate UK Post Codes. Note, this does not garuntee that the postcode
#' exists, only that is corresponds to the correct functional form of a UK
#' postcode.
#'
#' @param size number of post codes you wish to generate
#'
#' @return a character vector of length \code{size} of post codes
#' @export
#'
#' @examples
#' generate_post_code()
#' generate_post_code(5)
generate_post_code <- function(size = 1) {

  # Outward Code
  ## Area Code
  area_codes <- c("AB", "AL", "B", "BA", "BB", "BD", "BH", "BL", "BN",
                  "BR", "BS", "BT", "CA", "CB", "CF",
  "CH", "CM", "CO", "CR", "CT", "CV", "CW", "DA", "DD", "DE", "DG", "DH",
  "DL", "DN", "DT",
  "DY", "E", "EC", "EH", "EN", "EX", "FK", "FY", "G", "GL", "GU", "HA",
  "HD", "HG", "HP",
  "HR", "HS", "HU", "HX", "IG", "IP", "IV", "KA", "KT", "KW", "KY", "L",
  "LA", "LD", "LE",
  "LL", "LN", "LS", "LU", "M", "ME", "MK", "ML", "N", "NE", "NG", "NN",
  "NP", "NR", "NW",
  "OL", "OX", "PA", "PE", "PH", "PL", "PO", "PR", "RG", "RH", "RM", "S",
  "SA", "SE", "SG",
  "SK", "SL", "SM", "SN", "SO", "SP", "SR", "SS", "ST", "SW", "SY", "TA",
  "TD", "TF", "TN",
  "TQ", "TR", "TS", "TW", "UB", "W", "WA", "WC", "WD", "WF", "WN", "WR",
  "WS", "WV", "YO",
  "ZE")
  pos_area <- sample(area_codes, size = size, replace = TRUE)

  ## District Code
  pos_dist <- case_when(
    pos_area %in% c(
      "BR", "FY", "HA", "HD", "HG", "HR", "HS", "HX",
      "JE", "LD", "SM", "SR", "WC", "WN", "ZE") ~ sample(1:9, size = 1),
    pos_area %in% c("AB", "LL", "SO") ~ sample(11:99, size = 1),
    pos_area %in% c("BL", "BS", "CM", "CR", "FY", "HA", "PR", "SL",
                      "SS") ~ sample(0:9, size = 1),
    TRUE ~ sample(0:99, size = 1)
  )

  # Inward Code
  ## Sector Code
  pos_sector <- sample(0:9, size = size, replace = TRUE)

  ## Unit Code
  pos_unit <- paste0(
    sample(
      LETTERS, size = size, replace = TRUE),
    sample(LETTERS, size = size, replace = TRUE))

  post_code <- paste0(pos_area, pos_dist, " ", pos_sector, pos_unit)
}


#' Varify ICNARC Diagnostic Code
#'
#' Varifies that a code conforms to the ICNARC Diagnostic code specification.
#' This is a 5 level hierarchial code that follows a pattern such as
#' ##.##.##.##.##.
#'
#' Leading zeros in each level are not necessary. Acceptable separators are:
#' .,|/-
#'
#' @param icnarc_code as a character vector
#'
#' @return a logical vector
#' @export
#'
#' @importFrom rlang abort
#' @importFrom stringr str_split
#' @importFrom purrr map
#'
#' @examples
#' varify_icnarc("01.06.04.20.16") # correctly formatted
#' varify_icnarc("1.6.4.20.16") # not strictly correct, but parsable
#' varify_icnarc("01-6-04.20.16") # annoying, but parsable
verify_icnarc <- function(icnarc_code = NULL) {
  if (class(icnarc_code) != "character") {
    rlang::abort("Please enter ICNARC code as characters")
  }
  icnarc <- str_split(icnarc_code, pattern = "[.|/-]") %>%
    map(as.integer) %>%
    map(function(x) {
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


#' Generate ICNARC Diagnostic Codes
#'
#' @param size the number of codes you wish to generate
#'
#' @return a character vector of length \code{size} with ICNARC diagnostic codes
#' @export
#'
#' @importFrom stringr str_pad
#'
#' @examples
#' generate_icnarc()
generate_icnarc <- function(size = 1) {
  icnarc <- vector(mode = "character", length = size)

  for (i in seq_len(size)) {
    x <- vector(mode = "integer", length = 5)

    x[1] <- sample(1:2, size = 1)
    x[2] <- sample(1:12, size = 1)
    x[3] <- sample(1:13, size = 1)
    x[4] <- sample(1:54, size = 1)
    x[5] <- sample(1:17, size = 1)

    x <- paste0(
      str_pad(x, width = 2, side = "left", pad = "0"), collapse = ".")
    icnarc[i] <- x
  }

  return(icnarc)
}
