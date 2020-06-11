#' Validate VA_RC_01: Structural data missingness conforms to agreed schema
#'
#' @param tbls
#'
#' @return
#' @export
#'
#' @examples
va_rc_01 <- function(tbls) {
  tbls %>%
    imap(~ check_columns(.x, .y)) %>%
    bind_rows()
}
