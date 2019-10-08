# calculate_sofa <- function(x, lookups) {
#   if (is.null(attr(x, "lookups")) & is.null(lookups)) {
#     rlang::abort("There are no lookups to facilitate this function")
#   }
#   if (is.null(lookups)) {
#     lookups <- attr(x, "lookups")
#   }
# }
#
# sofa_cvs <- function(x, lookups) {
#   x %>%
#     mutate(
#       sofa_cvs = case_when(
#         norad > 0.1 | dop > 15 | adr > 0.1 | vaso > 0 ~ 4L,
#         (norad > 0.03 & norad <= 0.1) | (dop > 5 & dop <= 15) | (adr > 0 & adr <= 0.1) ~ 3L,
#         dop > 0 | dob > 0 | norad > 0 ~ 2L,
#         map_t < 70 ~ 1L,
#         map_t >= 70 ~ 0L,
#         TRUE ~ 0L)
#   )
# }
