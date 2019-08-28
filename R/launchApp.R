#' launches the shiny app
#'
#' @export launchApp
#'
#' @return shiny application object
#' @import shiny
#'
#' @examples
#' \dontrun{launchApp()}
launchApp <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
