#' launches the shiny app
#'
#' @export launchApp
#'
#' @return shiny application object
#' @importFrom shiny shinyApp
#'
#' @examples
#' \dontrun{launchApp()}
launchApp <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
