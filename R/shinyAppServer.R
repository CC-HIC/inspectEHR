#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'
shinyAppServer <- function(input, output) {
# Define server logic required to draw a histogram

  output$table <- renderTable({
    ctn <- connect(sqlite_file = "./data-raw/synthetic_db.sqlite3")
    core <- make_core(ctn)
    ext <- extract(core, input = input$dataitem)
    head(ext)
  })
}
