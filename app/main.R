box::use(
  sh = shiny,
  bsl = bslib,
)

box::use(
  app / view / sim,
  app / view / viz
)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  bsl$page(
    sim$ui(ns("sim")),
    viz$ui(ns("viz"))
  )
}

#' @export
server <- function(id) {
  sh$moduleServer(id, function(input, output, session) {
    df <- sim$server("sim")
    viz$server("viz", df)
  })
}
