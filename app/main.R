box::use(
  sh = shiny,
  bsl = bslib,
  tmc = thematic,
)

box::use(
  app / logic / options,
  app / view / sim,
  app / view / viz,
  eqt = app / view / equation,
)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  bsl$page(
    theme = bsl$bs_theme(
      version = 5L,
      primary = "#626e78",
      secondary = "#f0f3f5",
      success = "#71d1a6"
      ),
    sh$withMathJax(),
    sim$ui(ns("sim")),
    viz$ui(ns("viz")),
    eqt$ui(ns("eqt"))
  )
}

#' @export
server <- function(id) {
  sh$moduleServer(id, function(input, output, session) {
    df <- sim$server("sim")
    viz$server("viz", df)
  })
}
