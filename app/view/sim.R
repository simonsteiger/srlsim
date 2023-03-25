box::use(
  dp = dplyr,
  pr = purrr,
  sh = shiny,
  magrittr[`%>%`]
)

box::use(
  srl = app / logic / fn,
  par = app / logic / params
)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  sh$tagList(
    sh$actionButton(ns("rerun"), label = "Simulate")
  )
}

#' @export
server <- function(id) {
  sh$moduleServer(
    id,
    function(input, output, session) {
      res <- sh$eventReactive(input$rerun, {
        srl$simulate()
      })

      sh$reactive(
        srl$prepare_plot_df(
          estimate = res(),
          id = par$id,
          species = par$species,
          trial = par$trial_all,
          rev = par$rev_all
        ) %>%
          dp$mutate(
            plot = pr$map(data, ~ srl$plot_srl(.x))
          )
      )
    }
  )
}
