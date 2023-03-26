box::use(
  dp = dplyr,
  pr = purrr,
  sh = shiny,
  bsi = bsicons,
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
    sh$div(
      class = "mt-4 d-flex justify-content-center",
      sh$actionButton(
        ns("rerun"),
        class = "btn-success",
        label = "Repeat simulation",
        icon = sh$icon("repeat")
      )
    ),
    sh$div(
      class = "mt-4 d-flex justify-content-center",
      "True SRL data would be better approximated by Richard's curve (next step!)"
    )
  )
}

#' @export
server <- function(id) {
  sh$moduleServer(
    id,
    function(input, output, session) {
      res <- sh$eventReactive(input$rerun, ignoreNULL = FALSE, {
        srl$simulate()
      })

      sh$reactive(
        srl$prepare_plot_df(
          performance = res(),
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
