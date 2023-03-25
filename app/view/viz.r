box::use(
    # sh = shiny,
    # dp = dplyr,
    # pr = purrr,
    # gg = ggplot2,
    # bsl = bslib,
    # plt = plotly
    shiny[...],
    dplyr[...],
    purrr[...],
    ggplot2[...],
    bslib[...],
    DT[...],
    plotly[...]
)

box::use(
    app / view / sim,
    srl = app / logic / fn,
)

srl_card <- function(header, fill, body) {
    card(
        height = 700, full_screen = TRUE,
        card_header(header),
        card_body_fill(fill),
        card_body(
            height = "30%",
            body
        )
    )
}

#' @export
ui <- function(id) {
    ns <- NS(id)

    tagList(
        layout_column_wrap(
            width = "200px", height = 700,
            srl_card("Species 1", plotlyOutput(ns("species1")), "Data for species 1"),
            srl_card("Species 2", plotlyOutput(ns("species2")), "Data for species 2"),
        )
    )
}

#' @export
server <- function(id, df) {
    moduleServer(
        id,
        function(input, output, session) {
            stopifnot(is.reactive(df))
          
          
            plot_frame <- reactive(
                df() %>%
                    mutate(
                        # plot_srl takes data, color, facet
                        plot = map(data, function(d) srl$plot_srl(d, id, rev))
                    )
            )

            output$species1 <- renderPlotly(ggplotly(plot_frame()$plot[[1]]))

            output$species2 <- renderPlotly(ggplotly(plot_frame()$plot[[2]]))
        }
    )
}
