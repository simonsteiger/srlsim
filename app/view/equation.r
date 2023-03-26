box::use(
    sh = shiny,
    bsl = bslib,
)

box::use(
    srl = app / logic / fn,
)

formula <-
    "
      \\begin{equation}
      \\begin{aligned}
      Y_{ijkl} = & \\theta_{000} + \\theta_{001}(Species_j) + U_{00i}
      + [\\theta_{010} + \\theta_{011}(Species_j) + U_{01i}](Reversal_k)
      + [\\theta_{100} + \\theta_{101}(Species_j) + U_{10i} + (\\theta_{110} + U_{11i})(Reversal_k)](Trial_l)
      + \\epsilon_{ijkl}
      \\end{aligned}
      \\end{equation}
    "

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        bsl$layout_column_wrap(
            class = "m-4", width = "100%",
            srl$hfb_card(
              card_height = 200,
              header = "Equation",
              fill = formula,
              body = "
              The data are generated based on this equation. 
              I will add the parameter values here later. 
              Negative trajectories are possible but implausible and will be prevented in a future version.
              ",
              card_body_height = "35%"
            )
        )
    )
}
