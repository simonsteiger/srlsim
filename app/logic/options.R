box::use(
  gg = ggplot2,
  tmc = thematic,
)

gg$theme_set(ggplot2::theme_minimal())

tmc$thematic_shiny(font = "auto")