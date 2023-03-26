box::use(
    tbl = tibble,
    tdr = tidyr,
    dp = dplyr,
    str = stringr,
    rl = rlang,
    gg = ggplot2,
    bsl = bslib,
    magrittr[`%>%`],
    stats[runif, rnorm],
)

box::use(
    par = app / logic / params,
)

#' @export
simulate <- function() {
    outcome <- vector(length = nrow(par$coefs))

    # each run is different
    set.seed(sample(0:999, 1))

    beta <- mapply(function(g, t) rnorm(par$j, g, t), g = par$gamma, t = par$tau)

    for (n in seq_along(outcome)) {
        # simulate linear response data
        outcome[n] <- rnorm(1, par$coefs[n, ] %*% beta[par$id[n], ], par$sigma)
    }

    outcome
}

#' @export
prepare_plot_df <- function(...) {
    dots <- rl$list2(...)

    tbl$tibble(!!!dots) %>%
        dp$mutate(
            dp$across(c(id, species), as.factor)
        ) %>%
        dp$filter(rev %in% c(0, 3, 7, 11)) %>% # could add control for that
        dp$mutate(
          rev = str$str_pad(rev, width = 2, side = "left", pad = "0"),
          rev = as.factor(paste("Reversal", rev)),
          trial_per_sub = seq_len(dp$n()),
          .by = id
        ) %>%
        tdr$nest(.by = species)
}

#' @export
plot_srl <- function(data, color = NULL, facet = NULL) {
    color <- rl$enquo(color)
    facet <- rl$enquo(facet)

    gg$ggplot(data, gg$aes(trial, performance, color = !!color)) +
        gg$geom_line(alpha = 0.5) +
        gg$facet_wrap(gg$vars(!!facet)) +
        gg$scale_color_viridis_d(option = "mako")
}

#' @export
hfb_card <- function(header, fill, body, card_height = 700, card_body_height = "15%") {
    bsl$card(
        class = "border-primary",
        height = card_height, full_screen = TRUE,
        bsl$card_header(header, class = "bg-primary"),
        bsl$card_body_fill(fill, class = "bg-secondary"),
        bsl$card_body(
            class = "bg-secondary",
            height = card_body_height,
            body
        )
    )
}