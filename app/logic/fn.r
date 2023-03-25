box::use(
    tbl = tibble,
    tdr = tidyr,
    dp = dplyr,
    rl = rlang,
    gg = ggplot2,
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
        dp$mutate(
            trial_per_sub = seq_len(dp$n()),
            .by = id
        ) %>%
        tdr$nest(.by = species)
}

#' @export
plot_srl <- function(data, color = NULL, facet = NULL) {
    color <- rl$enquo(color)
    facet <- rl$enquo(facet)

    gg$ggplot(data, gg$aes(trial, estimate, color = !!color)) +
        gg$geom_line(alpha = 0.5) +
        gg$facet_wrap(gg$vars(!!facet)) +
        gg$scale_fill_viridis_d() +
        gg$theme_bw()
}
