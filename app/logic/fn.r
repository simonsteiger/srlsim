box::use(
    tbl = tibble,
    tdr = tidyr,
    dp = dplyr,
    pr = purrr,
    rl = rlang,
    gg = ggplot2,
    magrittr[`%>%`],
    stats[runif],
)

box::use(
    p = app/logic/params,
)

simulate <- function() {
    outcome <- vector(length = nrow(p$coefs))
    
    set.seed(sample(0:999, 1))
    
    beta <- mapply(function(g, t) rnorm(p$j, g, t), g = p$gamma, t = p$tau)

    for (n in seq_along(outcome)) {
        # simulate linear response data
        outcome[n] <- rnorm(1, p$coefs[n, ] %*% beta[p$id[n], ], p$sigma)
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
        dplyr::mutate(
            trial_per_sub = seq_len(dp$n()),
            .by = id
        ) %>%
        tdr$nest(.by = species)
}

#' @export
plot_srl <- function(data) {
  gg$ggplot(data, gg$aes(trial_per_sub, estimate, group = rev)) +
    gg$geom_line() +
    gg$facet_wrap(~id) +
    gg$theme_bw()
}
