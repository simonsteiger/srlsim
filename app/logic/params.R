box::use(
  dp = dplyr,
  tdr = tidyr,
  tbl = tibble,
  pr = purrr,
  gg = ggplot2,
  sc = scales,
  pal = palettes,
  magrittr[`%>%`],
  stats[rnorm],
)

# PREPARE SIMULATION ----

set.seed(42)

# Prepare design matrix

#' @export
#' Number of subjects
j <- 12

# Number of trials per subject in each reversal
n_trial <- 30

# Number of reversals
n_rev <- 12

# Total number of trials across all individuals
n_tot <- j * n_trial * n_rev

#' @export
#' Subject-response vector
id <- rep(1:j, each = n_trial * n_rev)

#' @export
#' Trial vector
trial_all <- rep(0:(n_trial - 1), times = j * n_rev)

#' @export
#' Effect coding for species
species <- rep(c(-0.5, 0.5), each = n_tot / 2)

# Reversal vector for one individual
rev_one <- rep(0:(n_rev - 1), each = n_trial)

#' @export
#' Reversal vector for all individuals
rev_all <- rep(rev_one, times = j)

# rev:trial interaction vector
trial_by_rev <- rev_all * trial_all

#' @export
#' Population level regression coefficients
gamma <- c(
  mu = 0.1,
  eta = 0.015,
  nu = 0.04,
  ksi = 0.1,
  phi = -0.02,
  pi = 0.01,
  lambda = 0.02
)

#' @export
#' Number of group level effects
tau <- c(
  mu = 0.01, # alpha in formula
  eta = 0.01, # theta in formula
  nu = 0.01, # beta in formula
  ksi = 0,
  phi = 0,
  pi = 0,
  lambda = 0.01
)

# Mu
v_mu <- rep(1, times = n_tot)

# Eta
v_eta <- trial_all

# Nu
v_nu <- rev_all

# Lambda
v_lambda <- trial_by_rev / 25

# Ksi Species-specific effect on mean
v_ksi <- species

# Phi Species-specific reversal effect
v_phi <- c(rep(rev_one, times = j / 2), rep(0, times = n_tot / 2))

# Pi Species-specific trial effect
v_pi <- c(rep(1:n_trial, times = j * n_rev / 2), rep(0, times = n_tot / 2))

#' @export
#' Coefficient matrix
coefs <- cbind(v_mu, v_eta, v_nu, v_ksi, v_phi, v_pi, v_lambda)

#' @export
#' Std dev of individual observation
sigma <- 0.015

#' @export
#' Group-level coefficients
beta <- mapply(function(g, t) rnorm(j, g, t), g = gamma, t = tau)

# Plot
srlplot <- function(data) {
  gg$ggplot(data, gg$aes(trial_per_sub, outcome, group = rev_all)) +
    gg$geom_line() +
    gg$facet_wrap(~id) +
    gg$theme_bw()
}

# p <- out %>%
#   dp$mutate(
#     plot = pr$map(data, ~ srlplot(.x))
#   )
# 
# pr$walk(p$plot, print)
