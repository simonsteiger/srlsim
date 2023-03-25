box::use(
  dp = dplyr,
  tdr = tidyr,
  tbl = tibble,
  pr = purrr,
  gg = ggplot2,
  sc = scales,
  pal = palettes,
  magrittr[`%>%`],
)

# PREPARE SIMULATION ----

set.seed(42)

# Prepare design matrix

# Number of subjects
j <- 12

# Number of trials per subject in each reversal
n_trial <- 30

# Number of reversals
n_rev <- 10

# Total number of trials across all individuals
n_tot <- j * n_trial * n_rev

# Subject-response vector
id <- rep(1:j, each = n_trial * n_rev)

# Trial vector
trial_all <- rep(0:(n_trial - 1), times = j * n_rev)

# Effect coding for species
species <- rep(c(-0.5, 0.5), each = n_tot / 2)

# Reversal vector for one individual
rev_one <- rep(0:(n_rev - 1), each = n_trial)

# Reversal vector for all individuals
rev_all <- rep(rev_one, times = j)

# rev:trial interaction vector
trial_by_rev <- rev_all * trial_all

# Population level regression coefficients
gamma <- c(
  mu = 0.1,
  eta = 0.015,
  nu = 0.04,
  ksi = 0.1,
  phi = -0.02,
  pi = 0.01,
  lambda = 0.02
)

# Number of group level effects
tau <- c(
  mu = 0.01, # this is alpha
  eta = 0.01, # this is theta
  nu = 0.01, # this is beta
  ksi = 0,
  phi = 0,
  pi = 0,
  lambda = 0.01
)

# Std dev of individual observation
sigma <- 0.015

# Group-level coefficients
beta <- mapply(function(g, t) rnorm(j, g, t), g = gamma, t = tau)

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

# Bind submatrices together
params <- cbind(v_mu, v_eta, v_nu, v_ksi, v_phi, v_pi, v_lambda)

outcome <- vector(length = n_tot)

# SIMULATE DATA ----
for (n in seq_along(1:(n_tot))) {
  # simulate linear response data
  outcome[n] <- rnorm(1, params[n, ] %*% beta[id[n], ], sigma)
}

# Make a data frame
out <-
  tbl$tibble(id, species, rev_all, trial_all, outcome) %>%
  dp$mutate(
    dp$across(c(id, species), as.factor)
  ) %>%
  dplyr::mutate(
    trial_per_sub = seq_len(dp$n()),
    .by = id
  ) %>%
  tdr$nest(.by = species)

# Plot
srlplot <- function(data) {
  gg$ggplot(data, gg$aes(trial_per_sub, outcome, group = rev_all)) +
    gg$geom_line() +
    gg$facet_wrap(~id) +
    gg$theme_bw()
}

p <- out %>%
  dp$mutate(
    plot = pr$map(data, ~ srlplot(.x))
  )

pr$walk(p$plot, print)
