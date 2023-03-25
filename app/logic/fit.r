# FULL MODEL INPUT ----
# PRIORS
full_prior <- get_prior(
  Y ~ 1 + trial + rev + species +
    species:rev + species:trial + rev:trial +
    (1 + trial + rev + rev:trial || id),
  data = sim_data
)


# Fixed effects
full_prior$prior[1] <- "normal(0, 0.1)"
# Grand mean
full_prior$prior[8] <- "beta(1, 1)" # flat prior to recover mu parameter
# Random effects
full_prior$prior[9] <- "normal(0, 0.05)"
# Random error
full_prior$prior[15] <- "normal(0, 0.1)"

# STANCODE
full_stancode <- make_stancode(
  Y ~ 1 + trial + rev + species +
    species:rev + species:trial + rev:trial +
    (1 + trial + rev + rev:trial || id),
  data = sim_data,
  prior = full_prior
)

write(full_stancode, file = "full_stan_flat.txt")
# edited to include beta upper and lower bound
# file is full_stan.stan

full_data <- list(
  N = N, Y = Y, K = K, X = X,
  Z_1_1 = Xmu, Z_1_2 = trial,
  Z_1_3 = rev, Z_1_4 = revXtri,
  J_1 = id, N_1 = J, M_1 = M_1,
  NC_1 = NC_1, prior_only = 0
)

# SPECIES MODEL INPUT ----
# PRIORS
sp_prior <- get_prior(
  Y ~ 1 + trial + rev + species +
    species:rev + species:trial + rev:trial,
  data = sim_data
)


# Fixed effects
sp_prior$prior[1] <- "normal(0, 0.1)"
# Grand mean
sp_prior$prior[8] <- "beta(4, 4)"
# Random effects
sp_prior$prior[9] <- "normal(0, 0.1)"

# STANCODE
sp_stancode <- make_stancode(
  Y ~ 1 + trial + rev + species +
    species:rev + species:trial + rev:trial,
  data = sim_data,
  prior = sp_prior
)

# write(sp_stancode, file = "sp_stan.txt")
# edited to include beta upper and lower bound
# file is sp_stan.stan

# Other variables to be passed to STAN
Xsp <- cbind(Xmu, Xeta, Xnu, Xksi, Xphi, Xpi, Xlambda)

sp_data <- list(list(
  N = N, Y = Y, K = K, X = Xsp,
  NC_1 = NC_1, prior_only = 0
))

# ID MODEL INPUT ----
# PRIORS
id_prior <- get_prior(
  Y ~ 1 + trial + rev + rev:trial +
    (1 + trial + rev + rev:trial || id),
  data = sim_data
)

# Fixed effects
id_prior$prior[1] <- "normal(0, 0.1)"
# Grand mean
id_prior$prior[5] <- "beta(4, 4)"
# Random effects
id_prior$prior[6] <- "normal(0, 0.05)"
# Random error
id_prior$prior[12] <- "normal(0, 0.1)"

id_stancode <- make_stancode(
  Y ~ 1 + trial + rev + rev:trial +
    (1 + trial + rev + rev:trial || id),
  data = sim_data,
  prior = id_prior
)

# write(id_stancode, file = "id_stan.txt")
# edited to include beta upper and lower bound
# file is id_stan.stan

# Other variables to be passed to STAN
Xid <- cbind(Xmu, Xeta, Xnu, Xlambda)

id_data <- list(list(list(
  N = N, Y = Y, K = K, X = Xid,
  Z_1_1 = Xmu, Z_1_2 = trial,
  Z_1_3 = rev, Z_1_4 = revXtri,
  J_1 = id, N_1 = J, M_1 = M_1,
  NC_1 = NC_1, prior_only = 0
)))

# FIT MODELS ----

# FULL
full_stan <- fit.stan(
  file = "full_stan_flat.stan",
  data = full_data
)

# BRMS ----
# FULL MODEL
brm <- brm(
  Y ~ trial + rev + species +
    species:rev + species:trial + rev:trial +
    (1 + trial + rev + rev:trial || id),
  data = sim_data,
  prior = full_prior,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)
