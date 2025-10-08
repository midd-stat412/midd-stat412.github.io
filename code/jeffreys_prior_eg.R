library(pracma)

# Data: single observation
y <- 2.5
sigma <- 1

# Grid in mu-space
mu <- seq(-5, 5, length.out = 10000)

# Likelihood in mu
lik_mu <- dnorm(y, mean = mu, sd = sigma)

# Prior 1: flat on mu (improper uniform, so constant)
# Happens to be Jeffreys prior on mu: uniform (constant)
prior_flat_mu <- rep(1, length(mu))

# Posterior in mu
post_mu <- lik_mu * prior_flat_mu
post_mu <- post_mu / trapz(mu, post_mu)

# Transform parameter: phi = exp(mu)
phi <- exp(mu)
dmu_dphi <- 1 / phi  # derivative of mu w.r.t phi

# Transform posterior to phi-space
post_phi_from_mu <- post_mu / dmu_dphi  # divide by |dmu/dphi| for pdf transform
post_phi_from_mu <- post_phi_from_mu / trapz(phi, post_phi_from_mu)

# Now, prior 2: flat prior on phi (instead of mu)
phi_grid <- seq(min(phi), max(phi), length.out = length(mu))
prior_flat_phi <- rep(1, length(phi_grid))

# Likelihood in phi-space (transform mu to phi)
lik_phi <- dnorm(y, mean = log(phi_grid), sd = sigma)

# Posterior in phi-space
post_phi <- lik_phi * prior_flat_phi
post_phi <- post_phi / trapz(phi_grid, post_phi)

# Sampling function from mu posterior
cdf_mu <- cumtrapz(mu, post_mu)
cdf_mu <- cdf_mu / max(cdf_mu)
sample_mu <- function(n) {
  u <- runif(n)
  approx(cdf_mu, mu, xout = u, rule = 2)$y
}
mu_samps <- sample_mu(50000)
phi_samps_from_mu <- exp(mu_samps)

# Sampling function from phi posterior
cdf_phi <- cumtrapz(phi_grid, post_phi)
cdf_phi <- cdf_phi / max(cdf_phi)
sample_phi <- function(n) {
  u <- runif(n)
  approx(cdf_phi, phi_grid, xout = u, rule = 2)$y
}
phi_samps <- sample_phi(50000)

# Plot comparison
hist(phi_samps_from_mu, breaks = 100, freq = FALSE, col = rgb(1,0,0,0.4),
     main = expression(paste("Posterior in ", phi, "-space")),
     xlab = expression(phi), xlim = range(phi))
hist(phi_samps, breaks = 100, freq = FALSE, col = rgb(0,0,1,0.4), add = TRUE)
legend("topright", legend = c("Posterior from mu-space", "Posterior directly in phi-space"),
       fill = c(rgb(1,0,0,0.4), rgb(0,0,1,0.4)))


library(pracma)

# Data
y <- 2.5
sigma <- 1

# Grid in mu-space
mu <- seq(-5, 5, length.out = 10000)

# Likelihood in mu
lik_mu <- dnorm(y, mean = mu, sd = sigma)

# Jeffreys prior on mu: uniform (constant)
prior_jeff_mu <- rep(1, length(mu))

# Posterior in mu
post_mu_jeff <- lik_mu * prior_jeff_mu
post_mu_jeff <- post_mu_jeff / trapz(mu, post_mu_jeff)

# Transform parameter phi = exp(mu)
phi <- exp(mu)
dmu_dphi <- 1 / phi  # derivative of mu wrt phi

# Transform posterior to phi-space
post_phi_from_mu <- post_mu_jeff / dmu_dphi  # divide by |dmu/dphi| for pdf transform
post_phi_from_mu <- post_phi_from_mu / trapz(phi, post_phi_from_mu)

# Jeffreys prior on phi:
# Fisher info transforms as I(phi) = I(mu) * (dmu/dphi)^2 = (1/sigma^2) * (1/phi)^2 = 1/(sigma^2 phi^2)
# So Jeffreys prior on phi is proportional to 1/phi
prior_jeff_phi <- 1 / phi

# Likelihood in phi-space (using mu = log(phi))
lik_phi <- dnorm(y, mean = log(phi), sd = sigma)

# Posterior in phi
post_phi_jeff <- lik_phi * prior_jeff_phi
post_phi_jeff <- post_phi_jeff / trapz(phi, post_phi_jeff)

# Sampling functions using inverse CDF method
cdf_mu_jeff <- cumtrapz(mu, post_mu_jeff)
cdf_mu_jeff <- cdf_mu_jeff / max(cdf_mu_jeff)
sample_mu_jeff <- function(n) {
  u <- runif(n)
  approx(cdf_mu_jeff, mu, xout = u, rule = 2)$y
}
mu_samps_jeff <- sample_mu_jeff(50000)
phi_samps_from_mu_jeff <- exp(mu_samps_jeff)

cdf_phi_jeff <- cumtrapz(phi, post_phi_jeff)
cdf_phi_jeff <- cdf_phi_jeff / max(cdf_phi_jeff)
sample_phi_jeff <- function(n) {
  u <- runif(n)
  approx(cdf_phi_jeff, phi, xout = u, rule = 2)$y
}
phi_samps_jeff <- sample_phi_jeff(50000)

# Plot to compare posterior in phi-space
hist(phi_samps_from_mu_jeff, breaks = 100, freq = FALSE, col = rgb(1,0,0,0.4),
     main = expression(paste("Jeffreys Posterior in ", phi, "-space")),
     xlab = expression(phi), xlim = range(phi))
hist(phi_samps_jeff, breaks = 100, freq = FALSE, col = rgb(0,0,1,0.4), add = TRUE)
legend("topright", legend = c("From mu-space", "Directly in phi-space"),
       fill = c(rgb(1,0,0,0.4), rgb(0,0,1,0.4)))
