
model {
## sampling model
for (i in 1:N) {
   y[i] ~ dnorm(theta, tau2)
}

## priors
theta ~ dnorm(mu0, phi0)
sigma2 ~ dgamma(a, b)
tau2 <- 1/sigma2
}

