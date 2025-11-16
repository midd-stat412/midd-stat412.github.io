
model {
## sampling model
for (i in 1:n) {
   y[i] ~ dnorm(beta0[county_id[i]] + beta1[county_id[i]]*x[i], 1/sigma2)
}

## priors
for(j in 1:J){
  beta0[j] ~ dnorm(mu_beta0, 1/s2_beta0)
  beta1[j] ~ dnorm(mu_beta1, 1/s2_beta1)
}
mu_beta0 ~ dnorm(mu0, 1/s20)
s2_beta0 ~ dgamma(a0, b0)

mu_beta1 ~ dnorm(mu1, 1/s21)
s2_beta1 ~ dgamma(a1, b1)

sigma2 ~ dgamma(a, b)
}

