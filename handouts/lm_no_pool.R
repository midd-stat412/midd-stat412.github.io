
model {
## sampling model
for (i in 1:n) {
    y[i] ~ dnorm(beta0[county_id[i]] + beta1[county_id[i]]*x[i], 1/sigma2)
}

## priors
for(j in 1:J){
  beta0[j] ~ dnorm(mu0, 1/s20)
  beta1[j] ~ dnorm(mu1, 1/s21)
}
sigma2 ~ dgamma(a, b)
}

