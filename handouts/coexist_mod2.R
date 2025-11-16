
model {
## sampling model
for (i in 1:n) {
   y[i] ~ dbern(p[i])
   p[i] = 1/(1 + exp(-(beta %*% X[i, ])))
}

## priors
for(k in 1:K){
  beta[k] ~ dnorm(0, 1/s2_beta)
}
}

