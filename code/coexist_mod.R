
model {
## sampling model
for (i in 1:n) {
   y[i] ~ dbern(p[i])
   p[i] = 1/(1 + exp(-(beta0 + beta1*X[i,2] + beta2*X[i,3] + beta3*X[i,4] + beta4*X[i,5] + beta5*X[i,6])))
}

## priors
beta0 ~ dnorm(0, 1/3)
beta1 ~ dnorm(0, 1/3)
beta2 ~ dnorm(0, 1/3)
beta3 ~ dnorm(0, 1/3)
beta4 ~ dnorm(0, 1/3)
beta5 ~ dnorm(0, 1/3)
}

