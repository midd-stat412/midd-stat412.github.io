theta <- seq(0,1,0.01)
a <- 2; b <- 1.25
y <- 1
n <- 3
like <- choose(n,y)* theta^y * (1-theta)^(n-y)

prior_kern <- theta^(a-1) * (1-theta)^(b-1)
prior_const <- gamma(a+b)/(gamma(a) * gamma(b))
prior <- prior_const * prior_kern
num <- prior * like
denom <- choose(n,y)* gamma(a+b) / (gamma(a) * gamma(b) )* gamma(a + y) * gamma(b +n -y)/gamma(a+b + n)
data.frame(prior = prior, like = like, posterior = num/denom, theta = theta) |>
  pivot_longer(cols = 1:3, names_to = "dist", values_to = "val") |>
  ggplot(aes(x = theta, y = val, col = dist))  +
  geom_line() + 
  geom_hline(yintercept = denom) 


p1 <- data.frame(prior = prior, like = like, unnormalized = num, theta = theta) |>
  pivot_longer(cols = 1:3, names_to = "dist", values_to = "val") |>
  mutate(dist = factor(dist, levels = c("prior", "like", "unnormalized"))) |>
  mutate(ll = dist == "unnormalized") |>
  ggplot(aes(x = theta, y = val))  +
  geom_line(aes(col = dist, linetype = ll), linewidth = 1)  +
  scale_color_manual(values =c("tomato", "gold", "orange")) +
  # scale_linetype_manual(values = c("dotted","dotted","solid")) +
  theme_bw() +
  guides(linetype = "none") +
  labs(caption = paste("unnormalized = prior x like"),
       y = element_blank(),
       x = expression(theta)) +
  theme(legend.title=element_blank()) +
  lims(y = c(0,2))


p2 <- data.frame(prior = prior, like = like, unnormalized = num, posterior = num/denom, theta = theta) |>
  pivot_longer(cols = c("unnormalized", "posterior"), names_to = "dist", values_to = "val") |>
  mutate(dist = factor(dist, levels = c("posterior", "unnormalized"))) |>
  ggplot(aes(x = theta, y = val, col = dist))  +
  geom_line(aes(linetype = dist), linewidth = 1) + 
  scale_color_manual(values = c("orange", "orange"))  +
  theme_bw() +
  # geom_hline(yintercept = denom)  +
  labs(caption = paste("Marginal likelihood:", round(denom,3)),
       y = element_blank(),
       x = expression(theta))+
  theme(legend.title=element_blank())+
  lims(y = c(0,2))


p3 <- data.frame(prior = prior, like = like, posterior = num/denom, theta = theta) |>
  pivot_longer(cols = 1:3, names_to = "dist", values_to = "val") |>
  mutate(dist = factor(dist, levels = c("prior", "like", "posterior"))) |>
  ggplot(aes(x = theta, y = val, col = dist))  +
  geom_line(linewidth = 1)  +
  scale_color_manual(values =c("tomato", "gold", "orange")) +
  theme_bw() +
  theme(legend.title=element_blank())+
  lims(y = c(0,2)) +
  labs( y = element_blank(),
        x = expression(theta))

p1 + p2 + p3

