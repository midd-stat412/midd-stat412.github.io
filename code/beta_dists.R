# Load required library
library(ggplot2)

# Define parameters for three beta distributions
params <- data.frame(
  alpha = c(2, 8, 1),
  beta = c(5, 2, 1),
  group = c("a = 2, b = 5", "a = 8, b = 2", "a = 1, b = 1")
)

# Create x values
x <- seq(0, 1, length.out = 200)

# Create a data frame with densities for all three distributions
df <- do.call(rbind, lapply(1:nrow(params), function(i) {
  data.frame(
    x = x,
    density = dbeta(x, shape1 = params$alpha[i], shape2 = params$beta[i]),
    group = params$group[i]
  )
}))

# Plot
ggplot(df, aes(x = x, y = density, color = group)) +
  geom_line(size = 1.2) +
  labs(
    title = "Beta Distributions",
    x = "x",
    y = "Density",
    color = "Parameters"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
