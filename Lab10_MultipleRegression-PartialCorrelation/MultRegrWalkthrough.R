## Regression Walkthrough

library(MASS)

# Simulate some correlated variables ----
Sigma <- rbind(
  y  = c( 1, .7, .3),
  x1 = c(.7,  1, .5),
  x2 = c(.3, .5,  1)
)
d <- as.data.frame(MASS::mvrnorm(100, mu = c(0, 0, 0), Sigma = Sigma))

# Standardize the variables
Z <- function(x) {(x - mean(x)) / sd(x)}
d$y <- Z(d$y)
d$x1 <- Z(d$x1)
d$x2 <- Z(d$x2)

# Compute the correlation matrix and regression model ----
r <- cor(d)
print(r)

m <- lm(y ~ x1 + x2, data = d)
summary(m)

R2_full <- summary(m)$r.squared

# Compute beta coefficients based on correlations ----
B1 <- (r["x1", "y"] - (r["x2", "y"] * r["x1", "x2"])) / (1 - r["x1", "x2"]^2)
B2 <- (r["x2", "y"] - (r["x1", "y"] * r["x1", "x2"])) / (1 - r["x1", "x2"]^2)
print(c("x1" = B1, "x2" = B2))
print(coefficients(m)[c("x1", "x2")])

# Compute t-values based on semi-partial correlations ----
sr_x1y <- (r["x1", "y"] - (r["x2", "y"] * r["x1", "x2"])) / sqrt(1 - r["x1", "x2"]^2)
sr_x2y <- (r["x2", "y"] - (r["x1", "y"] * r["x1", "x2"])) / sqrt(1 - r["x1", "x2"]^2)

N <- nrow(d)
P <- 2
MS <- (1 - R2_full) / (N - P - 1)
se <- sqrt(MS)
t_x1 <- sr_x1y / se
t_x2 <- sr_x2y / se
print(c("t_x1" = t_x1, "t_x2" = t_x2))
print(summary(m)$coefficients[, "t value"][c('x1', 'x2')])

# Explore residuals ----
m.x1 <- lm(x1 ~ x2, data = d)
x1_hat <- predict(m.x1, d)
x1_residuals <- d$x1 - x1_hat
all.equal(residuals(m.x1), x1_residuals)

m.x2 <- lm(x2 ~ x1, data = d)
x2_hat <- predict(m.x2, d)
x2_residuals <- d$x2 - x2_hat
all.equal(residuals(m.x2), x2_residuals)

d_res <- data.frame(y = d$y, x1_res = x1_residuals, x2_res = x2_residuals)
sr <- cor(d_res)
print(c("sr_x1y" = sr_x1y, "sr_x2y" = sr_x2y))
print(c("sr_x1y" = sr["x1_res", "y"], "sr_x2y" = sr["x2_res", "y"]))