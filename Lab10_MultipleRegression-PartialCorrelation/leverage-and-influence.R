library('MASS')
library('psych')
library('car')
# leverage ----
d <- as.data.frame(mvrnorm(n = 30, mu = c(0,0), Sigma = matrix(c(1,0.6,0.6,1), ncol = 2, nrow = 2)))
names(d) <- c('x','y')
d$x <- as.vector(scale(d$x))
d$y <- as.vector(scale(d$y))
d1 <- d
d2 <- d

m <- lm(y~x, data = d)
plot(d$x, d$y, ylim = c(-2,7))
abline(m)

d1[51,] <- c(0,5)
m1 <- lm(y~x, data = d1)
points(d1$x[51], d1$y[51], col = 'blue')
abline(m1, col = 'blue')

d2[51,] <- c(2,predict(m1, list(x = 2))+5)
m2 <- lm(y~x, data = d2)
points(d2$x[51], d2$y[51], col = 'red')
abline(m2, col = 'red')


d <- data.frame(
  y = c(0, 1),
  x = c(0, 1)
)

m <- lm(y~x, data = d)
plot(d$x, d$y)
abline(m)

d <- data.frame(
  y = c(0, 1, 0),
  x = c(0, 1, 0.5)
)

m <- lm(y~x, data = d)
points(d$x[3], d$y[3], col = 'blue')
abline(m, col = 'blue')

d <- data.frame(
  y = c(0, 1, 0.5),
  x = c(0, 1, 1)
)

m <- lm(y~x, data = d)
points(d$x[3], d$y[3], col = 'red')
abline(m, col = 'red')
