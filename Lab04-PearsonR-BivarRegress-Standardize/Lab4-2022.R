### Lab 4

## Load data
data(trees)

## Computing Pearson's R
# Does the tree height correlate with the tree volume?
standardize <- function(datoi) {
  numerator <- datoi - mean(datoi)
  denominator <- sd(datoi)
  zscore <- numerator/denominator
  return(zscore)
}
x <- standardize(trees$Height)
y <- standardize(trees$Volume)
n <- length(x)
r <- cor(x, y)

# Standard Error 
se_cor <- function(pearsR, samplen) {
  numer <- 1 - pearsR^2
  denom <- samplen - 2
  finalse <- sqrt(numer/denom)
  return(finalse)
}
se <- se_cor(r, n)

# t-test 
tval <- r / se

# Significance 
# multiply by 2 for 2 tailed
pval <- pt(tval, df = 30, lower.tail = FALSE) * 2


## Significance of Pearson's R
# Is our R regarding the correlation of tree height and tree volume significant?
cor.test(trees$Height, trees$Volume)
cor.test(x, y) # same!

# Plot
plot(trees$Height, trees$Volume) + abline(lm(trees$Volume ~ trees$Height), col="red")
# Note: note the direction of arguments in abline(lm( Y ~ X )) 


## Bivariate Regression
# By hand
mu <- c(height = mean(trees$Height), volume = mean(trees$Volume))
s <- c(height = sd(trees$Height), volume = sd(trees$Volume))
unknown <- c(height = 84)
B1 <- (s['volume'] / s['height']) * r
B0 <- mu['volume'] - B1 * mu['height']

y_hat <- B0 + (B1 * unknown)

abline(v = unknown, h = y_hat, col = "blue")
png(file = "Lab4-cor-bireg.png") # NOTE: DO NOT use spaces in file names!
# The .png will save in your current, working directory

# Standardizing Method
# if standardized, y-hat = B1*x where B1 = r
unknown_std <- (84 - mean(trees$Height)) / sd(trees$Height)
y_hat_std <- r * unknown_std
y_hat_std * sd(trees$Height) + mean(trees$Height)
plot(x, y) + abline(lm(y ~ x), col="red") + abline(v = unknown_std, h = y_hat_std, col = "blue")

