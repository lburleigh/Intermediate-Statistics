### Lab 5

## Load data ----
data('iris')
str(iris)
summary(iris)
hist(iris$Petal.Length)
plot(iris$Petal.Length, iris$Sepal.Length)


## Arguments for plots
# Change the axis labels
plot(
  x = iris$Petal.Length,
  y = iris$Sepal.Length,
  xlab = "Petal Length",
  ylab = "Sepal Length",
  main = "Iris Measurements",
  # Change axis limits
  xlim = c(0, 8),
  ylim = c(2, 9),
  # Change color
  col = "blue",
  # Rainbow option: col = rep(1:8, 7)[1:50] 
  # Plot without box
  axes = FALSE
  )
# Add lines for each side
axis(side = 1)
axis(side = 2)
# Add line of best fit with linear model
m <- lm(Sepal.Length ~ Petal.Length, data = iris)
abline(m, lty = 4, col = "green")
# Add regression formula
B <- coefficients(m)
eq <- sprintf("%.2f + (%.2f * length)", B[1], B[2])
legend(x = 0, y = 5, legend = eq, lty = 1, col = 1, bty = 'n')
# NOTICE: legend doesn't go by y axis but by y coordinate space

## Bar Plots
# Have 2 variables to consider
iris$Death <- ifelse(iris$Sepal.Length > 5, 'Yes', 'No')
# find average of petal length as separated by species
x <- tapply(iris$Petal.Length, list(iris$Death, iris$Species), mean) # list allows coersion while c requires same dimensions

# Make bar plot
barplot(x, xlab = "Species", ylab = "Petal Length", main = "Survival of Iris")
legend("topleft", legend = c("No", "Yes"), fill = grey.colors(2))

# make bars side by side with beside
barplot(x, beside = TRUE, xlab = "Species", ylab = "Petal Length", main = "Survival of Iris")
legend("topleft", legend = c("No", "Yes"), fill = grey.colors(2))

# Add error bars with script
source('add_errorbars.R')
print(add_errorbars)
# Find porportion of people that survived for each class
p <- x["Yes", ] / colSums(x)
mid <- barplot(p, xlab = "Species", ylab = "Proportion", main = "Survival of Iris", ylim = c(0, 1))
add_errorbars(x = mid, y = p, w = 0.1)
# Add horizontal lines
add_errorbars(x = mid, y = p, w = 0.1, horiz = .4) 
# change horiz smaller and plot without clearing

png(file = "species-survival.png")


## Nifty package function of the day
library(psych)
pairs.panels(iris, bg="yellow", pch=21, stars=TRUE)
