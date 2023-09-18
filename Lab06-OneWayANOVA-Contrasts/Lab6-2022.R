### Lab 6

## Load in Data
data("iris")
str(iris)

m1 <- lm(Sepal.Length ~ Species, data = iris)
summary(m1)
anova(m1)

a1 <- aov(Sepal.Length ~ Species, data = iris)
summary(a1)

# This looks exactly like what we did above... why do it with aov()?
# The output is understood by R to specifically be an ANOVA table...
TukeyHSD(m1)
TukeyHSD(a1)

# Contrasts ----
contrasts(iris$Species)

# Polynomial (non-linear) contrasts
iris$Species_Poly <- iris$Species
contrasts(iris$Species_Poly) <- contr.poly(3) # contrast based on orthogonal [uncorrelated] polynomials
round(contrasts(iris$Species_Poly), 3)

m2 <- lm(Sepal.Length ~ Species_Poly, data = iris)
summary(m2)
anova(m2)

# Compute the F-ratio by computing sums of squares ----
SSt <- function(y){
  sum((y - mean(y))^2)
}
SStot <- SSt(iris$Sepal.Length)

SSb <- function(y, g){
  n <- tabulate(y)
  sq <- ((y[unique(g)] - mean(y))^2) #(yj - mean(y))^2
  sum(n,sq)
}
SSbet <- SSb(iris$Sepal.Length, iris$Species)

SSw <- function(y, g) {
  grpg <- sum((y[unique(g)] - mean(y[unique(g)]))^2) #(yi - mean(y))^2
  sum(grpg)
}
SSwith <- SSw(iris$Sepal.Length, iris$Species)

## Find the MS-between
# SSb/dfb
betdf <- length(unique(iris$Species)) - 1
MSb <- SSbet/betdf

## Find the MS-within
# SSw/dfw
dfw <- sum(length(iris$Species)) - length(unique(iris$Species))
MSw <- SSwith/dfw

## Find the F
# MSb/MSw
Fval <- MSb/MSw

## Find the critical F-value
alpha <- .05
qf(alpha, betdf, witdf, lower.tail=FALSE)

## What is the p-value of our F statistic?
pf(Fval, dfb, dfw, lower.tail=FALSE)


# Protected t-test b/t setosa and virginica
x <- tapply(iris$Sepal.Length, iris$Species, mean)
n <- tabulate(iris$Species)
tprotect <- abs(x[1] - x[3]) / sqrt((MSw / n[1]) + (MSw / n[3]))


## Lab activity
# Solve for F ratio as you might by hand ----
# I am telling you that SS_total = 492.37 and means are:
# 
#     a      b       c      d
#    7.2    7.5     6.4     6.8
#      
# All groups have equal n = 67.
grp_means <- c(7.2, 7.5, 6.4, 6.8)
grnd_mean <- sum(grp_means) / 4
ss_bet_hand <- sum(67 * (grp_means - grnd_mean)^2)
ss_with_hand <- 492.37 - ss_bet_hand
msb <- ss_bet_hand/3
msw <- ss_with_hand/63
f <- msb/msw


# box plot
boxplot(Sepal.Length ~ Species, data = iris)

# Ggplot option [not to present, just for reference after lab]
library(ggplot2)
library(viridis) # color-blind friendly color palettes
# See this link for viridis info: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
# You can even see how different forms of color blindness see different palettes!
ggplot(iris, aes(x=Species, y=Sepal.Length, fill = Species)) + 
  geom_boxplot(outlier.colour="red", notch=TRUE) +
  stat_summary(fun=mean, geom="point", shape=20, size = 4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal()

# Set column to factor manually ----
data("ToothGrowth")
d <- ToothGrowth
str(d)
d$dose <- factor(d$dose, levels = c(0.5, 1.0, 2.0), labels = c("low", "mid", "high"))
