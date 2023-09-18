## RM ANOVA

# Load packages
library(tidyverse)
library(datarium)
library(ez)
library(apaTables)

# Load data
data("selfesteem", package = "datarium")
str(selfesteem)

# Data Handling
d_long <- selfesteem %>%
  tidyr::pivot_longer(
  cols = c(t1, t2, t3),
  names_to = "time",
  values_to = "rating",
  names_transform = list(time = as.factor)) %>%
  dplyr::mutate(id = as.factor(id))

str(d_long)

d_long %>%
  dplyr::group_by(time) %>%
  dplyr::summarize("avg" = mean(rating), "stdv" = sd(rating))

## Run ANOVA
# Using aov()
aovmod <- aov(rating ~ time + Error(id/time), data = d_long)
# Error(): specify that we should partition residual error based
# on participants being nested in levels of the categorical IV
summary(aovmod)

# Using ez
aovez <- ez::ezANOVA(d_long, dv = rating, wid = .(id), within = .(time), detailed = TRUE)
# note: ges = generalized eta^2, can specify type [default = 2]
apaTables::apa.ezANOVA.table(aovez, correction = "GG", filename = "RManovatable.doc")
apaTables::apa.ezANOVA.table(aovez, correction = "none")

# Quick plot
bxp <- ggplot2::ggplot(d_long, aes(x = time, y = rating, colour = time)) +
  geom_boxplot()

# Follow-up with t.test()
t.test(rating ~ time, data = d_long, subset = (time %in% c("t1", "t2")), paired = TRUE)



## Run the RM ANOVA Manually
sstotal <- function(x) {
  return(sum((x - mean(x))^2))
}
ssbetween <- function(y, g){
  n <- table(g)
  x <- tapply(y, g, mean)
  return(sum(n * (x - mean(y))^2))
}
# Find the n and dfs
N <- nrow(d_long)
dfD <- nlevels(d_long$time) - 1
dfP <- nlevels(d_long$id) - 1
dfDxP <- dfD * dfP

# Find the SSs
ssT <- sstotal(d_long$rating)
ssD <- ssbetween(d_long$rating, d_long$time)
ssP <- ssbetween(d_long$rating, d_long$id)
ssDxP <- ssT - ssD - ssP

# Find the MSs
msD <- ssD / dfD
msDxP <- ssDxP / dfDxP

# Find the F-value
F_Drink <- msD / msDxP

# Find the F cv
F_crit <- qf(0.05, df1 = dfD, df2 = dfDxP, lower.tail=FALSE)


## Assignment
data("ToothGrowth")
ToothGrowth$Participant <- as.factor(rep(1:10))
# Focus only on dose, ignore supplement
tg <- ToothGrowth %>%
  dplyr::group_by(dose) %>%
  dplyr::summarize("avg" = mean(len), "stdv" = sd(len))
aovtg <- aov(len ~ dose + Error(Participant/dose), data = ToothGrowth)
summary(aovtg)
