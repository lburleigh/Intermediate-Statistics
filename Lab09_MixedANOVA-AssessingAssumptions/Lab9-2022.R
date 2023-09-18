## Mixed ANOVA

# run mixed with aov and ez
# qqplot + discuss assumptions
# colour vs fill in ggplot? [fill = whole inside, colour = border usually]

# Load packages
library(datarium)
library(tidyverse)
library(rstatix)
library(ez)
library(apaTables)
library(ggpubr)

## Load data
data("anxiety", package = "datarium")
str(anxiety)

## Adjust tibble
longc <- anxiety %>% dplyr::filter(group == "grp1" | group == "grp2") %>%
    dplyr::rename("holiday" = "group",
           "twix" = "t1",
           "snickers" = "t2",
           "reeses" = "t3") %>%
    dplyr::mutate(holiday = factor(holiday, levels = c("grp1", "grp2"),
                            labels = c("Halloween", "Valentines"))) %>%
    tidyr::pivot_longer(cols = c(twix, snickers, reeses),
                        names_to = "candy",
                        values_to = "rating")
longc$candy <- as.factor(longc$candy)

## Assess data
longc %>% dplyr::group_by(holiday, candy) %>%
    dplyr::summarise("mean" = mean(rating), "sd" = sd(rating))

## Mixed ANOVA
# aov(y ~ group * RM + Error(participant/RM, data = data))
a <- aov(rating ~ holiday * candy + Error(id / candy), data = longc)
# Simple Effect
sea <- aov(rating ~ candy + Error(id / candy), data = longc,
         subset = holiday == "Halloween")
twixa <- aov(rating ~ holiday, data = longc, subset = candy == "twix")
summary(twixa)
# Double check dfs!
# total: nkc-1, between: nk-1, df-group: k-1, df-rm: c-1, df-rm-x-g: (k-1)(c-1)
eaov <- ez::ezANOVA(data = longc, dv = rating, wid = .(id),
                within = .(candy),
                between = .(holiday))
eaov

## Assessing Assumptions
# Outliers
longc %>% group_by(holiday) %>%
    rstatix::identify_outliers(rating)
ggplot2::ggplot(longc, aes(x = candy, y = rating, fill = candy)) +
    geom_boxplot() + facet_wrap(~holiday)

# Normality
# dv should be normally distributed in each cell
longc %>% group_by(holiday) %>%
    rstatix::shapiro_test(rating)
ggpubr::ggqqplot(filter(longc, holiday == "Halloween"), "rating") +
    facet_wrap(~candy) + theme_bw()

# Variance
# var of dv should be equal between groups of between subject factor
longc %>% rstatix::levene_test(rating ~ holiday)

## Sphericity
# See ez but also rstatix
# variance of differences b/t within sub groups should be equal
# ANOVA NOT robust to violations - MS_rm-x-part may not accurately describe residual var at
# specific level of rm or group
# y ~ b1*b2*w1 + Error(id/w1)
longc %>% rstatix::anova_test(rating ~ holiday * candy + Error(id/candy))

## Homogeneity of covariance
# covar matrices should be equal across cells of bet sub factors
rstatix::box_m(longc[, "rating", drop = FALSE], longc$holiday)

# plot
ggplot2::ggplot(longc, aes(x = candy, y = rating, fill = candy)) +
  geom_boxplot() + facet_wrap(~holiday) +
  scale_fill_manual(values=c("#F75F1C", "#881EE4", "#85E21F")) +
  theme_dark() +
  labs(title = "Candy Enjoyment on Holidays",
  x = "Candy",
  y = "Rating")


## Lab Assignment
data("anxiety", package = "datarium")

longc2 <- anxiety %>% dplyr::filter(group == "grp2" | group == "grp3") %>%
    dplyr::rename("holiday" = "group",
                  "twix" = "t1",
                  "snickers" = "t2",
                  "reeses" = "t3") %>%
    dplyr::mutate(holiday = factor(holiday, levels = c("grp2", "grp3"),
                                   labels = c("Halloween", "Valentines"))) %>%
    tidyr::pivot_longer(cols = c(twix, snickers, reeses),
                        names_to = "candy",
                        values_to = "rating")
a <- aov(rating ~ holiday * candy + Error(id / candy), data = longc2)
summary(a)

ggplot2::ggplot(longc2, aes(x = candy, y = rating, fill = candy)) +
    geom_boxplot() + facet_wrap(~holiday) +
    scale_fill_manual(values=c("#F75F1C", "#881EE4", "#85E21F")) +
    theme_dark() +
    labs(title = "Candy Enjoyment on Holidays",
         x = "Candy",
         y = "Rating")
