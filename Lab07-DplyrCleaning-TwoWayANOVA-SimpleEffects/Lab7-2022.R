### Lab 7

library(dplyr)
data("starwars")

# Only characters with no hair
starwars %>%
  dplyr::filter(hair_color == "none")

# Characters with no hair that aren't female
starwars %>%
  dplyr::filter(hair_color == "none" & sex != "female")
# -OR- 
starwars %>%
  dplyr::filter(hair_color == "none") %>%
  dplyr::filter(sex != "female")
# This is same as
starwars[starwars$hair_color == "none" & starwars$sex != "female", ]

# Arrange by birth year
# ascending order
starwars %>%
  filter(hair_color == "none" & sex != "female") %>%
  arrange(birth_year)
# descending order
starwars %>%
  filter(hair_color == "none" & sex != "female") %>%
  arrange(desc(birth_year))

# Turn height from cm to in
sw <- starwars %>% 
  mutate(height_in = height*0.393701)

# Put it together to clean the data set
# Watch order!
swclean <- starwars %>%
  mutate(height_in = height*0.393701) %>%
  filter(hair_color == "none" & sex != "female") %>%
  filter(!is.na(sex), !is.na(height)) %>%
  arrange(desc(birth_year)) %>%
  select(name, height, height_in, mass, hair_color, birth_year, sex, species)

## Summarize the data
# using summary() won't separate by groups!
swsummary <- swclean %>%
  group_by(species) %>%
  summarize("mean" = mean(height), "median" = median(height),
            "min" = min(height), "max" = max(height))

## Manipulate dataset format 
library(tidyr)
# Give each film it's own row
starwars %>% 
  dplyr::select(name, films) %>%
  tidyr::unnest_longer(films) #pivot_longer if not nested
# Give each film it's own column
starwars %>%
  dplyr::select(name, films) %>%
  tidyr::unnest_wider(films) #pivot_wider if not nested
## I wanted to use pivot_longer and/or pivot_wider as I think these are more
## commonly helpful but couldn't find a nice easy way to include them


### If done early, run through quick two-way ANOVA and simple effects
## ANOVA
data("ToothGrowth")
ToothGrowth$dose <- factor(ToothGrowth$dose, levels = c(0.5, 1.0, 2.0), labels = c('low','mid', 'high'))
m1 <- lm(len ~ dose * supp, data = ToothGrowth)
summary(m1)
anova(m1)

# Because of the significant interaction, we should follow up with a simple
# effects analysis: refit the model looking at only the VC condition, or only
# the OJ condition.
a2.OJ <- aov(len ~ dose, data = ToothGrowth, subset = (supp == "OJ"))
a2.VC <- aov(len ~ dose, data = ToothGrowth, subset = (supp == "VC"))
summary(a2.OJ)
summary(a2.VC)

a2.low <- aov(len ~ supp, data = ToothGrowth, subset = (dose == "low"))
a2.mid <- aov(len ~ supp, data = ToothGrowth, subset = (dose == "mid"))
a2.high <- aov(len ~ supp, data = ToothGrowth, subset = (dose == "high"))
summary(a2.low)
summary(a2.mid)
summary(a2.high)