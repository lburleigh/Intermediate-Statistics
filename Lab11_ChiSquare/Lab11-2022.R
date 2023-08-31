## Lab 12

# Load packages
library(tidyverse)

# Load data
data("iris")


# Organize data
iris2 <- iris %>% mutate(size = ifelse(Sepal.Length < median(Sepal.Length),
                              "small", "big"))

sizestidy <- iris2 %>% count(Species, size) %>% #count the unique values of one or more variables
  pivot_wider(names_from = size, values_from = n)
# BUT tidyverse isn't always the best case
sizes <- table(iris2$Species, iris2$size)

# check types
class(sizestidy)
class(sizes)


# Chi Sq Test
both <- chisq.test(sizes, correct = FALSE) # goodness of fit
both$statistic # test statistic

big <- sizes[,1]

biggood <- chisq.test(big)
biggood

probs <- c(.10, .30, .60)
bigexp <- chisq.test(big, p = probs)
bigexp

ps <- c(10, 30, 60)
chisq.test(big, p = ps, rescale.p = TRUE)


# second method:
summary(sizes2)

# plot
ggplot(iris2, aes(x = Species, fill = size)) +
  geom_bar(position = "dodge")

iris.summary <- iris2 %>%
  group_by(Species, size) %>%
  summarise(
    sd = sd(Sepal.Length, na.rm = TRUE),
    len = mean(Sepal.Length)
  ) %>% filter(size == "big")

ggplot(iris.summary, aes(x = Species, y = len)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
              position=position_dodge(.9))


## Lab activity
# run chisq by hand
exp <- (probs) * 77
chi <- sum(((exp - big)^2) / exp )
pchisq(chi, df = 2, lower.tail = FALSE)

chisq.test(big, p = probs)


