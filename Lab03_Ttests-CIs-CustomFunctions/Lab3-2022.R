### Lab 3


## Get data
data('sleep') # Is it wide or long?
# Check n data points for each ID
table(sleep$ID)


## Independent Sample t-tests
# Let's imagine instead of each participant completing both 
# groups, we have 20 participants, half of each were assigned
# to each group. 
sleep$CreatedID <- seq.int(nrow(sleep))

# What test do we do? Independent t-test
t.test(
  x = sleep$extra[sleep$group == 1],
  y = sleep$extra[sleep$group == 2],
  var.equal = TRUE, # default: FALSE
  paired = FALSE, # default: FALSE
  alternative = "two.sided" # default: two-tailed/"two.sided"
)
# -OR-
# DV ~ IV
t.test(extra ~ group, data=sleep, paired = FALSE, var.equal = TRUE, alterative = "two.sided")

# What if one tailed?
t.test(extra ~ group, data=sleep, paired = FALSE, var.equal = TRUE, alterative = "greater")

# What is missing that we need to report in APA
# extra fun: tapply to determine group sds
tapply(sleep$extra, sleep$group, sd)


## Dependent Sample t-tests
# If each participant completed both groups, 
# as indicated in original ID column
t.test(
  sleep$extra[sleep$group == 1],
  sleep$extra[sleep$group == 2],
  paired = TRUE,
  var.equal = TRUE
)
# DV ~ IV
t.test(extra ~ group, data = sleep, paired = TRUE, var.equal = TRUE)


## Confidence Intervals
# find critical t
t_crit <- qt(
  p = .05,
  df = 10 - 1,
  lower.tail = FALSE
)

# Dependent - find differences
D <- sleep$extra[sleep$group == 1] - sleep$extra[sleep$group == 2]
# Why not tapply() here? - b/c use diff()
d2 <- tapply(sleep$extra, sleep$ID, diff)

# Custom SE Function
# What se do we use for dependent t-tests? One-sample se
# [write formula on board: se = sd / sqrt(n)] 
se_one_sample <- function(v, n) {
  s <- sqrt(v)
  se <- s / sqrt(n)
  return(se)
}
n <- length(D)
v <- var(D)
se <- se_one_sample(v, n)
grpmeans <- tapply(sleep$extra, sleep$group, mean)
grpdiff <- grpmeans[1] - grpmeans[2] # why not use diff here? lose direction
# NOTE: Notice that when we do this method with tapply, 
# grpdiff stays list cause started as list
ci_upper <- grpdiff + (t_crit * se)


## G*Power
# apriori: from the former
# post hoc: after this
