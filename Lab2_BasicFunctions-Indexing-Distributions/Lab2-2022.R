### Lab 2

## Setup data
# Participant IDs
firstcol <- c("001", "002", "003", "004", "005", "006")
# Data
secondcol <- c(7, 5, 2, 3, 4, 8)
# Dataframe
df <- data.frame(firstcol, secondcol)
str(df)


## Core arithmetic functions
mean(df$secondcol) # How would we write this with brackets?
mean(df[,2])
sd(df$secondcol)
sum(df$secondcol)
length(df$secondcol) 
length(df) # NOTICE this gives number of columns
sqrt(df$secondcol) # NOTICE takes sqrt of each point 


## Column Arithmetic
 # We can save calculations as a new col!
df$sqroot <- sqrt(df$secondcol)
df$double <- df$secondcol*2
 # We can also do calculations of a subset based on a column 
(df$secondcol[df$secondcol > 4])/2


## Indexing
# Numerical
# We talked about numerical indexing last time
df$secondcol[1:4]
# Logical Indexing - True/False
# ==, >/<, |, &, !
df$secondcol == 3  # where is value 3 in second col
df$secondcol != 3
# How do we find the ID of this data point?
df$firstcol[df$secondcol == 3] 
# We can also build these concepts - sub-setting within functions
mean(df$sqroot[df$secondcol > 3])
# How do we specify for data points that fall b/t 3 - 7
mean(df$sqroot[df$secondcol > 3 & df$secondcol < 7])


## Normal Distributions
# pnorm()
?pnorm() #note stats package in help
# What does a pnorm of 1 give us?
stats::pnorm(1) # gives proportion of scores LESS THAN OR EQUAL TO z score of 1
pnorm(1, lower.tail = FALSE) # focuses on upper tail
# If we want to determine what standardized score 
# gives 95th percentile
qnorm(.95)
# What happens if we put 1.644854 in pnorm()?
pnorm(1.6448, lower.tail = FALSE) # p = .05



## Lab Activity
data(sleep)
?sleep

# Q1 - How much extra sleep was reported by the participant with ID 3 in 
# group 2?
sleep$extra[sleep$ID == 3 & sleep$group == 2]

# Q2 - If the population from which these observations were drawn has mu = 0 
# and sigma = 2, what is the "extra" value of the participant with ID 2 in 
# Group 1 in units of the standard deviation of the population?
sleep$z <- (sleep$extra - 0)/2
sleep$z[sleep$ID == 2 & sleep$group == 1]

# Q3 - Assuming the same facts about the population (mu = 0, sigma = 2), 
# report the "extra" value for participant ID 7 in Group 2 in standard 
# deviation units of it's sampling distribution?
sleep$stand <- (sleep$extra[sleep$group == 2])/(2/sqrt(10))
sleep$stand[sleep$ID == 7 & sleep$group == 2]
