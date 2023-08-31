### Lab 1: Indexing and Subsetting


## Slides 1-4

## Set Working Directory
# file tab, gear, set
setwd("C:/Users/larn_/OneDrive - Louisiana State University/LSU/4111Stats_TA_fall22/Lab1")


## Read in data 
# Online data
# NOTE: Need to assign to object or will be lost
# Show first without assigning to name
shotlen <- read.csv("https://users.stat.ufl.edu/~winner/data/movie_avshotlength.csv")
# to view: click in environment or View(shotlen)
# Read from .csv
# Go to site [https://users.stat.ufl.edu/~winner/datasets.html]
# Search for 'shot length' to find
shotlen <- read.csv("movie_avshotlength.csv")


## Reference the dataframe
# Shape
dim(shotlen) # matches shotlen info in environment
nrow(shotlen)
ncol(shotlen)

# Variable info
names(shotlen)
str(shotlen)
summary(shotlen)
max(shotlen) # Show for error! Need to specify columns

# Report a column
# [row, col]
shotlen[,1]
shotlen[,1:3]
shotlen[,c(1,3)]
shotlen$ASL 
# Now that we know how to reference a column, how do we find 
# the maximum average shot length?
max(shotlen$ASL)

# Report a row
# Knowing how to do columns, how do you think we 
# can find rows?
shotlen[1,]
shotlen[1:3,]
shotlen[c(1,3),]

# Report a data point
# We want the first ASL, any guesses on how to do that with brackets?
shotlen[1,5]
# What about with variable names?
shotlen$ASL[1] # NOTE: R index starts with 1 [unlike python: 0]
# Can we find multiple points with variable names?
shotlen$ASL[c(1,3)]
# What if we want to look for relative data points?
# Find ASLs greater than 3
shotlen$ASL[shotlen$ASL > 3] # NOTE: spaces here are fine
# Find ASLs less than 3
shotlen$ASL[shotlen$ASL < 3]
# Can even assign to an object
short <- shotlen$ASL[shotlen$ASL < 3]

# Make new cols in dataframe
# Let's assign the row number to a new column
shotlen$rownumb <- 1:nrow(shotlen)
# What if we want a col indicating short or long ASL?
shotlen$short <- shotlen$ASL < 3


## Arithmetic
x <- 1
y <- x + 10
# Using functions
ASLmean <- mean(shotlen$ASL)


## Download the HappyFear dataset from Moodle and complete
## Activity 1
# Report the fourth value from the Age variable in the dataframe
HappyFeardata$Age[4]
# Report the sixth value from the fifth column in the dataframe
HappyFeardata[6, 5]
# Report the mean of the HappyRating variable in the dataframe
mean(HappyFeardata$HappyRating)
# Report the maximum of values 2 through 8 from the Age Variable
max(HappyFeardata$Age[2:8])
# Report the difference between value 6 from from the HappyRating and the FearRating
HappyFeardata$HappyRating[6] - HappyFeardata$FearRating[6]




