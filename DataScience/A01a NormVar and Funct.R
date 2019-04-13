## Assignment 1 ##
## Part A
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/ProgrammingStats")
cardio <- read.csv("CardioGoodnessFit.csv", header=T)

# Writing my own functions #
# Average - His review - For this and all the others, can just use the same object name throughout (myAvg)
myAvg <- function(anyVector) {
  means <- mean(anyVector)
  return(means)
}

# Standard Deviation
mySD <- function(anyVector) {
  sds <- sd(anyVector)
  return(sds)
}

# Coefficient of Variance - Allows you to normalize the variation
install.packages('sjstats')
library(sjstats) # Do not need to do manually as there is a package for this. 
myCoefVar <- function(anyVector) {
  cofvar <- cv(anyVector)
  return(cofvar)
}

# Combining all into function that reports all three statistics neatly. 
myPrint <- function(anyVector) {
  means <- mean(anyVector)
  sds <- sd(anyVector)
  cofvar <- cv(anyVector)
  result <- c(means, sds, cofvar)
  names(result) <- c("Mean", "Std. Dev.", "Coef. of Var.")
  return(result)
}

# Testing the function
IncomeSum <- myPrint(cardio$Income)
MilesSum <- myPrint(cardio$Miles)
CombinedSum <- rbind(IncomeSum, MilesSum)
print(CombinedSum)

## Look back when he sends the answers at how to structure the lines of the tabale like he did using paste
