##########################
##LECTURE 5: OLS Example ##
##########################

#EXAMPLE 2 -- OLS from scratch
#Uses monthly 1-unit housing permits time series from the US Census Bureau

#Load library
  library(ggplot2)

#Load data
setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro/lecture-05 Supervised Learning and OLS/data")
out <- read.csv("building_permits.csv")

#Check file
  str(out[1:3,])
  out$date <- as.Date(paste0(out$date,"01"), "%Y%m%d")
  
#DATA PREP -------------
#Monthly line plot
  ggplot(out, aes(date, permits)) + geom_line()
  # y = b0 + d(monthly dummies) + time trend 

#Monthly dummies - model.matrix below creates the dummy matrix 
  month <- format(out$date, "%m")
  dummies <- model.matrix(~ month)
  colnames(dummies)

#Date Index - This helps us come up with the time trend (ascending) (Could've just doine this with the homework... No need for repeat...)
  date.index <- 1:nrow(out)

#Create matrix of input features
  X <- cbind(dummies[, -13], date.index)
  head(X)
  
#Get y as a vector - Permits is what we care about. 
  y <- out$permits

#MATH -------------
#Matrix multiply XT by X - Corresponds to slide 74 "Take inverse" part.
  a <- t(X) %*% X
  
#Solve for inverse of a - Solve is a way to solve a matrix (invert the matrix)
  w <- solve(a) %*% t(X) %*% y

#Compare w with lm() object 
  lm.obj <- lm(y ~ X[,-1])

#Consolidate and compare model coefficients
  comparison <- data.frame(`From Scratch` = w, 
                           `lm Function` = coef(lm.obj))

#Print
  print(comparison)
  
  # Replicate conditions needed to run a formula. W is the general formula for getting regression coefficients. 
  # For predict, it has to be a data frame.
  # What we just did above is a basic seasonal decomposition. We're getting a MAPE and essentially comparing the predicted to the actual. Time trend kind of fits like a line in the middle of all of them. 
  