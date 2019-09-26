#################
## Homework #4 ##
#################
# Kfolds, MAPE, VIF, Trim lapply, train.test

#Specify your name
my.name <- "Christian Conroy"
my.email <- "cmc454@georgetown.edu"

# #Load the data
# library(rio)
# github.url <- "https://github.com/GeorgetownMcCourt/data-science/blob/master/homework_data/"
# file.url <- "housing_sales.Rda?raw=true"
# download.file(paste0(github.url, file.url), "temp.Rda")
# load("temp.Rda")

# Route initially laid out above for downloading data was not working. Downloaded data directly and imported instead. 
# setwd(...) - Be sure to setwd to appropriate directory. 
housing <- load("housing_sales.Rda")

#Write your code and annotation below
#As those records are ones you'll predict

### 1. Write a set of two functions to perform k-folds cross validation. ###
## Function 1. Given a training set train, randomly assign each record to one of k-number of user specified partitions. Each of the k partitions should be roundly the same size. Return a vector of the partition assignments.

# Creating the folds
vec <- runif(nrow(train))
folds <- cut(vec, 10, labels = FALSE)

# Affixing folds to observations
train$folds <- folds

# Ensuring that the groups are approximately equal 
table(train$folds)
# Each fold ends up with somewhere between 24570 and 24889 obervations. 

# Function #2. Given train, the vector of k-assignments from function #1, and a model specification spec, write a function that will:
# - Run a linear regression using lm() on k ??? 1 folds, then predict the kth fold using lm.predict(), and save the accuracy (Mean Absolute Percent Error - see following section) for the predicted fold.
# - Repeat until each of the k folds has been predicted once. Return the mean and standard deviation of the k-folds process.
# - Note: a model specification is formulated using as.formula() (e.g. as.formula("y ~ x + x2")).

# Function to train, predict, and get the mape
housemape <- function(k, spec) {
  # Create a train subset consisting of all folds that do not equal k-1.
  trainspec <- train[folds !=eval(k-1),]
  # Train the specification using the subset created. 
  specreg <- lm(formula = spec, data = trainspec)
  # Create a subset for prediction equal to the fold k
  trainpred <- train[folds == k,]
  # Predict by applying the specification trained on all folds not equal to k-1 to the subset for prediction equal to the fold k.
  trainpred$specpred <- predict(specreg, newdata = trainpred)
  # Return the mape for the predictions. 
  return(100*mean(abs(trainpred$specpred/trainpred$sale.price-1), na.rm = T))
}

# Run a linear regression and get MAPE for train on kth folds (1-10)
spec1 <- as.formula("sale.price ~ gross.square.feet")
f <- 1:10
kmapes <- lapply(f, housemape, spec1)
# Calculate the average of the list of Mapes (k = 1:10)
mean(unlist(kmapes))
# Avg. MAPE of 50.21 is not great. 

## 2. Choose three candidate regression specifications and run each through your cross-validation functions.
# Specification #1
spec2 <- lm(sale.price ~ gross.square.feet + p.lotarea + p.bldgarea + p.comarea + p.resarea + p.officearea + p.retailarea + p.garagearea + p.strgearea + p.factryarea + p.otherarea, data = train )

# Testing for multicollinearity in chosen model
# May need to install packages for car package to use vif function
library(car)
vif(spec2)
# Delete those above 10 and use the resulting model in specification. 
spec2 <- as.formula("sale.price ~ gross.square.feet + p.officearea + p.strgearea + p.factryarea + p.otherarea")

# Specification #2
spec3 <- lm(sale.price ~ gross.square.feet + p.officearea + p.strgearea + p.factryarea + p.otherarea + p.numbldgs + p.numfloors + p.unitstotal, data = train )
# Testing for multicollinearity in chosen model
vif(spec3)
# All VIFs satisfactory
spec3 <- as.formula("sale.price ~ gross.square.feet + p.officearea + p.strgearea + p.factryarea + p.otherarea + p.numbldgs + p.numfloors + p.unitstotal")

# Specification #3
spec4 <- lm(sale.price ~ gross.square.feet + p.officearea + p.strgearea + p.factryarea + p.otherarea + p.numbldgs + p.numfloors + p.unitstotal + p.lotfront + p.lotdepth + p.bldgfront + p.bldgdepth + factor(p.lot.type) + p.past.alteration + p.extension + p.garage + p.basement + factor(bldg.category) + total.units + commercial.units + factor(school.distnum), data = train )
summary(spec4)
# Testing for multicollinearity in chosen model
vif(spec4)
# All VIFs satisfactory
spec4 <- as.formula("sale.price ~ gross.square.feet + p.officearea + p.strgearea + p.factryarea + p.otherarea + p.numbldgs + p.numfloors + p.unitstotal + p.lotfront + p.lotdepth + p.bldgfront + p.bldgdepth + factor(p.lot.type) + p.past.alteration + p.extension + p.garage + p.basement + factor(bldg.category) + total.units + commercial.units + factor(school.distnum)")

# Use lapply and mean to get mean for list of mapes for applying the housemape function to k folds using respective model specifications. 
kmapes2 <- lapply(f, housemape, spec2)
mean(unlist(kmapes2))
# Avg. MAPE of 50.21 is not great.
kmapes3 <- lapply(f, housemape, spec3)
mean(unlist(kmapes3))
# Avg. MAPE of 47.75 is better, but still not great. 
kmapes4 <- lapply(f, housemape, spec4)
mean(unlist(kmapes4))
# Avg. MAPE of 37.79. Satisfactory. 

## 3. Based on the best perform model in step 2, re-train model on the lowest error specification.
specreg2 <- lm(formula = spec4, data = train)

# Trim: Need to deal with new levels of factors in test set
test <- test[test$p.lot.type %in% train$p.lot.type,] 

# Predict with test using the specification trained on train set. 
test$specpred <- predict(specreg2, newdata = test)
my.result <- subset(test, select=c("id", "specpred"))
summary(my.result$specpred)

#Your final submission should contain a data frame labeled ("my.result")
