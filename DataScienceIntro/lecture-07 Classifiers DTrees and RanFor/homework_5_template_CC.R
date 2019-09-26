#################
## Homework #5 ##
#################

# AMA Questions

# 2. Can you explain how lambda works with LASSO? Do we need to create a vector of lambdas to be able to do the analysis?  
# In Lasso, the value of lambda is tested along a grid. Whichever error or deviance is the lowest is the one that you choose. 

#Specify your name
my.name <- "Christian Conroy"
my.email <- "cmc454@georgetown.edu"

#Write your code and annotation below
#As those records are ones you'll predict

# Code below does not load in train and test correctly. 
# library(rio)
#   df <- import("https://raw.githubusercontent.com/GeorgetownMcCourt/data-science/master/homework_data/accel.Rda")

# Download and load in instead. 
setwd("C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro")
df <- load("accel.Rda")
    
#### K Folds Cross Validation Training ####
# (Go back and check his grading to make sure I did this right last time.)?

# Creating the folds
vec <- runif(nrow(train))
folds <- cut(vec, 10, labels = FALSE)

# Affixing folds to observations
train$folds <- folds

# Ensuring that the groups are approximately equal 
table(train$folds)
# Each fold ends up with somewhere between 1418 and 1541 obervations. 

#### Training Classification Models ####
# Clean up variables 
class(train$activity)
train$activity <- as.factor(train$activity)
levels(train$activity)


### Multinomial Logistic Regression
# Resource: https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/

# Load in package for multinomial logistic regression
library(nnet)

# Create the F1 Function 

F1 <- function(actual, predicted){
  # Desc:
  # Weighted mean F1 score function
  #
  # Args:
  # actual = a vector of actual labels
  # predicted = predicted labels
  #
  # Returns:
  # Single value F1 score
  #
  classes <- unique(actual)
  results <- data.frame()
  #Loop through all classes
  for(k in classes){
    prec <- sum(predicted == k & actual == k)/sum(predicted == k)
    rec <- sum(predicted == k & actual == k)/sum(actual == k)
    results <- rbind(results,
                     data.frame(class.name = k,
                                weight = sum(actual == k)/length(actual),
                                precision = prec,
                                recall = rec))
  }
  #Calculate weighted mean
  score <- results[,2] * 2 * (results[,3] * results[,4]) / (results[,3] + results[,4])
  return(sum(score, na.rm = TRUE))
}

# Function to train, predict, and get F1 across folds. 
logfunction <- function(k, spec) { 
  # Create a train subset consisting of all folds that do not equal k-1.
  trainspec <- train[folds !=eval(k-1),] 
  # Train the specification using the subset created.
  specreg <- multinom(formula = spec, data = trainspec)
  # Create a subset for prediction equal to the fold k
  trainpred <- train[folds == k,]
  # Predict by applying the specification trained on all folds not equal to k-1 to the subset for prediction equal to the fold k.
  trainpred$specpred <- predict(specreg, newdata = trainpred, type = 'class')
  # Evaluate the model by comparing the predicted versus actual values of the dependent variable. 
  F1 <- data.frame(trainpred$activity, trainpred$specpred)
  return(F1)
}

# Run a linear regression and get accuracy for train on kth folds (1-10)
spec1 <- as.formula("activity ~ accel")
f <- 1:10
klogs <- lapply(f, logfunction, spec1)
klogs2 <- do.call(rbind, klogs)
klogsF1 <- meanf1(klogs2$trainpred.activity, klogs2$trainpred.specpred)

# F1 score of .872394

# Now train the model on the entire train set and then produce the predictions for the test set
trainreg <- multinom(formula = spec1, data = train)
test$specpred <- predict(trainreg, newdata = test, type = 'class')

myPredictions <- subset(test, select=c("id", "specpred"))
summary(myPredictions$specpred)

# Should I be concerned that it predicted 0 stairs? 

### LASSO - Not sure how to do this with k-fold cross validation. 

# Reference: https://www.r-bloggers.com/ridge-regression-and-the-lasso/ (And Lecture 6 simulated r script)

library(class)
#MAPE
mape <- function(y, x){
  return(100 * mean(abs(y / x - 1)))
}

 cv
#### KNN
# Reference: Week 6 Folder from Intro to Data Science

traincl <- factor(train[,4])
knn(train[complete.cases(train),], test[c cvx c cvx cxvomplete.cases(test),], traincl, k=3)

knn.mean <- function(x_train, y_train, x_test, k){
  # vc
  # Calculates kNN for each value of a te cxvst sample based on the train sample
  #
  # Args:
  #   x_train, y_train: x data frame and y vector of training set
  #   x_test: x data frame of test set
  #   k: number of neighbors
  #
  # Result:
  #   Vector of predicted target value for test set
  
  #Set vector of length of test set
  output <-  vector(length = nrow(x_test))
  
  #Loop through each row of the test set
  for(i in 1:nrow(x_test)){
    
    #extract coords for the ith row
    cent <- x_test[i,]
    
    #Set vector length
    dist <- vector(length = nrow(x_train))
    
    #Calculate distance by looping through inputs
    for(j in 1:ncol(x_train)){
      dist <- dist + (x_train[, j] - cent[j])^2
    }
    dist <- sqrt(dist)
    
    #Calculate rank on ascending distance, sort by rank
    df <- data.frame(id = 1:nrow(x_train),rank = rank(dist))
    df <- df[order(df$rank),]
    
    #Calculate mean of obs in positions 1:k, store as i-th value in output
    output[i] <- mean(y_train[df[1:k,1]], na.rm=T)
  }
  return(output)
}

#Optimization code
knn.opt <- function(x_train, y_train, x_test, y_test, max, step){
  #
  # Returns a log of RMSE for various values of k in a kNN for continuous values
  #
  # Args:
  #   x_train, y_train: x data frame and y vector of training set
  #   x_test, y_test: x data frame and y vector of test set
  #   max: maximum number of k's to be tested
  #   step: increments to be test from 2 to max
  #
  # Result:
  #   Data frame containing k and RMSE
  
  #create log placehodler
  log <- data.frame()
  
  for(i in seq(1, max, step)){
    #Run KNN for value i
    yhat <- knn.mean(x_train, y_train, x_test, i)
    
    #Calculate RMSE
    rmse <- round(sqrt(mean((yhat  - y_test)^2, na.rm=T)), 3)
    
    #Add result to log
    log <- rbind(log, data.frame(k = i, rmse = rmse))
  }
  
  #sort log
  log <- log[order(log$rmse),]
  
  #return log
  return(log)
}

# Set up train/test (Go back through code and make sure I'm just doing this once instead of for each measure. )

#Train 
y.train <- train$activity
x.train <- as.matrix(train)

#Test
y.test <- test$activity
x.test <- as.matrix(test)

#Optimize KNN at one k increments
logs <- knn.opt(x.train, y.train, x.test, y.test, nrow(x.test), 1)

# Stuck here because of the categorical problem with the variable 


## Decision Trees

#Load rpart libraries
library(rpart)
library(rpart.plot)
library(ggplot2)
library(gridExtra)
library(plotROC)

#TRAIN MODEL
#Fit decision tree under default assumptions -- cp = 0.01
train$diff100 <- eval(train$max100-train$min100)
train$diff50 <- eval(train$max50-train$min50)
fit <- rpart(activity ~ accel + diff100 + diff50, 
             method = "class", data = train)

# Root node error specifies how many would be misclassified if we just assumed the node was the best example (Makes sense here because we specified 50-50)
printcp(fit)

#Use rpart.plot 
rpart.plot(fit, shadow.col="gray", nn=TRUE)

fit.0 <- rpart(activity ~ accel + diff100 + diff50, 
             method = "class", data = train, cp = 0)

#Examine printcp to find optimal cp value
#Find lowest xerror
printcp(fit.0)

# Refit with optimal 
fit.opt <- rpart(activity ~ accel + diff100 + diff50, 
               method = "class", data = train, cp = 0.049860)

rpart.plot(fit.opt, shadow.col="gray", nn=TRUE)

#Determine which variable had the greatest influence
fit.opt$variable.importance

#CALCULATE TEST ACCURACY
#Predict values for train set
pred.opt.train <- predict(fit.opt, train, type='prob')[,2]
pred.0.train <- predict(fit.0, train, type='prob')[,2]
pred.default.train <- predict(fit, train, type='prob')[,2]

# Prediction with two classes will predict probabilities for y=0 and y=1, so we want the second column for y=1.

#Predict values for test set
pred.opt.test <- predict(fit.opt, test, type='prob')[,2]
pred.0.test <- predict(fit.0, test, type='pro b')[,2]
pred.default.test <- predict(fit, test, type='prob')[,2]


#Your final submission should contain a data frame labeled ("myPredictions")
