################# 
## Homework #5 ##
#################

#Specify your namen cxv
my.name <- "Christian Conroy"
my.email <- "cmc454@georgetown.edu" cxv
 xcvn cv
# Code below does not load in train and test correctly. 
# library(rio)
#   df <- import("https://raw.githubusercontent.com /GeorgetownMcCourt/data-science/master/homework_data/accel.Rda")

# Download and load in instead. 
setwd("C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro")
df <- load("accel.Rda")

# Get rid of variable columns without data in train set
train <- train[-19]
sapply(train, function(x) sum(is.na(x)))

# Convert activity variable (our target variable) to factor
train$activity <- as.factor(train$activity)

# Get rid of variable columns without data in test set
test <- test[c(-4, -19, -21,-22,-23,-24,-25,-26,-27,-28)]
sapply(test, function(x) sum(is.na(x)))

#### K Folds Cross Validation Training - Go back in and add time! ####
# (Go back and check his grading to make sure I did this right last time.)?

# Creating the folds
vec <- runif(nrow(train))
folds <- cut(vec, 10, labels = FALSE)

# Affixing folds to observations
train$folds <- folds

# Ensuring that the groups are approximately equal 
table(train$folds)
# Each fold ends up with somewhere between 20187 and 20663 obervations. 

################# Training Classification Models and Predicting ###################
### Method 1: Multinomial Logistic Regression

# Load in package for multinomial logistic regression
library(nnet)

# Create the F1 Function 

meanf1 <- function(actual, predicted){
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
  # Evaluate the model by comparing the predicted versus actual values of the dependent variable. (Come back to and use better explanations and terms)
  F1 <- data.frame(trainpred$activity, trainpred$specpred)
  return(F1)
}

# Run a linear regression and get accuracy for train on kth folds (1-10)
spec1 <- as.formula("activity ~ accel + time + attitude_roll.radians. + attitude_pitch.radians. + attitude_yaw.radians. + rotation_rate_x.radians.s. + rotation_rate_y.radians.s. + rotation_rate_z.radians.s. + gravity_x.G. + gravity_y.G. + gravity_z.G. + user_acc_x.G. + user_acc_y.G. + user_acc_z.G. + magnetic_field_x.microteslas. + magnetic_field_y.microteslas.")
f <- 1:10
klogs <- lapply(f, logfunction, spec1)
klogs2 <- do.call(rbind, klogs)
klogsF1 <- meanf1(klogs2$trainpred.activity, klogs2$trainpred.specpred)

# F1 score of .9237

# Train the model on the entire train set and then produce the predictions for the test set
trainreg <- multinom(formula = spec1, data = train)
test$specpred <- predict(trainreg, newdata = test, type = 'class')

myPredictions <- subset(test, select=c("id", "specpred"))
colnames(myPredictions) <- c("id", "activity_logist")
summary(myPredictions$activity_logist)

# Might have great accuracy, but the fact that it predicted so few stairs predictions will lead us to choose a different classification model. 

#### KNN

# Setting up the data #
library(class)
train.gc <- train[complete.cases(train) ,c(-1,-3, -4)]
test.gc <- test[complete.cases(test), c(-1,-3)]
traincl <- train[complete.cases(train), 4]

# Use k values of 1, 5, and 20 to see how they perform in terms of correct proportion of a success rate. 
knn1 <- knn(train.gc, test.gc, traincl, k=1)
knn5 <- knn(train.gc, test.gc, traincl, k=5)
knn20 <- knn(train.gc, test.gc, traincl, k=20)

# Calculate Proportion of Correct Classified - Not sure where to go since we don't have values for the test set for activity here. 

# The below is from his original files, but probably will end up just sticking with the class package. 

knn.mean <- function(x_train, y_train, x_test, k){
  #
  # Calculates kNN for each value of a test sample based on the train sample
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
install.packages('rpart.plot')
library(rpart.plot)
library(ggplot2)
library(gridExtra)
library(plotROC)

# K Fold Cross Validation Function 
# Function to train, predict, and get F1 across folds. 
DTfunction <- function(k, spec) {
  # Create a train subset consisting of all folds that do not equal k-1.
  trainspec <- train[folds !=eval(k-1),]
  # Train the specification using the subset created. 
  specdt0 <- rpart(formula = spec, method = "class", data = train, cp = 0)
  xemin <- eval((fit.0$cptable[which.min(fit.0$cptable[,"xerror"])]) + (fit.0$cptable[which.min(fit.0$cptable[,"xerror"]),"xstd"]))
  specdtoptimal <- rpart(formula = spec, method = "class", data = train, cp = xemin)
  # Create a subset for prediction equal to the fold k
  trainpred <- train[folds == k,]
  # Predict by applying the specification trained on all folds not equal to k-1 to the subset for prediction equal to the fold k.
  trainpred$specpred <- predict(specdtoptimal, newdata = trainpred, type='class')
  # Evaluate the model by comparing the predicted versus actual values of the dependent variable. (Come back to and use better explanations and terms)
  F1 <- data.frame(trainpred$activity, trainpred$specpred)
  return(F1)
}


# Run a linear regression and get accuracy for train on kth folds (1-10)
spec5 <- as.formula("activity ~ accel + time + attitude_roll.radians. + attitude_pitch.radians. + attitude_yaw.radians. + rotation_rate_x.radians.s. + rotation_rate_y.radians.s. + rotation_rate_z.radians.s. + gravity_x.G. + gravity_y.G. + gravity_z.G. + user_acc_x.G. + user_acc_y.G. + user_acc_z.G. + magnetic_field_x.microteslas. + magnetic_field_y.microteslas. + avg50 + max50 + min50 + sd50 + avg100 + max100 + min100 + sd100")
f <- 1:10
kDTs <- lapply(f, DTfunction, spec5)
kDTs2 <- do.call(rbind, kDTs)
# Should we use f1 or should we use a confusion matrix? Why don't we use ROC here? Should we? Should we be graphing something? 
kDTsF1 <- meanf1(kDTs2$trainpred.activity, kDTs2$trainpred.specpred)

# F1 of 0.999323405446937 seems pretty good. 




### LEFT OVER CODE FROM BEFORE I CONVERTED IT ALL INTO CROSS FOLD VALIDATION STYLE!!!
#TRAIN MODEL
#Fit decision tree under default assumptions -- cp = 0.01
fit <- rpart(activity ~ accel + time + attitude_roll.radians. + attitude_pitch.radians. + attitude_yaw.radians. + rotation_rate_x.radians.s. + rotation_rate_y.radians.s. + rotation_rate_z.radians.s. + gravity_x.G. + gravity_y.G. + gravity_z.G. + user_acc_x.G. + user_acc_y.G. + user_acc_z.G. + magnetic_field_x.microteslas. + magnetic_field_y.microteslas. + avg50 + max50 + min50 + sd50 + avg100 + max100 + min100 + sd100, method = "class", data = train, cp = 0)

# Root node error specifies how many would be misclassified if we just assumed the node was the best example.
printcp(fit)

#Use rpart.plot 
rpart.plot(fit, shadow.col="gray", nn=TRUE)

#cp = 0
fit.0 <- rpart(activity ~ accel + time + attitude_roll.radians. + attitude_pitch.radians. + attitude_yaw.radians. + rotation_rate_x.radians.s. + rotation_rate_y.radians.s. + rotation_rate_z.radians.s. + gravity_x.G. + gravity_y.G. + gravity_z.G. + user_acc_x.G. + user_acc_y.G. + user_acc_z.G. + magnetic_field_x.microteslas. + magnetic_field_y.microteslas. + avg50 + max50 + min50 + sd50 + avg100 + max100 + min100 + sd100, method = "class", data = train, cp = 0)

#Examine printcp to find optimal cp value
#Find lowest xerror
printcp(fit.0)

xemin1 <- fit.0$cptable[which.min(abs(fit.0$cptable[,"CP"] - eval((fit.0$cptable[which.min(fit.0$cptable[,"xerror"]),"CP"])+(fit.0$cptable[which.min(fit.0$cptable[,"xerror"]),"xstd"]))))]

# Refit with optimal 
fit.opt <- rpart(activity ~ accel + time + attitude_roll.radians. + attitude_pitch.radians. + attitude_yaw.radians. + rotation_rate_x.radians.s. + rotation_rate_y.radians.s. + rotation_rate_z.radians.s. + gravity_x.G. + gravity_y.G. + gravity_z.G. + user_acc_x.G. + user_acc_y.G. + user_acc_z.G. + magnetic_field_x.microteslas. + magnetic_field_y.microteslas. + avg50 + max50 + min50 + sd50 + avg100 + max100 + min100 + sd100, method = "class", data = train, cp = xemin1)

rpart.plot(fit.opt, shadow.col="gray", nn=TRUE)

#Determine which variable had the greatest influence
fit.opt$variable.importance

#CALCULATE TEST ACCURACY
#Predict values for train set
pred.opt.train <- predict(fit.opt, train, type='class')
pred.0.train <- predict(fit.0, train, type='class')
pred.default.train <- predict(fit, train, type='class')

#Predict values for test set
pred.opt.test <- predict(fit.opt, test, type='class')
pred.0.test <- predict(fit.0, test, type='class')
pred.default.test <- predict(fit, test, type='class')







### Random Forest - FIX TO DEAL WITH THE FACT THAT A LOT OF THE TEST DATA IS FLAWED. 

#Load randomForest library
library(randomForest)

RFfunction <- function(k, spec) {
  # Create a train subset consisting of all folds that do not equal k-1.
  trainspec <- train[folds !=eval(k-1),]
  # Train the specification using the subset created. 
  fit.rf <- randomForest(formula = spec, method = "class", data = trainspec)
  fit.tune <- tuneRF(trainspec[,c(-1,-3,-4,-19,-20)], trainspec$activity, ntreeTry = 500, mtryStart = 1, stepFactor = 2, improve = 0.001, trace = TRUE, plot = TRUE)
  tune.param <- fit.tune[fit.tune[, 2] == min(fit.tune[, 2]), 1]
  
  # Create a subset for prediction equal to the fold k
  trainpred <- train[folds == k,]
  # Predict by applying the specification trained on all folds not equal to k-1 to the subset for prediction equal to the fold k.
  trainpred$specpred <- predict(fit.rf, trainpred, type='class')
  # Evaluate the model by comparing the predicted versus actual values of the dependent variable. (Come back to and use better explanations and terms)
  F1 <- data.frame(trainpred$activity, trainpred$specpred)
  return(F1)
}


# Run a linear regression and get accuracy for train on kth folds (1-10)
spec5 <- as.formula("activity ~ accel + time + attitude_roll.radians. + attitude_pitch.radians. + attitude_yaw.radians. + rotation_rate_x.radians.s. + rotation_rate_y.radians.s. + rotation_rate_z.radians.s. + gravity_x.G. + gravity_y.G. + gravity_z.G. + user_acc_x.G. + user_acc_y.G. + user_acc_z.G. + magnetic_field_x.microteslas. + magnetic_field_y.microteslas. + avg50 + max50 + min50 + sd50 + avg100 + max100 + min100 + sd100")
f <- 1:10
kRFs <- lapply(f, RFfunction, spec5)
kRFs2 <- do.call(rbind, kRFs)
# Should we use f1 or should we use a confusion matrix? Why don't we use ROC here? Should we? Should we be graphing something? 
kDTsF1 <- meanf1(kDTs2$trainpred.activity, kDTs2$trainpred.specpred)

# Leftover Code from Single Run without KFold CV

#Run Random Forest - In the actual homework assignment, you can extract the data using... 
fit.rf <- randomForest(activity ~ accel + time + attitude_roll.radians. + attitude_pitch.radians. + attitude_yaw.radians. + rotation_rate_x.radians.s. + rotation_rate_y.radians.s. + rotation_rate_z.radians.s. + gravity_x.G. + gravity_y.G. + gravity_z.G. + user_acc_x.G. + user_acc_y.G. + user_acc_z.G. + magnetic_field_x.microteslas. + magnetic_field_y.microteslas. + avg50 + max50 + min50 + sd50 + avg100 + max100 + min100 + sd100, method = 'class', data = train)
# Running 250 trees and that's what makes it computationally intensive. C and Java are faster and that's why a lot of these packages are developed in those. 

# What is it doing that takes this time: 
# Competing Ranger package can allocate each tree across cores. 

#Check OOB error
fit.rf

#Check tree decay
plot(fit.rf)

#Search for most optimal number of input features
fit.tune <- tuneRF(train[,c(-1,-3,-4,-19,-20)], train$activity, ntreeTry = 200, mtryStart = 1, stepFactor = 2, improve = 0.001, trace = TRUE, plot = TRUE)

# 2 gives us the lowest error here. 
fit.tune
tune.param <- fit.tune[fit.tune[, 2] == min(fit.tune[, 2]), 1]
# Above we're pulling out the best result. It's good to have this line of code if we are going to run tons of models. Saves a lot of time with manual copy between cross-validation log and the final model. 


#Predict values for train set
pred.rf.train <- predict(fit.rf, train, type='class')

#Predict values for test set
pred.rf.test <- predict(fit.rf, test, type='class')


#Your final submission should contain a data frame labeled ("myPredictions")

