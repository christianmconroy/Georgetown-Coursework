#################
## Homework #5 ##
#################

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

#### Method 2: Decision Trees

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
  specdt0 <- rpart(formula = spec, method = "class", data = trainspec, cp = 0)
  xemin <- eval((specdt0$cptable[which.min(specdt0$cptable[,"xerror"])]) + (specdt0$cptable[which.min(specdt0$cptable[,"xerror"]),"xstd"]))
  specdtoptimal <- rpart(formula = spec, method = "class", data = trainspec, cp = xemin)
  # Create a subset for prediction equal to the fold k
  trainpred <- train[folds == k,]
  # Predict by applying the specification trained on all folds not equal to k-1 to the subset for prediction equal to the fold k.
  trainpred$specpred <- predict(specdtoptimal, newdata = trainpred, type='class')
  # Evaluate the model by comparing the predicted versus actual values of the dependent variable. (Come back to and use better explanations and terms)
  F1 <- data.frame(trainpred$activity, trainpred$specpred)
  return(F1)
}

# Run a linear regression and get accuracy for train on kth folds (1-10)
spec5 <- as.formula("activity ~ accel + time + attitude_roll.radians. + attitude_pitch.radians. + attitude_yaw.radians. + rotation_rate_x.radians.s. + rotation_rate_y.radians.s. + rotation_rate_z.radians.s. + gravity_x.G. + gravity_y.G. + gravity_z.G. + user_acc_x.G. + user_acc_y.G. + user_acc_z.G. + magnetic_field_x.microteslas. + magnetic_field_y.microteslas.")
f <- 1:10
kDTs <- lapply(f, DTfunction, spec5)
kDTs2 <- do.call(rbind, kDTs)
kDTsF1 <- meanf1(kDTs2$trainpred.activity, kDTs2$trainpred.specpred)

# F1 of 0.9952 seems pretty good. 

# Train the model on the entire train set and then produce the predictions for the test set
fit.0 <- rpart(formula = spec5, method = "class", data = train, cp = 0)
xemin <- eval((fit.0$cptable[which.min(fit.0$cptable[,"xerror"])]) + (fit.0$cptable[which.min(fit.0$cptable[,"xerror"]),"xstd"]))
fit.optimal <- rpart(formula = spec5, method = "class", data = train, cp = xemin)
specpred2 <- predict(fit.optimal, newdata = test, type='class')

myPredictions <- cbind(myPredictions, specpred2)
colnames(myPredictions) <- c("id", "activity_logist", "activity_dectree")
summary(myPredictions$activity_dectree)

# Predictions appear to be far better distributed across the four categories. 

#### Method 3: Random Forest 

#Load randomForest library
library(randomForest)

# Run the model (without separate k fold cross validation process this time.)
fit.rf <- randomForest(activity ~ accel + time + attitude_roll.radians. + attitude_pitch.radians. + attitude_yaw.radians. + rotation_rate_x.radians.s. + rotation_rate_y.radians.s. + rotation_rate_z.radians.s. + gravity_x.G. + gravity_y.G. + gravity_z.G. + user_acc_x.G. + user_acc_y.G. + user_acc_z.G. + magnetic_field_x.microteslas. + magnetic_field_y.microteslas., method = 'class', data = train)

#Check OOB error
fit.rf

#Check tree decay
plot(fit.rf)

#Search for most optimal number of input features
fit.tune <- tuneRF(train[,c(-1,-3,-4,-19)], train$activity, ntreeTry = 200, mtryStart = 1, stepFactor = 2, improve = 0.001, trace = TRUE, plot = TRUE)

# 3 gives us the lowest error here. 
fit.tune
tune.param <- fit.tune[fit.tune[, 2] == min(fit.tune[, 2]), 1]

# Rerun with best fit 
fit.opt.rf <-randomForest(activity ~ accel + time + attitude_roll.radians. + attitude_pitch.radians. + attitude_yaw.radians. + rotation_rate_x.radians.s. + rotation_rate_y.radians.s. + rotation_rate_z.radians.s. + gravity_x.G. + gravity_y.G. + gravity_z.G. + user_acc_x.G. + user_acc_y.G. + user_acc_z.G. + magnetic_field_x.microteslas. + magnetic_field_y.microteslas., method = 'class', data = train, mtry=tune.param, importance=TRUE,ntree=200)
print(fit.opt.rf)

# Above we're pulling out the best result. It's good to have this line of code if we are going to run tons of models. Saves a lot of time with manual copy between cross-validation log and the final model. 

#Predict values for test set
pred.rf.test <- predict(fit.opt.rf, test, type='class')

# Bind to data frame containing predictions from other classification models
myPredictions <- cbind(myPredictions, pred.rf.test)
colnames(myPredictions) <- c("id", "activity_logist", "activity_dectree", "activity_ranfor")
summary(myPredictions$activity_ranfor)

# Also looks like there is a more accurate distribution across classes compared to multinomial logistic. Overall, however, it seems that decision tree might be most useful in this case, although there is not necessarily a major change in terms of predictive ability compared to random forest. 

#Your final submission should contain a data frame labeled ("myPredictions")



