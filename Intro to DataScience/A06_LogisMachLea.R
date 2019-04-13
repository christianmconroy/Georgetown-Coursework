## Stat Programming Assignment 6 ##
# setwd("...") Make Syre to Set Appropriate Working Directory with the appropriate data set. 

## Part A

# Read in data with appropriate rows (Skippiong first row accomplishes this)
credit <- read.csv("default of credit card clients.csv", skip = 1, header = T)
# Check for missing 
sapply(credit, function(x) sum(is.na(x)))
# There does not appear to be any missing values and imputation will therefore be unnecessary. 

## Part B 
# Create a training dataset and a test dataset by randomly sampling 70% of the data to the training dataset and 30% to the test dataset
## Set the 70% for the training data set
smp_size <- floor(0.70 * nrow(credit))

## set the seed to make your partition reproductible
set.seed(246)
train_ind <- sample(seq_len(nrow(credit)), size = smp_size)

credit_train <- credit[train_ind, ]
credit_test <- credit[-train_ind, ]

# Use logistic regression to create a prediction model regressing y on all or some of the xvariables in the training set
names(credit_train)
credlog <- glm(default.payment.next.month ~ LIMIT_BAL + factor(SEX) + factor(EDUCATION) + factor(MARRIAGE) + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6  + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, 
                data=credit_train,
                family=binomial)
summary(credlog)

#  Use the model to predict y based on the test dataset

creditlog.fitted <- predict(credlog, newdata = credit_test ,type = 'response')

## Part C 
# Evaluate the model by comparing the predicted versus actual values of the dependent svariable.
creditlog.fitted <- ifelse(creditlog.fitted > 0.5,1,0)
misClassificationError <- mean(creditlog.fitted != credit_test$default.payment.next.month)
print(paste('Accuracy = ', 1-misClassificationError))

# The model accuary is approximately 81.36% depending on the sampling. 

# Plot the Receiver Operating Characteristic (ROC) curve
library(ROCR) # for the "prediction" function
p <- predict(credlog, credit_test, type="response")
pr <- prediction(p, credit_test$default.payment.next.month)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0, b= 1)

# ROC plot is not perfect. Somewhere in between a bad and a good predictive capability.

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# AUC of .7292126 depending on the sample. 
## The area under the ROC curve ( AUC ) is a measure of how well a parameter can distinguish between two diagnostic groups (e.g. diseased/normal, dead/alive, yes/no, etc.). The metric accounts for both the proportion of true positives and true negatives and the likelihood of predicting them incorrectly. Given that a truly useless model would have an area of 0.5 and a perfect model would have an area of 1.00, we rate our model as falling somewhere in between a bad and a good model, with a slight skew towards bad. 




