#############
##LECTURE 7##
#############

#Set your working drive
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro/lecture-07 Classifiers DTrees and RanFor")

#Libraries required
  install.packages(c("rpart","rpart.plot", "devtools", "gridExtra", "randomForest" ))
  devtools::install_github("sachsmc/plotROC")

#Load data 
  health <- read.csv("data/lecture7.csv")
  str(health)

#Randomly subset data into train and test
  set.seed(100)
  rand <- runif(nrow(health)) 
  rand <- rand > 0.5

#Create train test sets
  train <- health[rand == T, ]
  test <- health[rand == F, ]

##################
##DECISION TREES##
##################  
  #Load rpart libraries
    library(rpart)
    library(rpart.plot)
    library(ggplot2)
    library(gridExtra)
    library(plotROC)
  
  #TRAIN MODEL
  #Fit decision tree under default assumptions -- cp = 0.01
  
    fit <- rpart(coverage ~ age + wage + cit + mar + educ + race, 
                 method = "class", data = train)
    
    # The method here is classification, but decision trees are also good for regression problems. 
    # We didn't specify cp here, which is short for complexity parameter, which is a way of controlling for how far the tree gets grown down. Can specify an option to throw it out exhaustively. Can also use cp to prune the tree. 
  
  #Tools to review output (Read out of the model accuracy for each additional split that is added)
    # Root node error specifies how many would be misclassified if we just assumed the node was the best example (Makes sense here because we specified 50-50)
    printcp(fit)
    
    # Complexity parameter is a way of denoting how deep the parameter is 
    # x error is cross validared error and xstd is cross validated standard deviation
  
  #Use rpart.plot 
    rpart.plot(fit, shadow.col="gray", nn=TRUE)
  
  #cp = 0
    fit.0 <- rpart(coverage ~ age + wage + cit + mar + educ + race, 
                   method = "class", data = train, cp = 0)
    
  #Examine printcp to find optimal cp value
  #Find lowest xerror
    printcp(fit.0)
  # So line 19 looks to be the lowest. We'll add the xstd to the cross validation error because there is error within one sd from the error
  # We end up at line 16. Not sure why. 
    
  #Refit with optimal
    fit.opt <- rpart(coverage ~ age + wage + cit + mar + educ + race, 
                     method = "class", data = train, cp = 1.0885e-03)
    
    rpart.plot(fit.opt, shadow.col="gray", nn=TRUE)
  
  #Determine which variable had the greatest influence
    fit.opt$variable.importance
  
  # Example interp: Look for node with high sample size and high prediction rate. For example, if you are to look at this specific profile of people who fall into this specific coverage bucket
    
  #CALCULATE TEST ACCURACY
  #Predict values for train set
    pred.opt.train <- predict(fit.opt, train, type='prob')[,2]
    pred.0.train <- predict(fit.0, train, type='prob')[,2]
    pred.default.train <- predict(fit, train, type='prob')[,2]
    
    # Prediction with two classes will predict probabilities for y=0 and y=1, so we want the second column for y=1.
  
  #Predict values for test set
    pred.opt.test <- predict(fit.opt, test, type='prob')[,2]
    pred.0.test <- predict(fit.0, test, type='prob')[,2]
    pred.default.test <- predict(fit, test, type='prob')[,2]
  
  #Set up ROC inputs
    input.test <- rbind(data.frame(model = "optimal", d = test$coverage, m = pred.opt.test), 
                        data.frame(model = "CP = 0", d = test$coverage,  m = pred.0.test),
                        data.frame(model = "default", d = test$coverage,  m = pred.default.test))
    input.train <- rbind(data.frame(model = "optimal", d = train$coverage,  m = pred.opt.train), 
                         data.frame(model = "CP = 0", d = train$coverage,  m = pred.0.train),
                         data.frame(model = "default", d =  train$coverage,  m = pred.default.train))
    
  
  #Graph all three ROCs
    roc.test <- ggplot(input.test, aes(d = d, model = model, m = m, colour = model)) + 
      geom_roc(show.legend = TRUE) + style_roc()  + ggtitle("Test")
# In m = m, the second m is the probability. We're telling ggplot to treat this like an roc curve 
    roc.train <- ggplot(input.train, aes(d = d, model = model, m = m, colour = model)) + 
      geom_roc(show.legend = TRUE) + style_roc()  +ggtitle("Train")
  
  #Plot
    grid.arrange(roc.train, roc.test, ncol = 2)
    calc_auc(roc.test)
    
 ##################
##RANDOM FORESTS##
##################  
  
  #Load randomForest library
  library(randomForest)
  
  #Run Random Forest
  fit.rf <- randomForest(coverage ~ age + wage + cit + mar + educ + race, data = train)
  # Running 250 trees and that's what makes it computationally intensive. C and Java are faster and that's why a lot of these packages are developed in those. 
  
  # What is it doing that takes this time: 
  # Competing Ranger package can allocate each tree across cores. 
  
  #Check OOB error
  fit.rf
  
  #Check tree decay
  plot(fit.rf)
  
  #Search for most optimal number of input features
  fit.tune <- tuneRF(train[,-1], train$coverage, ntreeTry = 500, 
                     mtryStart = 1, stepFactor = 2, 
                     improve = 0.001, trace = TRUE, plot = TRUE)
  # 2 gives us the lowest error here. 
  fit.tune
  tune.param <- fit.tune[fit.tune[, 2] == min(fit.tune[, 2]), 1]
  # Above we're pulling out the best result. It's good to have this line of code if we are going to run tons of models. Saves a lot of time with manual copy between cross-validation log and the final model. 
  
  
  #Predict values for train set
  pred.rf.train <- predict(fit.rf, train, type='prob')[,2]
  
  #Predict values for test set
  pred.rf.test <- predict(fit.rf, test, type='prob')[,2]
  
  
  #Set up ROC inputs
  input.rf <- rbind(data.frame(model = "train", d = train$coverage, m = pred.rf.train), 
                    data.frame(model = "test", d = test$coverage,  m = pred.rf.test))
  
  #Graph all three ROCs
  roc.rf <- ggplot(input.rf, aes(d = d, model = model, m = m, colour = model)) + 
    geom_roc(show.legend = TRUE) + style_roc()  +ggtitle("Train")
  roc.rf
  
  # More kaggle challenges are being won with xgboost, which is creating a tree once split and then adding in successive order in ways to minimize error. 
    # You can imaging how you can add one decision to stump onto another decision to stump to get a more accurate model 
  
  # Adaptive boosting 
  
  #AUC
  calc_auc(roc.rf)

  