# Question 1 #
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/ProgrammingStats/FinalExam/G22630214")
bank1 <- read.csv("bank1.csv")

# a. Use duration as the independent variable and develop four logistic regession models for four subsets of data. The data are based on loan (y/n) and default (no, yes, unknown)

# Delete all rows with unknown
bank1 <- bank1[!(bank1$y == "unknown" | bank1$loan == "unknown" | bank1$default == "unknown"),]

# recode yes to 1 and no to 0 (Can do because there are no NAs and only one other level)
bank1$y <- ifelse(bank1$y == "yes", 1, 0)
bank1$loan <- ifelse(bank1$loan == "yes", 1, 0)
bank1$default <- ifelse(bank1$default == "yes", 1, 0)

# # The above is the simpler way of doing the stuff below that I usually do. 
# bank1$y <- ifelse(bank1$y == "yes", 1, ifelse(bank1$y == "no", 0, NA))
# bank1$loan <- ifelse(bank1$loan == "yes", 1, ifelse(bank1$loan == "no", 0, NA))
# bank1$default <- ifelse(bank1$default == "yes", 1, ifelse(bank1$default == "no", 0, NA))

# Create Subsets 

LYDY <- bank1[bank1$loan == 1 & bank1$default == 1,]
LYDN <- bank1[bank1$loan == 1 & bank1$default == 0,]
LNDY <- bank1[bank1$loan == 0 & bank1$default == 1,]
LNDN <- bank1[bank1$loan == 0 & bank1$default == 0,]

# Plot the logistic regressions

#LYDY
plot(y~previous, data=LYDY, 
     xlab="Previous number of contacts",
     ylab="Probability of a term deposit") 
# fit a logistic regression model 
YY.log = glm(y ~ duration, data=LYDY, family=binomial) 
summary(YY.log)
curve(predict(YY.log, data.frame(duration=x), type="resp"), add=TRUE) 

# LYDN
plot(y~previous, data=LYDN, 
     xlab="Previous number of contacts",
     ylab="Probability of a term deposit") 
# fit a logistic regression model 
YN.log = glm(y ~ duration, data=LYDN, family=binomial) 
summary(YN.log)
curve(predict(YN.log, data.frame(duration=x), type="resp"), add=TRUE) 

# LNDY
plot(y~previous, data=LNDY, 
     xlab="Previous number of contacts",
     ylab="Probability of a term deposit") 
# fit a logistic regression model 
NY.log = glm(y ~ duration, data=LNDY, family=binomial) 
summary(NY.log)
curve(predict(NY.log, data.frame(duration=x), type="resp"), add=TRUE) 

# LNDN
plot(y~previous, data=LNDN, 
     xlab="Previous number of contacts",
     ylab="Probability of a term deposit") 
# fit a logistic regression model 
NN.log = glm(y ~ duration, data=LNDN, family=binomial) 
summary(NN.log)
curve(predict(NN.log, data.frame(duration=x), type="resp"), add=TRUE) 


# Write equations 
# LYDY -> log(p/1-p) = -5.97 + .0054(duration)  [62.79 AIC]
# LYDN -> log(p/1-p) = -5.81 + .0059(duration) [812.88 AIC]
# LNDY -> log(p/1-p) = -5.97 + .0084(duration) [89.89 AIC]
# LNDN -> log(p/1-p) = -5.31 + .0053(duration) [4231.4 AIC]

# Which models can the bank use
# As a lower AIC is better, we can conclude that the model with the Loan = No, Default = Yes subset is most suitable. 

# Question 2 #
cereal <- read.csv("cereal.csv")
names(cereal)

# a
mysummary <- function(df) {
  top <- df[order(df$rating),]
  subset <- head(top,5)
  cal <- mean(subset$calories)
  protein <- mean(subset$protein)
  fat <- mean(subset$fat)
  averages <- rbind(cal, protein, fat)
  return(averages)
}

mysummary(cereal)

# b 
dfq2 <- read.csv("cereal.csv")
x <- mysummary(dfq2)
sprintf("Average calories is %.2f, average protein is %.2f, and average fat is %.2f", x[1], x[2], x[3])

