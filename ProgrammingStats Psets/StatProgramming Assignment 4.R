## Stat Programming Assignment 4 $$

## Part A ##
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/ProgrammingStats") 

if (!require(car)) {
  install.packages("car")
  require(car)
}

install.packages('dplyr')
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

# Load in the Data
benergy <- read.csv("buildingenergy.csv", header=T)

# HL Regression Model 

regHL <- lm(HL ~ RC + SA + WA + RA + OH + OR + GA + GD, data = benergy)
summary(regHL)

# - How do we interpret the stars?
# Most statsig at 0.001 level. GD statsig at .01 level. OR not statsig (p = .80). RA not defined because of singularities (Need to figure out)

# Get rid of RA as it appears to be perfectly multicollinear with other variables in the model. In this case, I assume this is because roof area will be directly related to surface area. 

regHL2 <- lm(HL ~ RC + SA + WA + OH + OR + GA + GD, data = benergy)
summary(regHL2)

# Same stat sig conclusions. 

plot(regHL2)
# RvF seems essentially random with some outliers at the bottom
# With QQ, some values at the lower end and maybe one at the upper end skew away from the line
# Scale-location similar conclusion as RvF
# Cook's 16, 22, and 24 have big impact.


#

# Testing whether log would be better. 
hist(benergy$HL)
hist(log(benergy$HL))
# Log does not look any more normal. Stick with non-logged. 

vif(regHL2)
# Looks like there is a major multicollinearity problem with relative compactness, surface area, wall area, and overall height, which isn't surprising given how related those are. We use a liberal threshold of 10 for multicollinearity here.  

regHL3 <- lm(HL ~ OR + GA + WA + GD, data = benergy)
summary(regHL3)

vif(regHL3)
# Much more tolerable now. 

benergy$reHL <- resid(regHL3)
summary(benergy$reHL)

# We lose statsig on GD and have a much lower R^2. 

X <- subset(benergy,select=-c(HL))
round(cor(X),2)

## Summary of regression model
# - What are residuals - Mean of 0 is probably a pretty good thing.
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -9.89654 -1.31964 -0.02522  0.00000  1.35321  7.70523 

# - What is residual standard error?
# - What is the difference between R^2^ and Adjusted-R^2^?
# R^2 is 0.9162 and adjusted R-sq is 0.9154, so it's pretty good. 
# - What does the p-value at the end tell us? p-value of < 2.2e-16 
# The F-value is highly significant implying that all the explanatory variables together significantly explain HL.

# Add most important predictors based on stat sig to markedown

# CL Regression Model 

regCL <- lm(CL ~ RC + SA + WA + RA + OH + OR + GA + GD, data = benergy)
summary(regCL)

# - How do we interpret the stars?
# Similar to HL. Most statsig at 0.001 level. OR not statsig (p = .24). RA not defined because of singularities (Need to figure out)

# Get rid of RA as it appears to be perfectly multicollinear with other variables in the model. In this case, I assume this is because roof area will be directly related to surface area. 

regCL2 <- lm(CL ~ RC + SA + WA + OH + OR + GA + GD, data = benergy)
summary(regCL2)

# Same stat sig conclusions.

plot(regCL2)
# RvF seems essentially random with some outliers at the top
# With QQ, some values at the top end and maybe one at the ottomb end skew away from the line
# Scale-location similar conclusion as RvF
# Cook's 596, 584 have big impact.

#

# Testing whether log would be better. 
hist(benergy$CL)
hist(log(benergy$CL))
# Log does not look any more normal. Stick with non-logged. 

vif(regCL2)
# Looks like there is a major multicollinearity problem with relative compactness, surface area, and overall height, which isn't surprising given how related those are. We use a liberal threshold of 10 for multicollinearity here.  

regCL3 <- lm(CL ~ OR + GA + WA + GD, data = benergy)
summary(regCL3)

vif(regCL3)
# Much more tolerable now. 

benergy$reCL <- resid(regCL3)
summary(benergy$reCL)

# We lose statsig on GD and have a much lower R^2. 

X <- subset(benergy,select=-c(HL))
round(cor(X),2)

## Summary of regression model
# - What are residuals - Mean of 0 is probably a pretty good thing.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -15.830  -6.919  -1.427   0.000   7.005  18.721 

# - What is residual standard error? 8.393 on 763 DF
# - What is the difference between R^2^ and Adjusted-R^2^?
# R^2 is 0.2257 and adjusted R-sq is 0.2217, so it's pretty good. 
# - What does the p-value at the end tell us? p-value of < 2.2e-16 
# The F-value is highly significant implying that all the explanatory variables together significantly explain CL.

# Add most important predictors based on stat sig to markedown

## Part B

# Load in and clean the data
kicks <- read.csv("kicksfootball.csv", header=T)
colnames(kicks) <- c("Yards", "Goal", "Practiceormatch")
kicks$Goal <- ifelse(toupper(kicks$Goal)=="Y", 1,0)

# Create two subsets 
practice <- kicks %>%
  filter(Practiceormatch == "P")

match <- kicks %>%
  filter(Practiceormatch == "M")
  
# Fit the logistic regression model 
practlog <- glm(Goal ~ Yards, 
             data=practice,
             family=binomial)
summary(practlog)

matchlog <- glm(Goal ~ Yards, 
                data=match,
                family=binomial)
summary(matchlog)

# Look back at rmd for guidance in comparing models. Lower AIC for practice means it's prob better. 

# Predicting 
inp <- c(40)
newdata = data.frame(Yards=inp)
predict(practlog, newdata, type="response")
predict(matchlog, newdata, type="response")
# Better probability will be made in match. 

## Part C
p <- ggplot(benergy) + aes(x = WA, y = HL) + geom_point(size = 3, color='red')
p

# The below might run in rmarkdown, but not sure yet. 
quantile(benergy$HL)
benergy$HLcat <- ifelse(benergy$HL <= 13, 1, ifelse(benergy$HL > 13 & benergy$HL <= 19, 2, ifelse(benergy$HL > 19 & benergy$HL <= 32, 3, ifelse(benergy$HL > 32, 4, NA))))
benergy$HLcat <- as.factor(benergy$HLcat)

quantile(benergy$RC)
benergy$RCcat <- ifelse(benergy$RC <= .6201, 1, ifelse(benergy$RC > .6201 & benergy$RC <= .6826, 2, ifelse(benergy$RC > .6826 & benergy$RC <= .7501, 3, ifelse(benergy$RC > .7501, 4, NA))))
benergy$RCcat <- as.factor(benergy$RCcat)

q <- ggplot(benergy) + aes(x = WA, y = HL) + geom_point(size = 1, color='red') + facet_wrap(HLcat~RCcat)
q



# Fit the linear models 
h <- ggplot(benergy) + aes(x = WA, y = HL) + geom_point(size = 3, color='red') + stat_smooth(method = "lm", size = 1)
print(h)

j <- ggplot(benergy) + aes(x = WA, y = HL) + geom_point(size = 3, color='red') + stat_smooth(method = "lm", size = 1) + facet_grid(HL~RC)
print(j)

# Second Order Polynomial 
z <- ggplot(benergy) + aes(x = WA, y = HL) + geom_point(size = 3, color='red') + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)
z

v <- ggplot(benergy) + aes(x = WA, y = HL) + geom_point(size = 3, color='red') + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + facet_grid(HL~RC)
print(v)

# Fit the loess models

k <- ggplot(benergy) + aes(x = WA, y = HL) + geom_point(size = 3, color='red') + stat_smooth(method = "loess", size = 1)
k

m <- ggplot(benergy) + aes(x = WA, y = HL) + geom_point(size = 3, color='red') + stat_smooth(method = "loess", size = 1) + facet_grid(HL~RC)
m

#2 probit
# Practice
plot(Goal~Yards,data=practice,
     xlab="Yards",
     ylab="Probability of Goal")
curve(predict(practlog,
              data.frame(Yards=x),
              type="resp"),
      add=TRUE)
par(new=TRUE)
plot(Goal~Yards,data=match,
     xlab="Yards",
     ylab="Probability of Goal")
curve(predict(matchlog,
              data.frame(Yards=x),
              type="resp"),
      add=TRUE)

##
unique(benergy$GD)
unique(benergy$OR)

q <- ggplot(benergy) + aes(x = WA, y = HL) + geom_point(size = 1, color='red') + facet_wrap(GD~OR)
q


