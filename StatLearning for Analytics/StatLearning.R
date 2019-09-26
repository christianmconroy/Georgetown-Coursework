## 1/16/19 - First Class Simiulation ##

## Figure out Latek Knit issue

# Curse of dimensionality issue 

d <- 2
n <- 50

# Sampling my 50 two dimensional databases
# Center the cube at 0 is why we do the 2 and - 0.5
nearest_neighbor <- function(d,n) {
  m <- 2* (matrix(runif(n * d), nrow = n) - 0.5)
  
  # find how far each is from the origin 
  dists <- apply(m, 1, function(x) sqrt(sum(x^2)))
  
  # To get nearest neigbor 
  d_nn <- min(dists)
  
  d_nn
}

nearest_neighbor(2, 50)

avg_nn <- function(d, n, n_samples) {
  nn_dists <- sapply(1:n_samples, function(i) nearest_neighbor(d,n))
  mean(nn_dists)
}

avg_nn(2, 50, 100)

# Lets do this for a bunch of different dimensions so I can see how the distance changes at dimensionality gets higher and higher 

d <- 1:30
nn_dist <- sapply(d, function(i) avg_nn(i,50, 100))

plot(d, nn_dist, pch=19)

# So as we go from one dimension to thirty dimension, in one dimension, distance was practically 0, but as we get bigger, it gets further from 0. 

# Exploratory dataset

install.packages('ISLR')
library(ISLR)
data(USArrests)

# Values that surprise you? 
summary(USArrests)
max(USArrests$Murder)
# Georgia where I am from is the most dangerous for murders 
# 91% of peiple in California are in urban

# Dimensions: 200
str(USArrests)


sapply(USArrests, function(x) sum(is.na(x)))
# No missing data

# Are variables related to each other
cor(USArrests)
# Not much of a correlation between urban and the crime 
# Clearly high between assault and murder and slightly high between rape and those two as well 

################## Full Arrests Analysis ####################
## Exploratory Data Analysis

# Loaad Data
data("USArrests")
head(USArrests)

## Load Packages 
library(ggplot2)
install.packages('usmap')
library(usmap)
library(data.table)

#### Initial Analysis 

summary(USArrests)

max(USArrests$Murder)

## Summary Stats
summary(USArrests)
# Obviously the most recorded incident is the least serious one with the mean for assault at 170.8
max(USArrests$Murder)
# Georgia where I am from is the most dangerous for murders 
max(USArrests$UrbanPop)
# 91% of peiple in California are in urban
max(USArrests$Rape)
# Nevada is most for rape
max(USArrests$Assault)
# North Carolina is most for assault

## Dimensions
dim(USArrests)
# There are 50*4 = 200 dimensions in the dataset

## Missingness
sapply(USArrests, function(x) sum(is.na(x)))
# No missing data

# Are variables related to each other
cor(USArrests)
# Not much of a correlation between urban and crime reporting as one would expect (Rape is the closest at 0.411)
# If a place has a lot of one type of crime, it makes sense that they would have a lot of another type of crime - Clearly high between assault and murder and slightly high between rape and those two as well, but not as high as one might expect 


##### Draw graphs to visualize correlations

# Urban Pop - Rape

ggplot(USArrests, aes(UrbanPop, Rape)) + geom_point() +
  geom_smooth()+
  labs(
    x = "Proportion living in Urban Areas",
    y= "Incidents of rape per 100,000 people"
  ) +
  scale_x_continuous(breaks = seq(20,100, by = 20)) +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50, by = 10))



# Urban Pop - Murder

ggplot(USArrests, aes(UrbanPop, Murder)) + geom_point() +
  geom_smooth()+
  labs(
    x = "Proportion living in Urban Areas",
    y= "Murders per 100,000 people"
  ) +
  scale_x_continuous(breaks = seq(20,100, by = 20)) +
  scale_y_continuous(limits = c(0,25), breaks = seq(0,25, by = 5))

# Not really any strong relationship, and changing geom_smooth to be method = lm does not change anything. 

# Urban Pop - Assault

ggplot(USArrests, aes(UrbanPop, Assault)) + geom_point() +
  geom_smooth(method = lm)+
  labs(
    x = "Proportion living in Urban Areas",
    y= "Incidents of rape per 100,000 people"
  ) +
  scale_x_continuous(breaks = seq(20,100, by = 20)) +
  scale_y_continuous(limits = c(0,400), breaks = seq(0,400, by = 100))

# Picture looks a bit rosier with lm, but still not a strong relationship as it could just as easily be flat within the confidence interval 

# Assault - Rape

ggplot(USArrests, aes(Assault, Rape)) + geom_point() +
  geom_smooth(method = lm)+
  labs(
    x = "Assaults per 100,000 people",
    y= "Incidents of rape per 100,000 people"
  ) +
  scale_x_continuous(breaks = seq(0,400, by = 100)) +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50, by = 10))

# While the correlation is still not that high, it would appear that there may be more of a story with urban and rape. 

# Assault - Murder

ggplot(USArrests, aes(Assault, Murder)) + geom_point() +
  geom_smooth()+
  labs(
    x = "Assaults per 100,000 people",
    y= "Murders per 100,000 people"
  ) +
  scale_x_continuous(breaks = seq(0,400, by = 100))
scale_y_continuous(breaks = seq(20,100, by = 20))

# High corr obviously with both loess and lm showing this 

# Rape - Murder

ggplot(USArrests, aes(Rape, Murder)) + geom_point() +
  geom_smooth()+
  labs(
    x= "Incidents of rape per 100,000 people",
    y = "Murders per 100,000 people"
  ) +
  scale_x_continuous(breaks = seq(0,50, by = 10)) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by = 5))

# High corr, but not that strong. LM would appear to show positive relationship. 

##### Mapping the rates in the states 
# Set the row names to be the first column for use in mapping
names(USArrests)[1] <- "state"

# Murder
plot_usmap(data = USArrests, values = "Murder", lines = "black") + 
  scale_fill_continuous(name = "Murders per 100,000 People", label = scales::comma, low = "white", high = "red") + labs(title = "Murder Rates in the US by State") + theme(legend.position = "right")

# Rape
plot_usmap(data = USArrests, values = "Rape", lines = "black") + 
  scale_fill_continuous(name = "Rapes per 100,000 People", label = scales::comma, low = "white", high = "red") + labs(title = "Rape Rates in the US by State") + theme(legend.position = "right")

# Assault
plot_usmap(data = USArrests, values = "Assault", lines = "black") + 
  scale_fill_continuous(name = "Assaults per 100,000 People", label = scales::comma, low = "white", high = "red") + labs(title = "Assault Rates in the US by State") + theme(legend.position = "right")

# Urban Population
plot_usmap(data = USArrests, values = "UrbanPop", lines = "black") + 
  scale_fill_continuous(name = "% Population Urban", label = scales::comma, low = "white", high = "red") + labs(title = "% Urban by US State") + theme(legend.position = "right")

############
# Notes 1/23/19

Scores <- data.frame(GPA = c(3.5, 4.0, 2.9, 3.3, 2.5, 4.0), Age = c(10,10,11,11,12,12), Height = c(65,67,55,45,60, 71))
r1 <- lm(GPA ~ Age + Height, data = Scores)
summary(r1)

Scores <- data.frame(GPA = c(3.5, 4.0, 2.9, 3.3, 2.5, 4.0), Age = c(10,10,11,11,12,12), Gender = c(65,67,55,45,60, 71))
r2 <- lm(GPA ~ Age + Height, data = Scores)
summary(r2)

## Neural Networks ##

## Neural Networks ##
my.nnet = function(B, w, x, v = c(1,0)) {
  x.tilde = c(1,x)
  y.in = B%*%x.tilde # Goes into hidden layer
  y = tanh(y.in) # Nonlinear function (hyperbolic tangent)
  y.tilde = c(l,y) 
  z.in = sum(w*y.tilde) # Goes into output node 
  z = tanh(y) # Single scalar output (1 - g^2) 
  z = v[1]*z + v[2] # Change scale of output if desired 
  return(z)
}
  
library('ISLR')
library('nnet')
library('pROC')

data('Auto')
Auto1 <- Auto

Auto1 <- Auto 
Auto1$name <- NULL
Auto1$mpg01 <- as.numeric(Auto1$mpg > median(Auto1$mpg))

Auto2 <- Auto1[,-1] # This would obviously be very correlated with the binary, so... 

plot(Auto2$horsepower, Auto2$weight, col = Auto2$mpg01 +1)
  
# Looks like they'd be good features for predicting the binary variable (so just use logistic regression)


# In-Class Exercise on 3/27/19
# Predict wage from age (+ year)

data("Wage")
head(Wage)

# Piecwise Step Regression 
table(cut(Wage$age,4))
# Here cut() automatically picked the cutpoints at 33.5, 49, and 64.5 years of age.
fit <- lm(wage ~ cut(age ,4),data=Wage)
coef(summary (fit))

# The age<33.5 category is left out, so the intercept coefficient of $94,160 can be interpreted as the average salary for those under 33.5 years of age, and the other coefficients can be interpreted as the average additional salary for those in the other age groups

train <- sample(nrow(Wage),(nrow(Wage) * .70))
test <- Wage[-train,]

fit <- lm(wage ~ cut(age ,4), data= Wage, subset = train)
coef(summary(fit))

# Make predictions on the test dataset here 
Wages.predicted = predict(fit, newdata = Wage[-train,])

# Calculate the mean squared error (this is your accuracy prediction. Don't forget this!)
sqrt(mean((Wages.predicted - Wage$wage[-train])^2))



