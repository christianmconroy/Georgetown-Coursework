## Curse of Dimensionality

nearest_neighbor <- function(d, n) {
  
  m <- 2 * (matrix(runif(n * d), nrow = n)) - 0.5
  
  dists <- apply(m, 1, function(x) sqrt(sum(x^2)))
  d_nn <- min(dists)
  
  d_nn
  
}

nearest_neighbor(2, 50)

avg_nn <- function(d, n, n_samples) {
  
  nn_dists <- sapply(1:n_samples, function(i) nearest_neighbor(d, n))
  mean(nn_dists)
  
}

avg_nn(2, 50, 100)

d <- 1:30

nn_dist <- sapply(d, function(i) avg_nn(i, 50, 100))

plot(d, nn_dist, pch = 19)

###################

## Exploratory Data Analysis

data("USArrests")

head(USArrests)
summary(USArrests)

max(USArrests$Murder)

library(ggplot2)

## Draw graphs to visualize correlations

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

# Urban Pop - Assault

ggplot(USArrests, aes(UrbanPop, Assault)) + geom_point() +
  geom_smooth()+
  labs(
    x = "Proportion living in Urban Areas",
    y= "Incidents of rape per 100,000 people"
  ) +
  scale_x_continuous(breaks = seq(20,100, by = 20)) +
  scale_y_continuous(limits = c(0,400), breaks = seq(0,400, by = 100))

# Assault - Rape

ggplot(USArrests, aes(Assault, Rape)) + geom_point() +
  geom_smooth()+
  labs(
    x = "Assaults per 100,000 people",
    y= "Incidents of rape per 100,000 people"
  ) +
  scale_x_continuous(breaks = seq(0,400, by = 100)) +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50, by = 10))

# Assault - Murder

ggplot(USArrests, aes(Assault, Murder)) + geom_point() +
  geom_smooth()+
  labs(
    x = "Assaults per 100,000 people",
    y= "Murders per 100,000 people"
  ) +
  scale_x_continuous(breaks = seq(0,400, by = 100))
  scale_y_continuous(breaks = seq(20,100, by = 20))

# Rape - Murder

ggplot(USArrests, aes(Rape, Murder)) + geom_point() +
  geom_smooth()+
  labs(
    x= "Incidents of rape per 100,000 people",
    y = "Murders per 100,000 people"
  ) +
  scale_x_continuous(breaks = seq(0,50, by = 10)) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by = 5))

# 3.7 #13
set.seed(1)
x <- rnorm(100, 0, 1)
eps <- rnorm(100, 0.25)
y <- -1 + (0.5 * x)
# Length of vector y is 100, B0 is -1, B2 is -0.5
plot(x,y)
reg <- lm(y ~ x)
plot(x,y)
abline(reg)


library(MASS)
install.packages('SmartEDA')
library(SmartEDA)
fix(Boston)
# The below is super useful!
ExpData(data = Boston, type = 1)
# Dis is weighted mean of distance to five Boston employment centera, rad is index of accessibility to radial highways, and tax is full-value property tax rate per $10,0000. No categorical
reg <- lm(medv ~ tax + rad + dis, data = Boston)
summary(reg)

summary(scale(reg))
# Before standardizing, rad looks the most important, though it is one less degree of statistical significance compared to tax

# To compare, use standarsdized coefficients
reg2 <- lm(scale(medv) ~ scale(tax) + scale(rad) + scale(dis), data = Boston)
summary(reg2)

# Tax seems to be the most important once we standardized. Dis is least important. 

# One predictor in each model 
reg3 <- lm(medv ~ tax, data = Boston)
summary(reg3) # 0.2195 (Shows that it's most important)
reg4 <- lm(medv ~ rad, data = Boston)
summary(reg4) # 0.14
reg5 <- lm(medv ~ dis, data = Boston)
summary(reg5) # .06246

# The fact that something loses statistical significance was explained due to potential colinearity once both of those are in the model

## 1/30/19
library(ISLR)
data(Auto)
head(Auto)

lm_hwpr <- lm(mpg ~ horsepower, data = Auto)
# Before looking at summary, we want to verify if the assumptions are correct
plot(Auto$horsepower, Auto$mpg)
# This doesn't look linear.

errors <- residuals(lm_hwpr)
plot(Auto$horsepower, errors)
# It would be a flat line if it was linear, so clearly there is a problem here

# So what do we do?!
# Well it looks like we should inverse given the plot we saw. 
lm_inv <- lm(mpg ~ I(horsepower^-1), data = Auto)
plot(Auto$horsepower, residuals(lm_inv))
# Now it looks flatter, so this might be better

# If things were going up and down, try polynomials, for example

lm_inv <- lm(mpg ~ I(horsepower^-1) + weight, data = Auto)
plot(predict(lm_inv), residuals(lm_inv))
# Standard thing to do in these situations is to plot Predicted Y versus residuals
# Not great but kind of looks clustered around 0. 
# The variance appears to be making a cone though, which is a problem. There is not a fixed variance then (i.e heteroskedasticty problem) - When my Y values are bigger, my spread will be bigger
  # Sandwich estimators/ Robust standard errors are not the only solution to this. Can also try transforming your Y values
  # Often it's good to transform Xs to fix linearity problems and then transform Ys to fix potential heteroskedasticity   - Can use square root or log in this case to squish the conical part. But logs squish things more than square roots do!
lm_final <- lm(sqrt(mpg) ~ I(horsepower^-1) + weight, data = Auto)
lm_final <- lm(log(mpg) ~ I(horsepower^-1) + weight, data = Auto)
plot(predict(lm_final), residuals(lm_final))
# Log looks the best here!

# But wait, maybe don't go full steam ahead yet. What about outliers? So long as you don't have a big effect on the fit when you remove it.
  # But there will be an impact on the RSE (Overestimate)

summary(Auto[c("horsepower", "weight")])
plot(Auto$horsepower, Auto$weight)
# The plot shows that that one point is definitely weird 
# Residual plot might show it too

# This gives you all that you might want to look at 
par(mfrow = c(2,2))
plot(lm_inv)

# Scale Location is the same as Residuals vs. fitted but studentized instead of raw
# QQ Plot: If I have data points and a probability distribution that I think they came from, how good is the match

# Correlation of your error terms can also be a problem
  # Time series: Residuals start high and then get low (i.e. autocorrelated standard errors issue)

# Collinearity could be a big problem as well 
summary(lm(mpg ~ horsepower, Auto))
summary(lm(mpg ~ weight, Auto))
summary(lm(mpg ~ horsepower + weight, Auto))
# Some of the variation described by horsepower is described by weight. These two variables are correlated with each other. 
pairs(Auto[c("horsepower", "weight")])

