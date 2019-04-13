# Math426 ALA HW 6 #
# Question 1#
install.packages('SemiPar')
library(SemiPar)
data(pig.weights)
attach(pig.weights)

# Individual Trajectories 
install.packages('tidyverse')
library(tidyverse)
pig.weights_wide <- spread(pig.weights, key=num.weeks, value=weight)
pig.weights_wideplot <- pig.weights_wide[,2:10]


matplot(c(1,2,3,4,5,6,7,8,9), t(pig.weights_wideplot), type='b', pch=20, lty=1, col='gray', xlab = 'Time (Weeks)', ylab = 'Weight', main = 'Weight response per subject')

# Mean Weight Over Time
plot(c(1,2,3,4,5,6,7,8,9), apply(pig.weights_wideplot, 2, mean),  ylim = c(20,80),  xlab = 'Time (Weeks)', ylab = 'Weight', main = 'Mean Weight per Week', type = 'n')
abline(v = axTicks(1), h = axTicks(2), col = rgb(0.75, 0.75, 0.75, alpha = 0.5), lty = 3)
lines(c(1,2,3,4,5,6,7,8,9), apply(pig.weights_wideplot, 2, mean), type = 'b', pch = 1, lty = 2)

# Question 2, Part a #
install.packages('nlme')
library(nlme)
model1 <- lme(weight ~ num.weeks, random = ~ 1
             + num.weeks | id.num, data = pig.weights)
summary(model1)
intervals(model1, which = "fixed")

# Question 2, Part b #
2.643^2
0.616^2
1.264^2

# Question 2, Part c #
6.21+(c(-1,1)*1.645)*(0.616^2)

# Question 3, Part a #
install.packages('longpower')
library(longpower)

lmmpower(delta = 0.2,  t = seq(1, 9, 1), sig2.i =
           6.99, sig2.s = 0.379, sig2.e = 1.6, cov.s.i =
           -.063*sqrt(6.99)*sqrt(0.379), power = 0.90, sig.level = .05)

# Question 3, part b #
lmmpower(n = 48, t = seq(1, 9, 1), sig2.i =
           6.99, sig2.s = 0.379, sig2.e = 1.6, cov.s.i =
           -.063*sqrt(6.99)*sqrt(0.379), power = 0.80, sig.level = .05)

# GLM Review Question 1 #
access <- read.table('access.txt', header = T)
attach(access)
head(access)

modelR1 <- glm(wheeze ~ cort5 + bmi, family = binomial(link = 'logit'), data = access)
summary(modelR1)
exp(confint(modelR1))

# modelR1a <- glm(wheeze ~ cort5 + bmi, family = binomial(link = 'probit'), data = access)
# summary(modelR1a)
# confint(modelR1a)

# GLM Review Question #2 #
ihd <- read.table('ihd.txt', header = T)
attach(ihd)
head(ihd)

modelR2 <- glm(interventions ~ sex + comorbidity, family = poisson(link = 'log'), data = ihd)
summary(modelR2)
confint(modelR2)
exp(confint(modelR2))

# Pretty sure this is right, but double check. 
