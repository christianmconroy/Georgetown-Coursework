# Math 426 Problem Set 5 #
# Christian Conroy #

dental <- read.table("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/Math426ALA/dental2.txt")
attach(dental)

# Question 1, Part A #

names(dental) <- c('ID', 'Gender', '8', '10', '12', '14')
dental$Gender <- as.numeric(dental$Gender)
head(dental)
library(psych)
describeBy(dental, dental$Gender)
# Question 1, Part B #

install.packages('tidyverse')
library(tidyverse)
dental_female <- dental[which(dental$Gender == 1),3:6]
dental_male <- dental[which(dental$Gender == 2),3:6]

matplot(c(8,10,12,14), t(dental_female ), type='b', pch=20, lty=1, col='gray', xlab = 'Time (hours)', ylab = 'Distance from Center', main = 'Distance from Center response per subject - female')

matplot(c(8,10,12,14), t(dental_male), type='b', pch=20, lty=1, col='gray', xlab = 'Time (hours)', ylab = 'Distance from Center', main = 'Distance from Center response per subject - male')

# Question 1, Part C #
plot(c(8,10,12,14), apply(dental_female, 2, mean),  ylim = c(21,28),  xlab = 'Age)', ylab = 'Distance from Center', main = 'Mean Distance from Center per age', type = 'n')
abline(v = axTicks(1), h = axTicks(2), col = rgb(0.75, 0.75, 0.75, alpha = 0.5), lty = 3)
lines(c(8,10,12,14), apply(dental_female, 2, mean), type = 'b', pch = 1, lty = 2)
lines(c(8,10,12,14), apply(dental_male, 2, mean), type = 'b', pch = 16)
legend(x = 0, y = 39, legend = c('Female', 'Male'), lty =
         c(2,1), pch = c(1,16))

# Question 2, Part A, Part i#
dental_long <- reshape(dental, varying = list(3:6), times = c(8, 10, 12, 14), idvar = 'ID', v.names = 'level', direction = 'long')
head(dental_long)
attach(dental_long)
stage1 <- lmList(level ~ time | ID, data = dental_long)
stage1

#Question 2, Part A, Part ii #
stage1g <- lmList(level ~ time | Gender, data = dental_long)
stage1g

# Question 2, Part B, Part i-iii#
# Convert to wide here. Extract off and then convert to wide. 
beta1 <- coef(stage1)[,1]
beta2 <- coef(stage1)[,2]
dental_wide <- spread(dental_long, key = time, value = level)
stage21 <- lm(beta1 ~ dental_wide$Gender)
stage22 <- lm(beta2 ~ dental_wide$Gender)
summary(stage21)
summary(stage22)

# Question 3, Parts A-B #
library(nlme)
model1 <- lme(level ~ Gender*time, random = ~ 1 + time | ID, data = dental_long)
summary(model1)
# Within-subject is the residual for the random component and between subject variance is the intercept component. Subject-specific intercepts and slopes as random effects. We are doing the variance so we'll need to square.
1.3100396^2
2.4055009^2

# Question 3, Part C - The diff in mean intercept and mean slope are the coefficients under fixed effects. Difference in intercepts is beta 1 and the difference in the slopes is beta 3.  This is the standard error for the differences, which just means the standard error for beta hat 1 and the standard error for beta hat 3. 

# Question 3, Part E - This is the correlation of the random intercept and random slope. 

# Question 3, Part F #
summary(stage21)
summary(stage22)


# Question 3, Part G #
library(nlme)
model2 <- lme(level ~ factor(Gender)*time, random = ~ 1 + time | ID,  data = dental_long)
G <- getVarCov(model2)
s2 <- model2$sigma^2
Z <- matrix(c(1, 1, 1, 1, 8, 10, 12, 14), ncol = 2)
covy <- Z%*%G%*%t(Z) + s2*diag(4)
cov2cor(covy)

# Getvarcov gives you the G matrix: The G matrix summarizes the covariance of the radom slope and intercept. The diagonal of the G matrix gives you between subject. 
# model$Sigma^2 gives you within subject variability or the model errors. 
# Diagonal is between subject variability for baseline and then the second part of the diagonal is the variance for the random slope
# Off diagonal is the covariance between the two, which is only interesting in terms of eventually constructing covariance of the yijs. 




# Z is the design matrix for the fixed effects. 

# Question 3, Part H #
dental_long$occur <- rep(1:4, length(unique(dental_long$ID)))

head(dental_long)

dental_long <- dental_long[order(dental_long$ID),]
model3 <- lme(level ~ Gender*time, random = ~ 1 + time | ID, data = dental_long, correlation = corSymm(form = ~ occur | ID), weights = varIdent(form = ~ 1 | occur))
anova(model3, model1)
