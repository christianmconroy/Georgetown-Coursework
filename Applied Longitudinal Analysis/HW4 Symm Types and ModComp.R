# Math 426 Problem Set 4 #
# Christian Conroy #

ozone <- read.table("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/Math426ALA/ozone0246.txt")
attach(ozone)

# Question 1, Part A #
names(ozone) <- c('ID', 'time', 'treatment', 'fev1')
ozone_wide <- spread(ozone, key=time, value=fev1)
head(ozone_wide)
library(psych)
describeBy(ozone_wide, ozone_wide$treatment)
# Question 1, Part B #

install.packages('tidyverse')
library(tidyverse)
ozone_room <- ozone_wide[which(ozone_wide$treatment == 0),3:6]
ozone_ozone <- ozone_wide[which(ozone_wide$treatment == 1),3:6]

matplot(c(0,2,4,6), t(ozone_room), type='b', pch=20, lty=1, col='gray', xlab = 'Time (hours)', ylab = 'Fev1', main = 'Fev1 response per subject - room')

matplot(c(0,2,4,6), t(ozone_ozone), type='b', pch=20, lty=1, col='gray', xlab = 'Time (hours)', ylab = 'Fev1', main = 'Fev1 response per subject - ozone')

# Question 1, Part C #
plot(c(0,2,4,6), apply(ozone_room, 2, mean),  ylim = c(35,45),  xlab = 'Time (hours)', ylab = 'Fev1', main = 'Mean Fev1 per hour', type = 'n')
abline(v = axTicks(1), h = axTicks(2), col = rgb(0.75, 0.75, 0.75, alpha = 0.5), lty = 3)
lines(c(0,2,4,6), apply(ozone_room, 2, mean), type = 'b', pch = 1, lty = 2)
lines(c(0,2,4,6), apply(ozone_ozone, 2, mean), type = 'b', pch = 16)
legend(x = 0, y = 39, legend = c('Treatment 1', 'Treatment 2'), lty =
         c(2,1), pch = c(1,16))

# Question 2, Part A # 
install.packages('nlme')
library(nlme)

ozone$occur <- rep(1:4, length(unique(ozone$ID)))

head(ozone)

ozone <- ozone[order(ozone$ID),]

model1 <- gls(fev1 ~ factor(treatment) * factor(time), data = ozone, correlation = corSymm(form = ~ occur | ID), weights = varIdent(form = ~ 1 | occur))
summary(model1)


getVarCov(model1)
cov2cor(getVarCov(model1)) 

# question 2, Part B #
# Compound Symmetry 
model2 <-  gls(fev1 ~ factor(treatment) * factor(time), data = ozone, correlation = corCompSymm(form = ~ 1 | ID))
summary(model2)

getVarCov(model2)
cov2cor(getVarCov(model2))

# Heterogeneous compound symmetry
model3 <-  gls(fev1 ~ factor(treatment) * factor(time), data = ozone, correlation = corCompSymm(form = ~ 1 | ID), weights = varIdent(form = ~ 1 | occur))
summary(model3)

getVarCov(model3)
cov2cor(getVarCov(model3))

# LRT - Compound symmetry vs. Heterogeneous symmetry 
anova(model2, model3)

# First Order Autoregressive 
model4 <- gls(fev1 ~ factor(treatment) * factor(time), data = ozone, correlation = corAR1(form = ~ 1 | ID))
summary(model4)

getVarCov(model4)
cov2cor(getVarCov(model4))

# Unstructured vs First Order Autoregressive
anova (model1, model4)

# Heterogeneous First Order Autoreggressive 
model5 <- gls(fev1 ~ factor(treatment) * factor(time), data = ozone, correlation = corAR1(form = ~ 1 | ID),  weights = varIdent(form = ~ 1 | occur))
summary(model5)

getVarCov(model5)
cov2cor(getVarCov(model5))

# LRT - First order autoregressive vs heterogeneous first order autoregressive 
anova(model5, model4)


# AIC - Compound Symmetry vs First order autoregressive 
anova(model2, model4)


# Question 3, Part a #
model6 <- gls(fev1 ~ factor(treatment)*factor(time), data =
                ozone, correlation = corAR1(form = ~ 1 | ID),
              weights = varIdent(form = ~ 1 | occur))
anova(model6)


# Question 3, Part B #
model7 <- gls(fev1 ~ treatment*factor(time), data =
                ozone, correlation = corAR1(form = ~ 1 | ID),
              weights = varIdent(form = ~ 1 | occur), method = 'ML')

model8 <- gls(fev1 ~ treatment+factor(time), data =
                ozone, correlation = corAR1(form = ~ 1 | ID),
              weights = varIdent(form = ~ 1 | occur), method = 'ML')

anova(model8, model7)
# Question 4, Part A #
model9 <- gls(fev1 ~ treatment*factor(time), data =
                ozone, correlation = corAR1(form = ~ 1 | ID))

model10 <- gls(fev1 ~ treatment*factor(time), data =
                ozone, correlation = corAR1(form = ~ 1 | ID),
              weights = varIdent(form = ~ 1 | occur))

anova(model9, model10)
# Question 4, Part B #
model11 <- gls(fev1 ~ treatment*factor(time), data =
                ozone, correlation = corAR1(form = ~ 1 | ID),
              weights = varIdent(form = ~ 1 | occur))
summary(model11)

# Question 4, Part A and B#
model12 <- gls(fev1 ~ factor(treatment)*time, data =
                ozone, correlation = corAR1(form = ~ 1 | ID),
              weights = varIdent(form = ~ 1 | occur))
summary(model12)
coef(model12)

# Question 4, Part C #
anova(model12)

# Question 4, Part D #
confint(model12)
1.4093168 - 0.1800189

# Question 4, Part E
model13 <- gls(fev1 ~ treatment*factor(time), data =
                 ozone, correlation = corAR1(form = ~ 1 | ID),
               weights = varIdent(form = ~ 1 | occur), method = "ML")

model14 <- gls(fev1 ~ factor(treatment)*time, data =
                 ozone, correlation = corAR1(form = ~ 1 | ID),
               weights = varIdent(form = ~ 1 | occur), method = "ML")

anova(model13, model14)
