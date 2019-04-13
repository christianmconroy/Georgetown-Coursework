dental <- read.table("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/Math426ALA/dental.txt")
attach(dental)
head(dental)

# Remember to subset out for all females only on all of this !# 
# Question 1, Part A # 
dental_F <-subset(dental, V2=="F")
head(dental_F)
matplot(c(8,10,12,14), t(dental_F[,c(3:6)]), type='b', pch=20, lty=1, main= "Distance by Age", col='gray', xlab =
          "Age", ylab = "Distance from pit gland")

# Question1, Part B # 
summary(dental_F[,c(3:6)])
plot(c(8,10,12,14), apply(dental_F[,c(3:6)], 2, mean), xlab="Time", ylab="Means")
lines(c(8,10,12,14), apply(dental_F[,c(3:6)], 2, mean), xlab="Time", ylab="Means")
apply(dental_F[,c(3:6)],2,sd)

# Not required, but the SD plot is good for visualizing how the SD changes over time.
plot(c(8,10,12,14), apply(dental_F[,c(3:6)], 2, sd), xlab="Time", ylab="SD")
lines(c(8,10,12,14), apply(dental_F[,c(3:6)], 2, sd), xlab="Time", ylab="SD")


# Question 1, Part c#
# Raw estimate is taking the covariance or corrleation of those variables.
cov(dental_F[,c(3:6)])
cor(dental_F[,c(3:6)])

# Question 2, Part A #
# Fit a repeated measures model in which there is separate parameter for the mean distance at each of the four ages.
# Use an unstructured variance-covariance structure. For estimating parameters, fit the model using (i) the
# method = 'ML' argument and (ii) the method = 'REML' (default) argument.
install.packages("nlme")
library(nlme)
dental1 <- reshape(dental_F, varying = list(3:6), times = c(8, 10, 12, 14), idvar = 'ID',
                   v.names = 'level', direction = 'long')
head(dental1)
dental1 <- dental1[order(dental1$ID),]
dental1$occur <- rep(1:4, length(unique(dental1$ID)))
modelML<- gls(level ~ factor(time), data = dental1, correlation = corSymm(form = ~ occur | ID), weights =
                varIdent(form = ~ 1 | occur), method = 'ML')
modelREML<- gls(level ~ factor(time), data = dental1, correlation = corSymm(form = ~ occur | ID), weights =
                  varIdent(form = ~ 1 | occur), method = 'REML')
summary(modelML)
summary(modelREML)
getVarCov(modelML)
getVarCov(modelREML)
# Question 2 Part A Section iii #
21.181818 + 1.045455
21.181818 + 1.909091
21.181818 + 2.909091
# Question 2 Part B #
model2 <- lm(level ~ factor(time), data = dental1)
summary(model2)
# Question 2 Part B Section ii #
anova(modelML, model2)




