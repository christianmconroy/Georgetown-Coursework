# Math 426 Problem Set 3 #
# Christian Conroy #

compgrip <- read.table("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/Math426ALA/compgrip.txt")
attach(compgrip)

# Question 1#
names(compgrip) <- c('ID', 'treatment', 't0', 't1', 't2', 't3')

install.packages('psych')
library(psych)
compgrip$treatment <- as.numeric(compgrip$treatment == 2)
describeBy(compgrip, compgrip$treatment)


compgrip_1 <- compgrip[which(compgrip$treatment == 0),3:6]
compgrip_2 <- compgrip[which(compgrip$treatment == 1),3:6]
plot(c(0,1,2,3), apply(compgrip_1, 2, mean),  ylim = c(130,160), xlab = 'Time (weeks)', ylab = 'Mean Grip Strength', type = 'n')
abline(v = axTicks(1), h = axTicks(2), col = rgb(0.75, 0.75, 0.75, alpha = 0.5), lty = 3)
lines(c(0,1,2,3), apply(compgrip_1, 2, mean), type = 'b', pch = 1, lty = 2)
lines(c(0,1,2,3), apply(compgrip_2, 2, mean), type = 'b', pch = 16)
legend(x = 4, y = 13, legend = c('Treatment 1', 'Treatment 2'), lty =
         c(2,1), pch = c(1,16))


# Question 3#
install.packages("nlme")
library(nlme)

compgriplong <- reshape(compgrip, varying = list(3:6), idvar = 'ID',
                   v.names = 'level', direction = 'long')
head(compgriplong)

compgriplong <- within(compgriplong, {
  
  time <- factor(time)
  treatment <- factor(treatment, 0:1, c("A","B"))
})

compgriplong <- compgriplong[order(compgriplong$ID),]
head(compgriplong)

compgriplong$occur <- rep(1:4, length(unique(compgriplong$ID)))

model1 <- gls(level ~ treatment*factor(time), data =
                compgriplong, correlation = corSymm(form = ~ 1 | ID),
              weights = varIdent(form = ~ 1 | occur))

summary(model1)

# Question 5 #
MeanTimes <- expand.grid(time = c("1","2","3","4"),
                      treatment = c("A","B"))
MeanTimes$predicted <- predict(model1, newdata=MeanTimes, type="response")
print(MeanTimes, digits=4, row.names = F)

# Question 6 #
compgriplong$time <- as.numeric(compgriplong$time)
compgriplong$treatment <- as.numeric(compgriplong$treatment)
compgriplong$treatment <- as.numeric(compgriplong$treatment == 2)

compgriplong$week1 <- 0
compgriplong$week1[which(compgriplong$time == 2)] <- 1
compgriplong$week2 <- 0
compgriplong$week2[which(compgriplong$time == 3)] <- 1
compgriplong$week3 <- 0
compgriplong$week3[which(compgriplong$time == 4)] <- 1
compgriplong$week1t <-  compgriplong$week1* compgriplong$treatment
compgriplong$week2t <-  compgriplong$week2* compgriplong$treatment
compgriplong$week3t <-  compgriplong$week3* compgriplong$treatment

compgrip$D1 <- compgrip$t1 - compgrip$t0
compgrip$D2 <- compgrip$t2 - compgrip$t0
compgrip$D3 <- compgrip$t3 - compgrip$t0

library(tidyr)

compgripDiff <- gather(compgrip[,-c(3:6)], key = diff, value = level, D1:D3)
compgripDiff <- compgripDiff[order(compgripDiff$ID),]
rownames(compgripDiff) <- NULL
compgripDiff$occur <- rep(1:3, length(unique(compgripDiff$ID)))
head(compgripDiff)

compgripDiff <- within(compgripDiff, {
  
  diff <- factor(diff)
  treatment <- factor(treatment, 0:1, c("A","B"))
})

model2 <- gls(level ~ treatment*diff, data = compgripDiff, correlation = corSymm(form = ~ 1 | ID), weights = varIdent(form = ~ 1 | occur))
summary(model2)


MeanTimes2 <- expand.grid(diff = c("D1","D2","D3"),
                         treatment = c("A","B"))
MeanTimes2$predicted <- predict(model2, newdata=MeanTimes2, type="response")
print(MeanTimes2, digits=4, row.names = F)
head(compgrip_1)

# Question 7 #
auc1 <- (((0+1-(2*3))*compgrip_1[,1]) + ((2-0)*compgrip_1[,2]) + ((3-2)*compgrip_1[,3]))/2
auc2 <- (((0+1-(2*3))*compgrip_2[,1]) + ((2-0)*compgrip_2[,2]) + ((3-2)*compgrip_2[,3]))/2
t.test(auc2, auc1)
