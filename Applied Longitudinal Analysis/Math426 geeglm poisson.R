# Math 426 ALA HW 7 #

# Question 1
skin <- read.table('skin.txt', header = F)
names(skin) <- c('ID', 'Center', 'Age', 'Skin', 'Gender', 'Exposure', 'Y', 'Treatment', 'Year')

skin_wide <- spread(skin,key=Year,value=Y)
describeBy(skin_wide[8:12], skin_wide$Treatment)

# Question 1, Part B
modelR1 <- geeglm(Y ~ Treatment*Year - Treatment, family = poisson(link = 'log'), id = ID, scale.fix = FALSE, data = skin)
summary(modelR1)

install.packages('xtable')
library(xtable)
confint(modelR1, parm, level = 0.95)

modelR1_e <- xtable(modelR1)
print.xtable(modelR1_e, type="html", file="Pset7.html")

exp(0.0485)
exp(-0.0212)

# Question 2, Part A and B#
modelR2 <- geeglm(Y ~ Year + Treatment + Center + Age + Skin + Gender + Exposure, family = poisson(link = 'log'), id = ID, scale.fix = FALSE, data = skin)
summary(modelR2)

modelR2_e <- xtable(modelR2)
print.xtable(modelR2_e, type="html", file="Pset7.html")

# Question 2, Part C #
exp(0.59458)
exp(0.15041)
exp(0.13715)
exp(0.01643)

# Question 2, Part D #
modelR3 <- geeglm(Y ~ Year + Treatment + Center + Age + Skin + Gender + Exposure, family = poisson(link = 'log'), id = ID, scale.fix = TRUE, data = skin)
summary(modelR3)
