Reign <- read.table("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/Math426ALA/coup1980.txt", header=T, sep="")
attach(Reign)

# Questions 1 and 2 part 1
layout(1:3)
plot(democracy, logCoup)
abline(lm(logCoup~democracy))
plot(age, logCoup)
abline(lm(logCoup~age))
plot(tenure,logCoup)
abline(lm(logCoup~tenure))

# Question 2 part 2
# For printing output purposes 
install.packages("stargazer")
library(stargazer)

RegDem<- lm(logCoup~democracy)
summary(RegDem)
RegAge<- lm(logCoup~age)
summary(RegAge)
RegTen<- lm(logCoup~tenure)
summary(RegTen)
stargazer(RegDem, RegAge, RegTen, type="text",title="Log(CoupRisk) Against Each Democracy, Age, Tenure", digits=4,style="all", out="LCDem.txt")

# Question 3 

MVReg <- lm(logCoup~democracy+age+tenure)
summary(MVReg)
confint(MVReg,level=0.95)
stargazer(MVReg, type="text",title="Log(CoupRisk) Against Democracy, Age, Tenure", digits=4,style="all", ci=TRUE, ci.level=0.95, out="MVReg.txt")

# Question 5a
Reign$TenDemInt <- tenure*democracy
MVReg3 <- lm(logCoup~democracy+age+tenure+Reign$TenDemInt)
summary(MVReg3)

# Question 5b
Reign$MeanTenure <- mean(tenure)
Reign$Demeaned <- Reign$tenure- Reign$MeanTenure
Reign$MeanCTen <- Reign$Demeaned*democracy
MVReg4 <- lm(logCoup~democracy+age+Reign$Demeaned+Reign$MeanCTen)
summary(MVReg4)
stargazer(MVReg4, type="text",title="Log(CoupRisk) Against Democracy, Age, Centered Tenure, and Interacted Democracy and Centered Tenure", digits=4,style="all", ci=TRUE, ci.level=0.95, out="MVReg4.txt")




