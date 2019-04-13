############## The Analysis ####################
macWide <- read.table('MacroWide.txt', header = T)
names(MacroWide) <- c('Country', 'Country_Code', 'Indicator', 'Indicator_Code', '1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990','1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','Classification')
head(macWide)

setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/Math426ALA")
macLong <- read.table('MacroLong.txt', header = T)
head(macLong)

#### Stock Price Volatility Analysis ####

# Plot and Describe
install.packages('tidyverse')
library(tidyverse)

spv <- macLong[which(macLong$Indicator=='Stock price volatility'),]
head(spv)
group_names <- c('1'='Developed', '0'='Emerging') 

# Imputation (Model)
pred	<- gam(Value ~ Classification + s(Time), data=spv, family=gaussian)
spv$Value <- ifelse(is.na(spv$Value), predict(pred), spv$Value)
sum(is.na(spv$Value))

# Putting the imputation into wide form
spv_wide <- spread(spv,key=Time,value=Value)
describeBy(spv_wide, spv_wide$Classification)

# Using occurrences instead of years
spv$Time <- rep(0:35, length(unique(spv$Country)))

p.spv <- ggplot(data = spv, aes(x = Time, y = Value, group = Country)) 
p.spv + geom_line() + facet_grid(. ~ Classification, labeller = as_labeller(group_names)) 
p.spv + geom_line(alpha=.2) + facet_grid(. ~ Classification, labeller = as_labeller(group_names)) + 
  stat_summary(aes(group = Classification), geom = "point", fun.y = mean, shape = 19, size = 1) + 
  stat_summary(aes(group = Classification), geom='line', fun.y = mean, size = 0.8) 

p2.spv <- ggplot(data = spv, aes(x = Time, y = Value, color = Classification)) 
p2.spv + stat_summary(aes(group = Classification), geom='line', fun.y = mean, size = 1) + 
  stat_summary(aes(group = Classification), geom='point', fun.y = mean, size = 3) 

# Assessing and running spline model
model1 <- gamm(Value ~ Classification + s(Time, by=factor(Classification)), random = list(Country = ~ 1 + Time), data = spv)
par(mfrow=c(1,2))
plot(model1$gam)
summary(model1$gam)
summary(model1$lme)
7.964^2
0.245^2
7.914^2
# Looking at the plot, it would appear that the linear spline model might be useful here. 
# P-values to be viewed with suspicion.
#Is the smoothed effect of time equal to 0 across time. 
# Just talk about the Vizual of the plot

# yij = B0 + B1Classification + S0(time) + S1(time) + bi+ Eij
# s0 is for smooth effect for time for class = 1
# S1 ia for smooth effect for time for class = 0
# First graph is shat 0 of time and second is s hat 1 of time

# Don't worry about the heat map for this!!!!

# Testing everything else (To be sure)

# LME
library(nlme)
model2 <- lme(Value ~ Classification*Time, random = ~ 1 + Time | Country, data = spv)
summary(model2)
7.4713^2
0.2268^2
8.3999^2

# Checking whether we actually need varying intercepts and slopes or whether varying intercepts is enough. 
model3 <- lme(Value ~ Classification*Time, random = ~ 1 | Country, data = spv)

anova(model2, model3)

# LME with AR1
model4 <- lme(Value ~ Classification*Time, random = ~ 1 + Time | Country, data = spv, correlation = corAR1(form = ~1 | Country))
summary(model4)
3.1135868^2
0.0000879^2
9.2881286^2

anova(model2,model4)

# GEE
install.packages('geepack')
library(geepack)

model5 <- geeglm(Value ~ Classification*Time, family = gaussian(link = "identity"), corstr = 'ar1', id = Country, data = spv)
summary(model5)
-0.1116 + .0300

# No heat map code included for gamm 

#### Current Account Balance ####
curaco <- macLong[which(macLong$Indicator=='Current account balance (% of GDP)'),]
group_names <- c('1'='Developed', '0'='Emerging') 

# Imputation (Model)
pred	<- gam(Value ~ Classification + s(Time), data=curaco, family=gaussian)
curaco$Value <- ifelse(is.na(curaco$Value), predict(pred), curaco$Value)
sum(is.na(curaco$Value))

# Assessing nature of data. 
curaco_wide <- spread(curaco,key=Time,value=Value)
describeBy(curaco_wide, curaco_wide$Classification)
summary(curaco$Value)

# Using occurrences instead of years
curaco$Time <- rep(0:35, length(unique(curaco$Country)))

# Assessing whether a spline might be necessary
model6 <- gamm(Value ~ Classification + s(Time), random = list(Country = ~ 1), data = curaco)
plot(model6$gam)

p.curaco <- ggplot(data = curaco, aes(x = Time, y = Value, group = Country)) 
p.curaco + geom_line() + facet_grid(. ~ Classification, labeller = as_labeller(group_names)) 
p.curaco + geom_line(alpha=.2) + facet_grid(. ~ Classification, labeller = as_labeller(group_names)) + 
  stat_summary(aes(group = Classification), geom = "point", fun.y = mean, shape = 19, size = 1) + 
  stat_summary(aes(group = Classification), geom='line', fun.y = mean, size = 0.8) 

p2.curaco <- ggplot(data = curaco, aes(x = Time, y = Value, color = Classification)) 
p2.curaco + stat_summary(aes(group = Classification), geom='line', fun.y = mean, size = 1) + 
  stat_summary(aes(group = Classification), geom='point', fun.y = mean, size = 3) 

# LME
library(nlme)
model7 <- lme(Value ~ Classification*Time, random = ~ 1 + Time | Country, data = curaco)
summary(model7)
1.865^2
0.208^2
3.177^2

# Checking whether we actually need varying intercepts and slopes or whether varying intercepts is enough. 
model8 <- lme(Value ~ Classification*Time, random = ~ 1 | Country, data = curaco)

anova(model7, model8)


# LME with AR1

ctrl <- lmeControl(opt='optim')
model9 <- lme(Value ~ Classification*Time, random = ~ 1 + Time | Country, data = curaco, correlation = corAR1(form = ~1 | Country), control=ctrl)
# When I previously ran this, I could not do it because:
# nlminb problem, convergence error code = 1
# message = iteration limit reached without convergence (10)
# When I added the ctrl with the opt='optim,' it worked. 
# Optim is a General-purpose optimization based on Nelder-Mead, quasi-Newton and conjugate-gradient algorithms - Help from here https://stats.stackexchange.com/questions/40647/lme-error-iteration-limit-reached
summary(model9)
1.560583^2
0.000703^2
4.558059^2

anova(model7,model9)

# GEE
install.packages('geepack')
library(geepack)

model10 <- geeglm(Value ~ Classification*Time, family = gaussian(link = "identity"), corstr = 'ar1', id = Country, data = curaco)
summary(model10)

model11 <- geeglm(Value ~ Classification+Time, family = gaussian(link = "identity"), corstr = 'ar1', id = Country, data = curaco)
summary(model11)

anova(model10, model11)

rho <- 0.903
cov <- diag(35) 
cov <- rho^abs(row(cov)-col(cov)) 
library(reshape2)
meltcov <- melt(cov)
heatmap <- ggplot(data = meltcov, aes(x=Var1, y=ordered(Var2, levels = rev(sort(unique(Var2)))), fill=value)) + geom_tile()
heatmap + labs(x="Time",y="Time")

# Can do the GAM function to see if there is a better parametric model?s
