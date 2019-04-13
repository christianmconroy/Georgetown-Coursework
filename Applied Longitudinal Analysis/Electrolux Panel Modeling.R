# Panel data model in R to infer Market Size from Macroeconomic Indicators #

# 1. Building the model with total Market Size in each country #

# Installing and running the functions needed to run the fixed effects analysis and product coefficients/slope
# output
install.packages("pbkrtest")
install.packages("plm",dependencies = TRUE)
library(plm)

#Importing the CSV data (Self-selected variables)

mydata<- read.csv("C:\\Users\\ConroChr\\Desktop\\FullMacroeconomicData_062917_TotalMarketData_MarkTOT.csv", sep=";")
attach(mydata)

# Identifying the data as panel data in order to run the analysis 
pdata <- pdata.frame(mydata,index=c("Country","Year"), drop.index=TRUE, row.names=TRUE)
head(pdata)

# To show what the data looks like before jumping into any advanced analysis
# Includes previous iterations. Need to return and include only variables 
# Included in final model. 
library(ggplot2)

ggplot(mydata, aes(x = Year, y = MarketTotal, colour=Country)) +
  geom_line() +
  scale_colour_discrete(name = "Countries") +
  xlab("Year") +
  ylab("Market Total") +
  ggtitle("Market Total by Country, 2008-2016") + ylim(0,1070000000)

# In order to change Country index from character string to numeric for indexing purposes in Fixed Effects
# This is not necessarily needed. 
mydata <- data.frame(lapply(mydata,function(x) as.numeric(as.character(x))))
warnings()

# Running the analysis 
# Binding the X and Y. X Values subject to change upon further iteration and testing. 

Y <- cbind(MarketTotal)
X <- cbind (X..of.HHs.earning...US.10.000.p.a., Consumerpricesendperiod, Medianhouseholdincome,
            Personaldisposincomeperhead, Avenoperhousehold, GDP.per.head..US.., Households, Mean.years.of.schooling,
            Total.housing.stock..dwellings.per.1.000.pop., Privateconsumptionperhead, PerPop2039, Urbanpopulationper,
            Energy.intensity..Total.energy.consumption.per.capita, Lendinginterestrate, Unemploymentrate,
            Real.effective.exchange.rate, Population)

# The above just seems like the best we're going to get at this point. R-squared and adjusted R-squared still
# too low, but p-value is good sign. 

# Based on theoretical research of relationships, we should include %10,000 or more, Consumer prices,
# median household income, personal disposable, avg no per HH, GDP per head, HH, Stock Dwellings, Private Consumption,
# Urban pop, Energy, Lending interest rate, Exchange rate, Pop, Education (New), unemployment, housing price (?)

# Summary statistics on our independent and dependent variables
summary(Y)
summary(X)

# What it would look like if we just did a simple multivariate regression

MVOLS <-lm(Y~X, data=pdata)
summary(MVOLS)
residuals(MVOLS)

# R^2 still obviously very low at .08519 but at least we do not have an adjusted R^2 that is negative here.
# Residuals still too high. 

#Pooled OLS estimator. I include this just so that we can run the LM Test later to show that fixed effects is necessary
# in the first place. Obviously the countries are not all the same and we therefore can't run pooled OLS. Instead
# we need to account for the fact that there will be different intercepts. 
pooling <- plm(Y ~ X, data=pdata, model="pooling")
summary(pooling)
resid(pooling)

# Essentially same results as with mutlivariate regression. 

# Fixed effects or within estimator

fixed <- plm(Y ~ X, data=pdata, model = "within")
summary(fixed)
predict(fixed)
resid(fixed)

## Statistical significance on %earnings>, GDP per head, Private consumption, urban population %, 
## energy intensity, and population. Residuals still too large. 

# Most residuals between -804795300000 and 2092737000000 and many predictions negative (BAD)
# Predict: Argentina (7) 5.907425e+11 | (6) 1.072932e+12 | (57) 4.631613e+11

## Two way fixed effects
# Running to compare later with the random effects option. 

fixed2way <- plm(Y ~ X, data=pdata, model = "within", effect="twoways")
summary(fixed2way)

## Significance for Earnings> US 10,000, GDP per head, Private Consumption per Head, %Urban Pop, Energy intensity,
## and population. 

summary(fixef(fixed2way))
summary(fixef(fixed2way, effect="time"))

## Very few of the countries have significant fixed effects, meaning the model can be improved. 
## None of time fixed effects. 

## Below is how to get all 362 predicted values (Every country every year)

predict(fixed2way)
resid(fixed2way)

fixed2waypred <- predict(fixed2way)
fixed2way.resid <- resid(fixed2way)
summary(fixed2way.resid)

# Some are coming out negative... There are gigantic residuals too so the model is not great.  

# https://stats.stackexchange.com/questions/147836/prediction-interval-for-lmer-mixed-effects-model-in-r
# The above link might be helpful in terms of setting up the vector containing the new information

# Random effects estimator. Just to compare. 
random <- plm(Y ~ X, data=pdata, model="random")
summary(random)
residuals(random)

# NAs produced is a problem. 

# LM Test for fixed effects versus OLS (Would use plmtest(pooling,type=c("bp")) if we wanted to test random vs OLS)
pFtest(fixed,pooling)

# If we have a small p, it means that we have significant effects and are
# therefore right to do the fixed effects model vs pooled OLS 
# Results show this condition has been satisfied. FE seems better. 

# Hausman test for fixed versus random effects model 
phtest(random,fixed)

# If we have a small p, it means that we have significant effects and are
# therefore right to do the fixed effects model vs random effects model
# The output saying that one model is inconsistent means that the fixed effects model is inconsistent. 
# Results show a low p-value and an alternative hypothesis that says one model is inconsistent,
# meaning that FE might be better if we use effects at all. 

# To test whether one way or two way test is appropriate. 
plmtest(pooling, effect="twoways", type = "ghm")
# Results (p-value that's 1 and alternative hypothesis with significant effects)
# show that one-way would be what we should go with. 

# Test for serial correlation for one-way fixed effects
pcdtest(fixed, test=c("cd"))
# The p-value originally comes out very low herefor the one-way model, meaning that we also have 
# serial correlation. Not sure what to do about it as it's not running now. 

# Test for serial correlation for two-way fixed effects
pcdtest(fixed2way, test=c("cd"))
#The p-value comes out even lower here  meaning that we have serial correlation regardless of whether
# we do one or twoway. Does this mean we need a dynamic model? Not sure what to do about it as it's not running now. 

## Stop Reading Here. 
## Below is further experimentation and running by product line as well. 


## _________________________________________________________________________________

install.packages('sjPlot')
install.packages('sjmisc')
library(sjPlot)
library(sjmisc)

mydata$Country <- as.factor(mydata$Country)
head(mydata)
p <- ggplot() +
geom_point(data=mydata,aes(y=MarketTotal,x=Consumerpricesperpaavend),shape=Country,color=Country,size=Country)+
  geom_smooth(data=mydata, method=lm,se=FALSE,size=.3,aes(y=MarketTotal,x=Consumerpricesperpaavend)) 
+ scale_shape_manual(data=mydata,aes(y=MarketTotal,x=Consumerpricesperpaavend), values=1:nlevels(mydata$Country))+
  geom_point(data=mydata,aes(y=MarketTotal,x=Consumerpricesperpaavend))+
  geom_smooth(data=mydata, method=lm,se=FALSE,size=.3,aes(y=MarketTotal,x=Consumerpricesperpaavend), colour="black") 
+ coord_cartesian(xlim=c(0,10)) 

ggplot(mydata,aes(y=MarketTotal,x=Consumerpricesperpaavend)) + geom_point(aes(color=Country, shape=Country), size=.3, alpha = .8)
+ geom_smooth(method="lm",se=F,size=.3,aes(linetype=Country, group=Country)) + geom_smooth(method="lm",size=.3,colour="black",se=F)
+ theme_bw() + coord_cartesian(xlim=c(0,10)) 


# ------------------------------------------------------------------------------------------------------------------#
# Forecasting for each region (https://stackoverflow.com/questions/24910450/for-loop-for-forecasting-several-datasets-at-once-in-r)
# Not actually useful. 
install.packages("data.table")
install.packages("forecast")
library(data.table)
library(forecast)
mydata <- setDT(mydata)[,list(AR = list(auto.arima(MarketTotal))), by = Country]
mydata$AR

mydata[,sapply(AR,function(x) plot(forecast(x,10)))]

# OR 

library(dplyr)
library(forecast)
model_fits <- group_by(mydata,Country) %>% do(fit=auto.arima(.$MarketTotal))

model_fits$fit


# DISREGARD ALL OF THE BELOW!

# ------------------------------------------------------------------------------------------------------------------#
# 2. Building the model with Market Size by Product Group in each country #

proddata <- read.csv("C:\\Users\\ConroChr\\Desktop\\FullMacroeconomicData_Adjusted062017_NEW_ProdGroups.csv", sep=";")
attach(proddata)

# In order to change Country index from character string to numeric for indexing purposes in Fixed Effects
proddata$Country <- as.numeric(factor(proddata$Country, levels=unique(proddata$Country)))

# To turn factors into numeric for analysis and log conversion
proddata <- data.frame(lapply(proddata,function(x) as.numeric(as.character(x))))
warnings()

# Log for those variables in our model that are in percentage form
class(proddata$Averag.wagesmonthly)
class(proddata$Unemploymentrate)

log(proddata$Consumerpricesperpaavend)
log(proddata$Unemploymentrate)

# Identifying the data as panel data in order to run the analysis 
pdata2 <- pdata.frame(proddata,index=c("Country","Year"), drop.index=TRUE, row.names=TRUE)
head(pdata2)

names(pdata2)

# Checking on which variables have missing values in our unbalanced panel data set
sum(is.na(pdata2$Averag.wagesmonthly))
sum(is.na(pdata2$RealGDP))
sum(is.na(pdata2$Medianhouseholdincome))
sum(is.na(pdata2$Personaldisposableincome))
sum(is.na(pdata2$Unemploymentrate))
sum(is.na(pdata2$Population))
sum(is.na(pdata2$Newdwellingscompleted))
sum(is.na(pdata2$Retailsales))
sum(is.na(pdata2$Consumerpricesperpaavend))
sum(is.na(pdata2$ElectricalapplianceshousewareMDnomUSD))
sum(is.na(pdata2$Averagenoperhousehold))
sum(is.na(pdata2$Fabric.Care))
sum(is.na(pdata2$Home.Comfort))
sum(is.na(pdata2$Dish.Care))
sum(is.na(pdata2$Floor.Care))
sum(is.na(pdata2$Food.Preservation))
sum(is.na(pdata2$Food.Preparation))

# Not worth doing Home Care. Not Enough- Food Preservation and Food Preparation have most data. Dish and Floor
# not great but can still do. 

############ Food Preparation ################

# Binding the X and Y. X Values subject to change upon further iteration and testing. 
Y1 <- cbind(Food.Preparation)
X <- cbind (Averagenoperhousehold, Averag.wagesmonthly, ElectricalapplianceshousewareMDnomUSD, Consumerpricesperpaavend, Medianhouseholdincome,
            Personaldisposableincome, Importdutiespertotal, Population, RealGDP, 
            ConsumerexpenditureHousehol.goodsservices..US.., Retailsales, Unemploymentrate)

# Second best model: Averagenoperhousehold, Averag.wagesmonthly, Currentaccountbalance, Consumerpricesperpaavend, Medianhouseholdincome,
#Personaldisposableincome, Newdwellingscompleted, Importdutiespertotal, Population, RealGDP, 
# ConsumerexpenditureHousehol.goodsservices..US.., Retailsalesnonfood, Unemploymentrate)


# Summary statistics on our independent and dependent variables
summary(Y1)
summary(X)

#Pooled OLS estimator. I include this just so that we can run the LM Test later to show that fixed effects is necessary
# in the first place. Obviously the countries are not all the same and we therefore can't run pooled OLS. Instead
# we need to account for the fact that there will be different intercepts. 
pooling1 <- plm(Y1 ~ X1, data=pdata2, model="pooling")
summary(pooling1)

# Very statistically significant for food prep, but we need to still run the tests to see whether fixed is best.

# Fixed effects or within estimator
# Just running to compare later with the random effects option. Because of variability of time periods, we will
# employ two way fixed effects. 
fixed1 <- plm(Y1 ~ X, data=pdata2, model = "within")
summary(fixed1)
print(fixed1)
fixef(fixed1)
summary(fixef(fixed1))

# In terms of explanatory variables, there is statistical significance for new dwellings, consumer expenditure,
# consumer price changes, and unemployment. Population is also significant, but
# it is more of control variables. 

## Two way fixed effects

fixed2way2 <- plm(Y1 ~ X, data=pdata2, model = "within", effect="twoways")
summary(fixed2way2)
out2 <- capture.output(summary(fixed2way))
cat("Two-Way Fixed Effects Regression",out2,file="For Copy to PPT Fixed2.txt",sep="\n",append=TRUE)

print(fixed2way2)

# Only have significance with unemployment, consumer expenditures, and consumer prices changes now. Population
# still significant as a control variable. 

fixef(fixed2way2)
summary(fixef(fixed2way2))
fixef(fixed2way2, effect="time")
summary(fixef(fixed2way2, effect="time"))

# Unit and time level fixed effects again extremely statistically significant for almost all. 

# Below is best we have right now until we figure out how to impose black line for ALL Data on top. 
proddata$Country <- as.factor(proddata$Country)

ggplot(proddata,aes(x=Consumerpricesperpaavend,y=Food.Preparation,shape=Country,color=Country,size=Country)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + scale_shape_manual(values=1:nlevels(proddata$Country)) + 
  coord_cartesian(xlim=c(0,10)) 
labs(title="Food Preparation Market Size and Consumerpricesperpaavend with Country-specific regression lines",
     x="Consumerpricesperpaavend", y="Food Preparation Market Size") + theme_classic() 

#Do again with more variables to make a point#

# Example of how we can use the model for future projections. In practice, we'd upload new data as a vector and 
# then run the predict function below to run it through the same model. 
new.values <- data.frame(Averagenoperhousehold=c(1.7,2.4,3.6),
                         Averag.wagesmonthly=c(3345.6,4531.7,6667.1),
                         ElectricalapplianceshousewareMDnomUSD=c(3345.6,4531.7,6667.1),
                         Consumerpricesperpaavend  =c(1.4,2.2,2.9),
                         Medianhouseholdincome=c(3345,44444,23456),
                         Personaldisposableincome =c(2234,11111,23456),
                         Newdwellingscompleted =c(111,2345,1234),
                         Population =c(33451234,222222,123456),
                         RealGDP =c(123456,4567645,45364686),
                         ConsumerexpenditureHousehol.goodsservices..US..=c(100000,5000,25687),
                         Retailsales =c(3345.6,4531.7,6667.1),
                         Unemploymentrate=c(3345.6,4531.7,6667.1))

predict(fixed2way,newdata=new.values)


# And if we run it two way, we get no fixed effects. Need to figure out if this is a result of the data I 
# have and the indicators I am trying to use or whether I am actually running the analysis wrong, forgetting to 
# control for something, etc. 

# Random effects estimator
random1 <- plm(Y1 ~ X1, data=pdata2, model="random")
summary(random1)

# LM Test for fixed effects versus OLS (Would use plmtest(pooling,type=c("bp")) if we wanted to test random vs OLS)
pFtest(fixed1,pooling1)

# If we have a small p, it means that we have significant effects and are
# therefore right to do the fixed effects model vs pooled OLS 
# Results show this condition has been satisfied

# Hausman test for fixed versus random effects model 
phtest(random1,fixed1)

# If we have a small p, it means that we have significant effects and are
# therefore right to do the fixed effects model vs random effects model
# The output saying that one model is inconsistent means that the fixed effects model is consistent. 
# Results support a low enough p value and an alternative hypothesis that says one model is inconsistent,
# meaning that we should be using fixed effects vs random effects. 

# To test whether one way or two way test is appropriate. I think. ?
plmtest(pooling1, effect="twoways", type = "ghm")
# I think the results (p-value that's very low and alternative hypothesis with significant effects)
# mean that two-way is what we should go with

# Test for serial correlation for one-way fixed effects
pcdtest(fixed1, test=c("cd"))
# Still have low p-value...

# Test for serial correlation for two-way fixed effects
pcdtest(fixed2way2, test=c("cd"))
#The p-value comes out high here actually, so maybe this model is even more accurate.  

################## Food Preservation ###############

# Binding the X and Y. X Values subject to change upon further iteration and testing. 
Y2 <- cbind(Food.Preservation)
X <- cbind (Averagenoperhousehold, Averag.wagesmonthly, ElectricalapplianceshousewareMDnomUSD, Consumerpricesperpaavend, Medianhouseholdincome,
             Personaldisposableincome, Newdwellingscompleted, Importdutiespertotal, Population, RealGDP, 
             ConsumerexpenditureHousehol.goodsservices..US.., Retailsales, Unemploymentrate)

# Second best model: Averagenoperhousehold, Averag.wagesmonthly, Currentaccountbalance, Consumerpricesperpaavend, Medianhouseholdincome,
#Personaldisposableincome, Newdwellingscompleted, Importdutiespertotal, Population, RealGDP, 
# ConsumerexpenditureHousehol.goodsservices..US.., Retailsalesnonfood, Unemploymentrate)


# Summary statistics on our independent and dependent variables
summary(Y2)
summary(X)

#Pooled OLS estimator. I include this just so that we can run the LM Test later to show that fixed effects is necessary
# in the first place. Obviously the countries are not all the same and we therefore can't run pooled OLS. Instead
# we need to account for the fact that there will be different intercepts. 
pooling2 <- plm(Y2 ~ X, data=pdata2, model="pooling")
summary(pooling2)

# Very statistically significant for food pres, but we need to still run the tests to see whether fixed is best.

# Fixed effects or within estimator
# Just running to compare later with the random effects option. Because of variability of time periods, we will
# employ two way fixed effects. 
fixed2 <- plm(Y2 ~ X, data=pdata2, model = "within")
summary(fixed2)
print(fixed2)
fixef(fixed2)
summary(fixef(fixed2))

# In terms of explanatory variables, there is statistical significance for consumer expenditure,
# consumer price changes, and unemployment. Population and avg. no per household  also significant, but
# more of control variables. 

## Two way fixed effects

fixed2way3 <- plm(Y2 ~ X, data=pdata2, model = "within", effect="twoways")
summary(fixed2way3)
out3 <- capture.output(summary(fixed2way))
cat("Two-Way Fixed Effects Regression",out3,file="For Copy to PPT Fixed3.txt",sep="\n",append=TRUE)
print(fixed2way3)

# Have significance with unemployment, consumer expenditures, Real GDP, New dwellings, and consumer prices changes now. Population
# still significant as a control variable. 

fixef(fixed2way3)
summary(fixef(fixed2way3))
fixef(fixed2way3, effect="time")
summary(fixef(fixed2way3, effect="time"))

# Unit and time level fixed effects again extremely statistically significant for almost all. 

# Below is best we have right now until we figure out how to impose black line for ALL Data on top. 

ggplot(proddata,aes(x=Consumerpricesperpaavend,y=Food.Preservation,shape=Country,color=Country,size=Country)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + scale_shape_manual(values=1:nlevels(proddata$Country)) + 
  coord_cartesian(xlim=c(0,10)) 
labs(title="Food Preservation Market Size and Consumerpricesperpaavend with Country-specific regression lines",
     x="Consumerpricesperpaavend", y="Food Preservation Market Size") + theme_classic() 

#Do again with more variables to make a point#

# Example of how we can use the model for future projections. In practice, we'd upload new data as a vector and 
# then run the predict function below to run it through the same model. 
new.values <- data.frame(Averagenoperhousehold=c(1.7,2.4,3.6),
                         Averag.wagesmonthly=c(3345.6,4531.7,6667.1),
                         ElectricalapplianceshousewareMDnomUSD=c(3345.6,4531.7,6667.1),
                         Consumerpricesperpaavend  =c(1.4,2.2,2.9),
                         Medianhouseholdincome=c(3345,44444,23456),
                         Personaldisposableincome =c(2234,11111,23456),
                         Newdwellingscompleted =c(111,2345,1234),
                         Population =c(33451234,222222,123456),
                         RealGDP =c(123456,4567645,45364686),
                         ConsumerexpenditureHousehol.goodsservices..US..=c(100000,5000,25687),
                         Retailsales =c(3345.6,4531.7,6667.1),
                         Unemploymentrate=c(3345.6,4531.7,6667.1))

predict(fixed2way3,newdata=new.values)


# And if we run it two way, we get no fixed effects. Need to figure out if this is a result of the data I 
# have and the indicators I am trying to use or whether I am actually running the analysis wrong, forgetting to 
# control for something, etc. 

# Random effects estimator
random2 <- plm(Y2 ~ X, data=pdata2, model="random")
summary(random2)

# LM Test for fixed effects versus OLS (Would use plmtest(pooling,type=c("bp")) if we wanted to test random vs OLS)
pFtest(fixed2,pooling2)

# If we have a small p, it means that we have significant effects and are
# therefore right to do the fixed effects model vs pooled OLS 
# Results show this condition has been satisfied

# Hausman test for fixed versus random effects model 
phtest(random2,fixed2)

# If we have a small p, it means that we have significant effects and are
# therefore right to do the fixed effects model vs random effects model
# The output saying that one model is inconsistent means that the fixed effects model is consistent. 
# Results support a low enough p value and an alternative hypothesis that says one model is inconsistent,
# meaning that we should be using fixed effects vs random effects. 

# To test whether one way or two way test is appropriate. I think. ?
plmtest(pooling2, effect="twoways", type = "ghm")
# I think the results (p-value that's very low and alternative hypothesis with significant effects)
# mean that two-way is what we should go with

# Test for serial correlation for one-way fixed effects
pcdtest(fixed2, test=c("cd"))
# Still have low p-value...

# Test for serial correlation for two-way fixed effects
pcdtest(fixed2way3, test=c("cd"))
#The p-value comes out super high here actually, so maybe this model is even more accurate.  

################ Dish Care ################
# Binding the X and Y. X Values subject to change upon further iteration and testing. 
Y3 <- cbind(Dish.Care)
X <- cbind (Averagenoperhousehold, Averag.wagesmonthly, ElectricalapplianceshousewareMDnomUSD, Consumerpricesperpaavend, Medianhouseholdincome,
            Personaldisposableincome, Newdwellingscompleted, Importdutiespertotal, Population, RealGDP, 
            ConsumerexpenditureHousehol.goodsservices..US.., Retailsales, Unemploymentrate)

# Second best model: Averagenoperhousehold, Averag.wagesmonthly, Currentaccountbalance, Consumerpricesperpaavend, Medianhouseholdincome,
#Personaldisposableincome, Newdwellingscompleted, Importdutiespertotal, Population, RealGDP, 
# ConsumerexpenditureHousehol.goodsservices..US.., Retailsalesnonfood, Unemploymentrate)


# Summary statistics on our independent and dependent variables
summary(Y3)
summary(X)

#Pooled OLS estimator. I include this just so that we can run the LM Test later to show that fixed effects is necessary
# in the first place. Obviously the countries are not all the same and we therefore can't run pooled OLS. Instead
# we need to account for the fact that there will be different intercepts. 
pooling3 <- plm(Y3 ~ X, data=pdata2, model="pooling")
summary(pooling3)

# Very statistically significant for dish care, but we need to still run the tests to see whether fixed is best.

# Fixed effects or within estimator
# Just running to compare later with the random effects option. Because of variability of time periods, we will
# employ two way fixed effects. 
fixed3 <- plm(Y3 ~ X, data=pdata2, model = "within")
summary(fixed3)
print(fixed3)
fixef(fixed3)
summary(fixef(fixed3))

# In terms of explanatory variables, there is statistical significance for consumer expenditure,
# and unemployment. Population and avg. no per household  also significant, but
# more of control variables. 

## Two way fixed effects

fixed2way4 <- plm(Y3 ~ X, data=pdata2, model = "within", effect="twoways")
summary(fixed2way4)
out3 <- capture.output(summary(fixed2way))
cat("Two-Way Fixed Effects Regression",out3,file="For Copy to PPT Fixed3.txt",sep="\n",append=TRUE)
print(fixed2way4)

# Have significance with unemployment, consumer prices changes, and New dwellings. Population and 
# still significant as a control variable. 

fixef(fixed2way4)
summary(fixef(fixed2way4))
fixef(fixed2way4, effect="time")
summary(fixef(fixed2way4, effect="time"))

# Unit and time level fixed effects again extremely statistically significant for almost all. 

# Below is best we have right now until we figure out how to impose black line for ALL Data on top. 

ggplot(proddata,aes(x=Consumerpricesperpaavend,y=Dish.Care,shape=Country,color=Country,size=Country)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + scale_shape_manual(values=1:nlevels(proddata$Country)) + 
  coord_cartesian(xlim=c(0,10)) 
labs(title="Dish Care Market Size and Consumerpricesperpaavend with Country-specific regression lines",
     x="Consumerpricesperpaavend", y="Dish Care Market Size") + theme_classic() 

#Do again with more variables to make a point#

# Example of how we can use the model for future projections. In practice, we'd upload new data as a vector and 
# then run the predict function below to run it through the same model. 
new.values <- data.frame(Averagenoperhousehold=c(1.7,2.4,3.6),
                         Averag.wagesmonthly=c(3345.6,4531.7,6667.1),
                         ElectricalapplianceshousewareMDnomUSD=c(3345.6,4531.7,6667.1),
                         Consumerpricesperpaavend  =c(1.4,2.2,2.9),
                         Medianhouseholdincome=c(3345,44444,23456),
                         Personaldisposableincome =c(2234,11111,23456),
                         Newdwellingscompleted =c(111,2345,1234),
                         Population =c(33451234,222222,123456),
                         RealGDP =c(123456,4567645,45364686),
                         ConsumerexpenditureHousehol.goodsservices..US..=c(100000,5000,25687),
                         Retailsales =c(3345.6,4531.7,6667.1),
                         Unemploymentrate=c(3345.6,4531.7,6667.1))

predict(fixed2way3,newdata=new.values)


# And if we run it two way, we get no fixed effects. Need to figure out if this is a result of the data I 
# have and the indicators I am trying to use or whether I am actually running the analysis wrong, forgetting to 
# control for something, etc. 

# Random effects estimator
random3 <- plm(Y3 ~ X, data=pdata2, model="random")
summary(random3)

# LM Test for fixed effects versus OLS (Would use plmtest(pooling,type=c("bp")) if we wanted to test random vs OLS)
pFtest(fixed3,pooling3)

# If we have a small p, it means that we have significant effects and are
# therefore right to do the fixed effects model vs pooled OLS 
# Results show this condition has been satisfied

# Hausman test for fixed versus random effects model 
phtest(random3,fixed3)

# If we have a small p, it means that we have significant effects and are
# therefore right to do the fixed effects model vs random effects model
# The output saying that one model is inconsistent means that the fixed effects model is consistent. 
# Results support a low enough p value and an alternative hypothesis that says one model is inconsistent,
# meaning that we should be using fixed effects vs random effects. 

# To test whether one way or two way test is appropriate. I think. ?
plmtest(pooling3, effect="twoways", type = "ghm")
# I think the results (p-value that's very low and alternative hypothesis with significant effects)
# mean that two-way is what we should go with

# Test for serial correlation for one-way fixed effects
pcdtest(fixed3, test=c("cd"))
# Higher p value now. 

# Test for serial correlation for two-way fixed effects
pcdtest(fixed2way4, test=c("cd"))
#The p-value comes out super high here actually, so maybe this model is even more accurate.  

################ Floor Care ################
# Binding the X and Y. X Values subject to change upon further iteration and testing. 
Y4 <- cbind(Floor.Care)
X <- cbind (Averagenoperhousehold, Averag.wagesmonthly, ElectricalapplianceshousewareMDnomUSD, Consumerpricesperpaavend, Medianhouseholdincome,
            Personaldisposableincome, Newdwellingscompleted, Importdutiespertotal, Population, RealGDP, 
            ConsumerexpenditureHousehol.goodsservices..US.., Retailsales, Unemploymentrate)

# Second best model: Averagenoperhousehold, Averag.wagesmonthly, Currentaccountbalance, Consumerpricesperpaavend, Medianhouseholdincome,
#Personaldisposableincome, Newdwellingscompleted, Importdutiespertotal, Population, RealGDP, 
# ConsumerexpenditureHousehol.goodsservices..US.., Retailsalesnonfood, Unemploymentrate)


# Summary statistics on our independent and dependent variables
summary(Y4)
summary(X)

#Pooled OLS estimator. I include this just so that we can run the LM Test later to show that fixed effects is necessary
# in the first place. Obviously the countries are not all the same and we therefore can't run pooled OLS. Instead
# we need to account for the fact that there will be different intercepts. 
pooling4 <- plm(Y4 ~ X, data=pdata2, model="pooling")
summary(pooling4)

# Very statistically significant for dish care, but we need to still run the tests to see whether fixed is best.

# Fixed effects or within estimator
# Just running to compare later with the random effects option. Because of variability of time periods, we will
# employ two way fixed effects. 
fixed4 <- plm(Y4 ~ X, data=pdata2, model = "within")
summary(fixed4)
print(fixed4)
fixef(fixed4)
summary(fixef(fixed4))

# In terms of explanatory variables, there is statistical significance for Electrical appliance and houseware demand, 
# personale disposable income, new dwellings completed, and real GDP. Population also significant, but
# more of control variables. 

## Two way fixed effects

fixed2way5 <- plm(Y4 ~ X, data=pdata2, model = "within", effect="twoways")
summary(fixed2way5)
print(fixed2way5)

# Same stat sig as one/way here

fixef(fixed2way5)
summary(fixef(fixed2way5))
fixef(fixed2way5, effect="time")
summary(fixef(fixed2way5, effect="time"))

# Unit and time level fixed effects again extremely statistically significant for almost all. 

# Below is best we have right now until we figure out how to impose black line for ALL Data on top. 

ggplot(proddata,aes(x=Consumerpricesperpaavend,y=Floor.Care,shape=Country,color=Country,size=Country)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + scale_shape_manual(values=1:nlevels(proddata$Country)) + 
  coord_cartesian(xlim=c(0,10)) 
labs(title="Floor Care Market Size and Consumerpricesperpaavend with Country-specific regression lines",
     x="Consumerpricesperpaavend", y="Floor Care Market Size") + theme_classic() 

#Do again with more variables to make a point#

# Example of how we can use the model for future projections. In practice, we'd upload new data as a vector and 
# then run the predict function below to run it through the same model. 
new.values <- data.frame(Averagenoperhousehold=c(1.7,2.4,3.6),
                         Averag.wagesmonthly=c(3345.6,4531.7,6667.1),
                         ElectricalapplianceshousewareMDnomUSD=c(3345.6,4531.7,6667.1),
                         Consumerpricesperpaavend  =c(1.4,2.2,2.9),
                         Medianhouseholdincome=c(3345,44444,23456),
                         Personaldisposableincome =c(2234,11111,23456),
                         Newdwellingscompleted =c(111,2345,1234),
                         Population =c(33451234,222222,123456),
                         RealGDP =c(123456,4567645,45364686),
                         ConsumerexpenditureHousehol.goodsservices..US..=c(100000,5000,25687),
                         Retailsales =c(3345.6,4531.7,6667.1),
                         Unemploymentrate=c(3345.6,4531.7,6667.1))

predict(fixed2way3,newdata=new.values)


# And if we run it two way, we get no fixed effects. Need to figure out if this is a result of the data I 
# have and the indicators I am trying to use or whether I am actually running the analysis wrong, forgetting to 
# control for something, etc. 

# Random effects estimator
random4 <- plm(Y4 ~ X, data=pdata2, model="random")
summary(random4)

# LM Test for fixed effects versus OLS (Would use plmtest(pooling,type=c("bp")) if we wanted to test random vs OLS)
pFtest(fixed4,pooling4)

# If we have a small p, it means that we have significant effects and are
# therefore right to do the fixed effects model vs pooled OLS 
# Results show this condition has been satisfied

# Hausman test for fixed versus random effects model 
phtest(random4,fixed4)

# Seems to be telling us here that random might have been better. Does this have to do with sample size change? 

# To test whether one way or two way test is appropriate. I think. ?
plmtest(pooling4, effect="twoways", type = "ghm")
# I think the results (p-value that's very low and alternative hypothesis with significant effects)
# mean that two-way is what we should go with

# Test for serial correlation for one-way fixed effects
pcdtest(fixed4, test=c("cd"))
# Higher p value now. 

# Test for serial correlation for two-way fixed effects
pcdtest(fixed2way5, test=c("cd"))
#The p-value is lower again now, suggesting potential serial correlation not controlled for with time 
# specific fixed effects

##_______________________________________________________________________________________________
## The lmer4 package approach
library(lme4)
library(arm)

fixed2waylmer <- lmer(Y~X+(1|Country)+(1|Year),data=pdata)
display(fixed2waylmer)

predict(fixed2waylmer)
# Seems to be predicting a bunch of negative market size, which can't be right. 
resid(fixed2waylmer)

summary(fixed2waylmer)$coefficients
# This reports an intercept, so I am assuming we use the fixed effects to assess the specific intercepts 
# for each one? 

# The below is a way to directly test for statistial significance of specific effects. What we are doing is takinng 
# out one effect (I haven't done this yet in the code below) and assessing the change via anova. 
# Would report like this: ". politeness affected pitch (??2
# (1)=11.62, p=0.00065), lowering it by
# about 19.7 Hz ? 5.6 (standard errors) ." (http://www.bodowinter.com/tutorial/bw_LME_tutorial.pdf)

X <- cbind (X..of.HHs.earning...US.25.000.p.a., Realdomesticdemand, Consumerpricesendperiod, Medianhouseholdincome,
            Personaldisposincomeperhead, Avenoperhousehold, GDP.per.head..US.., Households, Currentaccountbalance,
            Total.housing.stock..dwellings.per.1.000.pop., RetailsalesUS, Privateconsumptionperhead, PerPop2039, Urbanpopulationper,
            Energy.intensity..Total.energy.consumption.per.capita, GDP.deflator....change..av., Lendinginterestrate, 
            Real.effective.exchange.rate, Real.GDP.at.factor.cost, Population)
fixed2waylmer.null <- lmer(Y~X+(1|Country)+(1|Year),data=pdata)
anova(fixed2waylmer.null, fixed2waylmer)

coef(fixed2waylmer)

# Is lmer better? What does this even do? P-values obviously still suck here. The "coef" function is big. 
# This would just allow me to plug and play using a normal linear regression set up. Not sure how to do this
# using plm function. 
Y <- cbind(MarketTotal)
X <- cbind (X..of.HHs.earning...US.25.000.p.a., Realdomesticdemand, Consumerpricesendperiod, Medianhouseholdincome,
            Personaldisposincomeperhead, Avenoperhousehold, GDP.per.head..US.., Households, Currentaccountbalance,
            Total.housing.stock..dwellings.per.1.000.pop., RetailsalesUS, Privateconsumptionperhead, PerPop2039, Urbanpopulationper,
            Energy.intensity..Total.energy.consumption.per.capita, GDP.deflator....change..av., Lendinginterestrate, 
            Real.effective.exchange.rate, Real.GDP.at.factor.cost, Population)


rm(X,Y)
rm(fixed2way, fixed, pooling, MVOLS,treg, treg2,fixed2way.resid,fixed2waypred)
rm(mydata,pdata)

plot

## The simpler fixedd effects regression (supposedly)
## Laid out in https://www.youtube.com/watch?v=pQvhi60yN74

Y <- cbind(MarketTotal)
X <- cbind (Country, X..of.HHs.earning...US.10.000.p.a., Consumerpricesendperiod, Medianhouseholdincome,
            Personaldisposincomeperhead, Avenoperhousehold, GDP.per.head..US.., Households, Mean.years.of.schooling,
            Total.housing.stock..dwellings.per.1.000.pop., Privateconsumptionperhead, PerPop2039, Urbanpopulationper,
            Energy.intensity..Total.energy.consumption.per.capita, Lendinginterestrate, Unemploymentrate,
            Real.effective.exchange.rate, Population)

CountDum = as.factor(Country)
TimeDum = as.factor(Year)

treg2 <- lm(MarketTotal ~ CountDum + TimeDum +  Medianhouseholdincome)
summary(treg2)
resid(treg2)


# If there are no fixed effects, then this should be fine right? 
