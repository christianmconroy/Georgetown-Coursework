install.packages(c("xts","lubridate","ggplot2","forecast"))
library(xts)
library(lubridate)
library(ggplot2)
library(forecast)

seaice<- read.csv("C:\\Users\\ConroChr\\Desktop\\seaice.csv", stringsAsFactors = FALSE)
View(seaice)

# To filter to observations in nortern hemisphere
seaice <- seaice[seaice$hemisphere == "north",]

# This is for making multiple columns of dates into one column. Very valuable! 
seaice$Date <- as.Date(paste(seaice$Year, seaice$Month, seaice$Day, sep = "-"))

# xts creates an xts time series object (it's a construction function)
# We can also use the order.by to set up time series correctly in date order
seaice.xts <- xts(x= seaice$Extent, order.by = seaice$Date)
str(seaice.xts)

# To aggregate the data (the data we have is collected every 2 days at the moment)
# We're taking all the values for each two days and averaging to get the monthly average. 
# Can do for daily, monthly, quarterly, and yearly
seaice.monthly <-apply.monthly(seaice.xts, mean)
str(seaice.monthly)

# In TS best practice, we take out a chunk of the data as a test to verify at the end of the project
# that our models actually work well 
# 80% to build and train model and 20% held out to not be looked at until end of project to create predictions 
# and see how good it really is 
# Weirdly, you do not randomize in TS when splitting because you want to take the most recent data and hold
# that out as youor test set

seaice.end <- floor(0.8 * length(seaice.monthly))
seaice.train <- seaice.monthly[1:seaice.end]
# We want to start the test set at the number right after the last in the first 80%
seaice.test <- seaice.monthly[(seaice.end+1):length(seaice.monthly)]

# R data visualizes better with normal ts, so let's convert back from xts   
# Give me the start of the TS (Use ?ts in future if there are differences in results or data)
seaice.start <- c(year(start(seaice.train)), month(start(seaice.train)))
# Give me the end of the TS
seaice.end <- c(year(end(seaice.train)), month(end(seaice.train)))
# 
seaice.train <- ts(as.numeric(seaice.train), start = seaice.start, end = seaice.end, frequency = 12)
seaice.train


# R data visualizes better with normal ts, so let's convert back from xts   
# Give me the start of the TS (Use ?ts in future if there are differences in results or data)
seaice.start <- c(year(start(seaice.test)), month(start(seaice.test)))
# Give me the end of the TS
seaice.end <- c(year(end(seaice.test)), month(end(seaice.test)))
# 
seaice.test <- ts(as.numeric(seaice.test), start = seaice.start, end = seaice.end, frequency = 12)
seaice.test

# Create a variable to store the forecast horizon, which says for how many periods in the future we want to 
# forecast for. Into the future is focused on how big our test set is. We're looking at 90 months here. 
forecast.horizon <- length(seaice.test)
newmodel = auto.arima(seaice.train, ic="aic")
summary(newmodel)
# ______________________________________________________________________________________________________________

data("AirPassengers")
ap=AirPassengers
ap
# Of note is that the ap dataset is already in ts form in this example
class(ap)
plot(ap,col=c("blue"))

# The model here would look something like Yt = at st + et (alpha, seasonal, and error)
# To decompose the data into each element of that model:
plot(decompose(ap))
ap.decom = decompose(ap,type="mult")
plot(ap.decom)

trend=ap.decom$trend
trend
seasonal = ap.decom$seasonal
seasonal
# To commpare the trend and the seasonal
ts.plot(cbind(trend,trend*seasonal), lty=1:2)
plot(stl(ap,"periodic"))

## Autoregressive processes - AR(1)
# yt = phiyt-1 + c + et (y1, y2=phiy1 + c + e, y3 = phiy2+c+e3)

random = ap.decom$random
random
# To help not worry about missing values 
pacf(random,na.action=na.exclude)
# We have one lag definitely above (confirmed in acf below)
#Autocorrelation function
acf(random,na.action=na.exclude)
# The goal of the above seems to be to take seasonality and trend out

#c(1,0,0) means one lag. m1 will give us the values of alpha and c. 
m1=arima(random,order=c(1,0,0))
# For a time series, the residuals should be normally distributed. We do Shapiro test to get the p vaue. 
# Null is that the values are normally distributed. Reject if p-value is less than 0.05. We want null to be true 
# in ths case. 
res1=residuals(m1)
shapiro.test(res1)
m1
# In the above the null is confirmed, meaning we have TS data and residuals are normally distributed. 
# To know if intercept and coefficient we have are statisically signiifcant from 0 or not, we run the 
# confidence interval test
confint(m1)
# Significant in that 0 is not contained in the interval. 
# Alpha is 0.40 and constant intercept is .99 and both are significant. 
# Predict data in 1960 for 10 months
predict(m1,n.ahead=10)

# Singular values have no meaning. We have to give a confidence interval of those values. The prediction 
# not really mean anything. Whatever we obtain will be around the values above. CI gives you probability. 

# What if we want to put in two lags? (2,0,0)
m2=arima(random,order=c(2,0,0))
res2=residuals(m2)
shapiro.test(res2)
# P-value greater than 0.05 again. Residuals are normally distributed. 
m2
confint(m2)
# 0 is included in ar2, so AR2 is not significantly different from 0. We obviously then select AR1. 
predict(m2,n.ahead=10)

# One lag corresponded to correlation and one to moving average. 
m3=arima(random,order=c(1,0,1))
res3=residuals(m3)
shapiro.test(res3)
m3
confint(m3)
# 0 is included in ma1, so ma1 is not significantly different from 0.

## The below is switching back to practice for panel data
install.packages("plm")
library(plm)

pracdata<- read.csv("C:\\Users\\ConroChr\\Desktop\\FullMacroeconomicData_062917_TotalMarketData_MarkTOT.csv", sep=";")
attach(pracdata)
Y <- cbind(MarketTotal)
X <- cbind (X..of.HHs.earning...US.10.000.p.a., Consumerpricesendperiod, Medianhouseholdincome,
            Personaldisposincomeperhead, Avenoperhousehold, GDP.per.head..US.., Households, Mean.years.of.schooling,
            Total.housing.stock..dwellings.per.1.000.pop., Privateconsumptionperhead, PerPop2039, Urbanpopulationper,
            Energy.intensity..Total.energy.consumption.per.capita, Lendinginterestrate, Unemploymentrate,
            Real.effective.exchange.rate, Population)

pooling <- plm(Y ~ X, data=pracdata, index=c("Country","Year"), model="pooling")
summary(pooling)
resid(pooling)

fixed <- plm(Y ~ X, data=pracdata, index=c("Country","Year"), model="within")
summary(fixed)
resid(fixed)

random <- plm(Y ~ X, data=pracdata, index=c("Country","Year"), model="random")
summary(random)
resid(random)

phtest(fixed,random)
# The fact that the probability is low means that we can reject the null, meaning that random may not
# be the most appropriate

plmtest(pooling,type=c("bp"))
# The p-value is still low enough to reject the null, which means that mixed effects most appropriate. Probably 
# fixed in this case. 

pcdtest(fixed,test=c("cd"))
# The fact that the p-value is still low means we reject the null. We have serial correlation then.  

#_______________ PLotting the actual data

# Does plot all that we want but it is too crunched and doesn't really show much. 
library(car)
scatterplot(MarketTotal~Year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, ylim=c(0,10000000000)) 

# To change the year to the date format r prefers for timme series 
library(zoo)
as.Date(as.yearmon(Year))

Argentina <- mydata[mydata$Country == "Argentina",]
Argentina.date <- as.Date(Argentina$Year)
Argentina.ts <- zoo(Argentina$MarketTotal, Argentina.date)
plot(Argentina.ts)

Australia <- mydata[mydata$Country == "Australia",]
Australia.date <- as.Date(Argentina$Year)
Australia.ts <- zoo(Australia$MarketTotal, Australia.date)
plot(Australia.ts)

Australia$Country <- NULL
Australia$KPI <- NULL
AustraliaNewest <- t(Australia)

ggplot(AustraliaNewest, aes(x = Year, y = MarketTotal, colour=Country)) +
  geom_line() +
  scale_colour_discrete(name = "Countries") +
  xlab("Year") +
  ylab("Market Total") +
  ggtitle("Market Total by Country, 2008-2016") + ylim(0,10000000000)

# This again is good for all countries, but the scale sucks. 
ggplot(pdata, aes(x = Year, y = MarketTotal, colour=Country)) +
  geom_line() +
  scale_colour_discrete(name = "Countries") +
  xlab("Year") +
  ylab("Market Total") +
  ggtitle("Market Total by Country, 2008-2016") + ylim(0,10000000000)

# _____________________________ Principal Component Analysis in R 

names(mydata)
# Define the variables (For our specific time series in this case)
attach(Argentina)
names(Argentina)
# One way of setting up the data 
X <- cbind (X..of.HHs.earning...US.10.000.p.a., Consumerpricesendperiod, Medianhouseholdincome,
            Personaldisposincomeperhead, Avenoperhousehold, GDP.per.head..US.., Households, Mean.years.of.schooling,
            Total.housing.stock..dwellings.per.1.000.pop., Privateconsumptionperhead, PerPop2039, Urbanpopulationper,
            Energy.intensity..Total.energy.consumption.per.capita, Lendinginterestrate, Unemploymentrate,
            Real.effective.exchange.rate, Population, Tradebalance, Consumerpriceschangepaav, NominaldomesticdemandUS, 
            Consumer.expenditure..Household.goods...services..US.., ElectricalapplianceshousewareMdemandnomU, 
            Totalhousingstockdwellings, Interestratespread)



#Descriptive statistics 
summary(X)
cor(X)

# Principal Component Analysis 
pcal <- princomp(X, scores=TRUE, cor=TRUE)
summary(pcal)
# What we find here is the first component explains 21.36% of the variance, the second component 17.3%,
# third 7.8%, and so on
# We can look at item values, which is sD^2, and we want them to be above 1. This would mean that they 
# explain at least as much of the variance as the original variable. 
# Another rule that we can use is that we use as many of the first components it takes to reach a cumulative 
# proportion of 85% of the data

biplot(iris.pca, cex = .6)

#Alternative method for PCA. This might be better as it alows me to normalize variables. Data looks same though. 
pc <- prcomp(X, center = TRUE, scale.=TRUE)
summary(pc)

print(pc)
# Loadings are normalized linear commbination of original variables 
# Values will lie beteeen -1 and 1. 

# Loadings of principal components 
loadings(pcal)
# R suppresses values here if they are very small 
# We want to see as few as possible loading strongly on the components. Numbers here represent the correlation
# between the component and the original variable 

# Scree plor of the values 
plot(pcal)
# We should retain the components with item values above 1, so looks like comp 1, 2, 3, 4, 5, 6, 7, 8, 9
screeplot(pcal, type="line", main="Scree Plot")
# Same thing as above, but line instead of bar

# Biplot of score variables 
biplot(pcal)
# These are the components 1 and 2 and the original variables, showing how those original variables 
# line up on the component space. 
# This shows graphically what we have for components 1 and 2 in the loadings matrix 

# Scores of the components
pcal$scores[1:10,]

# To create a new data frame with the new PCA components instead of just the original variables 
# Would likely just do dynamic linear regression model per country then with these 
pcap <- predict(pc, X)
pcap
pcap <- data.frame(pcap, mydata$MarketTotal, mydata$Year, mydata$Country)

# Model would look something like (Assuming we did the lag part right)
# Shouold be able to do what we did with Argentina and Australia and just cut the data that way.
# Can do all X variables and see what the Kaiser gives back
lagY <- lag(MarketTotal,k=1)
PCATSmodel <- lm(MarketTotal~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+lagY, data=pcap)
summary(PCATSmodel)
# What does essentially perfect fit mean? Wouldn't that be a good thing?  

## ___________________________ PCA Sample with Argentina TS
# Another way of setting up the data without having to write out all the variables
head(Argentina)
names(Argentina)
Apca <- Argentina[,4:12]
PCPCPC <- princomp(Apca, cor=TRUE,scores=TRUE)
summary()
# Come back to. Not sure why ths is messing up. 

##________________________________________________ < Using the MTS Package

Argentina <- mydata[mydata$Country == "Argentina",]
Argentina$Year <- as.Date(paste(Argentina$Year, 1, 1, sep="-"))
Argentina.timeseries <- ts(Argentina,start = c(2008,1))

install.packages("MTS")
library(MTS)

## ________________________________ Trying to get PCA done for entire short data set

mydata<- read.csv("C:\\Users\\ConroChr\\Desktop\\FullMacroeconomicData_062917_TotalMarketData_MarkTOT.csv", sep=";")
attach(mydata)

Apca <- mydata[,4:67]
install.packages("FactoMineR")
library(FactoMineR)

Apca <- data.frame(lapply(Apca,function(x) as.numeric(as.character(x))))
res <- PCA(Apca)
summary(res)
res


pcap <- predict(res)
pcap
pcap <- data.frame(res, mydata$MarketTotal, mydata$Year, mydata$Country)

# Saving just in case we need later: 

ggplot(mydata,aes(x=Year,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,6.5)) 
labs(title="Market Size and Average no Per Household",
     x="Year", y="Market Size") + theme_classic() 

ggplot(mydata,aes(x=Averagewagesmonthly,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,8000)) 
labs(title="Market Size and Averag.wagesmonthly",
     x="Averag wages monthly ", y="Market Size") + theme_classic() 

ggplot(mydata,aes(x=ElectricalapplianceshousewareMDnomUSD ,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,40000)) 
labs(title="Market Size and ElectricalapplianceshousewareMDnomUSD",
     x="ElectricalapplianceshousewareMDnomUSD ", y="Market Size") + theme_classic() 

ggplot(mydata,aes(x=Consumerpricesperpaavend,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,25)) 
labs(title="Market Size and Consumerpricesperpaavend",
     x="Average no per household", y="Consumerpricesperpaavend") + theme_classic() 

ggplot(mydata,aes(x=Medianhouseholdincome  ,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,100000)) 
labs(title="Market Size and Medianhouseholdincome",
     x="Medianhouseholdincome", y="Market Size") + theme_classic() 

ggplot(mydata,aes(x=Personaldisposableincome,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,45000)) 
labs(title="Market Size and Personaldisposableincome",
     x="Personaldisposableincome", y="Market Size") + theme_classic() 

ggplot(mydata,aes(x=Newdwellingscompleted,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,600)) 
labs(title="Market Size and Newdwellingscompleted",
     x="Newdwellingscompleted", y="Market Size") + theme_classic() 

ggplot(mydata,aes(x=Population,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,150)) 
labs(title="Market Size and Population",
     x="Population", y="Market Size") + theme_classic() 

ggplot(mydata,aes(x=RealGDP,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,25000)) 
labs(title="Market Size and RealGDP",
     x="RealGDP", y="Market Size") + theme_classic() 

ggplot(mydata,aes(x=ConsumerexpenditureHousehol.goodsservices..US..,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,150000)) 
labs(title="Market Size and ConsumerexpenditureHousehol.goodsservices..US..",
     x="ConsumerexpenditureHousehol.goodsservices..US..", y="Market Size") + theme_classic() 

ggplot(mydata,aes(x=Retailsales,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,180000)) 
labs(title="Market Size and Retailsales",
     x="Retailsales", y="Market Size") + theme_classic() 

ggplot(mydata,aes(x=Unemploymentrate,y=MarketTotal)) + geom_point() + 
  geom_smooth(method=lm,se=FALSE,size=.3) + 
  coord_cartesian(xlim=c(0,30)) 
labs(title="Market Size and Unemploymentrate",
     x="Unemploymentrate", y="Market Size") + theme_classic() 
