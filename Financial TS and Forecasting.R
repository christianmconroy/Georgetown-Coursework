## ---------------------------------------------------------------------##
## THE BELOW IS ALL FOR TIME SERIES ANALYSIS 
 cxv cvx cvx cvx c c

 cvx cxv cxv cxn cvx cx

##Air Passengers Example: Plots, trends, and seasonal variation

data("AirPassengers")
AP <- AirPassengers
AP
## This data is already in the TS format

class(AP)
start(AP); end(AP); frequency(AP)
summary(AP)

plot(AP, ylab = "Passengers (1000's)")

layout(1:2)
plot(aggregate(AP))
boxplot(AP~cycle(AP))

www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/Maine.dat"
Maine.month <- read.table(www, header=TRUE)
attach(Maine.month)
class(Maine.month)
# The data above is in data frame, not TS, format

Maine.month.ts <- ts(unemploy, start=c(1996,1), freq=12)

# Divide by 12 to get the mean annual rate
Maine.annual.ts <- aggregate(Maine.month.ts)/12

par(mar = rep(2, 4))
layout(1:2)
plot(Maine.month.ts, ylab="unemployed (%)")
plot(Maine.annual.ts, ylab="unemployed (%)")

# Extract that part of the TS between specified start and end points
Maine.Feb <- window(Maine.month.ts, start=c(1996,2), freq=TRUE)
Maine.Aug <- window(Maine.month.ts, start=c(1996,8), freq=TRUE)
Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug)/mean(Maine.month.ts)
Feb.ratio
Aug.ratio
# On average, unemployment is 22% higher in February and 18% lower in August. 

www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/USunemp.dat"
US.month <- read.table(www, header=T)
attach(US.month)
US.month.ts <- ts(USun, start=c(1996,1), end=c(2006,10), freq=12)
plot(US.month.ts,ylab="unemployed(%)")

# Working with multiple time series
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat"
CBE <- read.table(www,header=T)
CBE[1:4,]
class(CBE)

# To create separate time series for each independent variable
Elec.ts <- ts(CBE[,3], start=1958, freq=12)
Beer.ts <- ts(CBE[,2], start=1958, freq=12)
Choc.ts <- ts(CBE[,1], start=1958, freq=12)

#To plot multiple TS at same time. Alternatively, can use ts.intersect to obtain the intersection of the two series if they overlap in time.
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

# For two series that overlap in time
AP.elec <- ts.intersect(AP,Elec.ts)

start(AP.elec)
end(AP.elec)
AP.elec[1:3,]

# To extract and plot data for both of the series in the intersection
AP <- AP.elec[,1]
Elec <- AP.elec[,2]
layout(1:2)
plot(AP, main="",ylab="Air passengers/1000's")
plot(Elec, main="", ylab="Electricity Production/MkWh")

# as.vector is needed to convert the ts object to ordinary vectors suitable for a scatter plot.
plot(as.vector(AP), as.vector(Elec),
     xlab="Air passengers/1000's",
     ylab="Electricity production/MWh")
abline(reg=lm(Elec~AP))

# Quarterly exchange rates
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/pounds_nz.dat"
Z <- read.table(www, header=T)

Z[1:4,]
# We use 4 for the frequency below because we're concerned with quarterly data
Z.ts <- ts(Z, st=1991, fr=4)
plot(Z.ts, xlab="time/years", 
     ylab="Quarterly exchange rate in $NZ/pound")

# To deal with stochastic trends, we usually use a random walk model. Stochastic trends are common in financial series. 

Z.92.96 <- window(Z.ts, start=c(1992,1), end=c(1996,1))
Z.96.98 <- window(Z.ts, start=c(1996,1), end=c(1998,1))

layout(1:2)
plot(Z.92.96, ylab="Exchange rate in $NZ/pound", xlab="Time (years)")
plot(Z.96.98, ylab="Exchange rate in $NZ/pound", xlab="Time(years")

# Global temperature time series 
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat"
# Scan is used to read data into a vector or list from the console or file. It is evidently a way of reading columnar data. 
Global <- scan(www)
Global.ts <- ts(Global, st=c(1856,1), end=c(2005,12), fr=12)
# Because the trend is what we care about here, the aggregate function is used to remove any seasonal effects within each year and produce an annual series of mean temperatures. (Even if the data set is monthly.)
Global.annual <- aggregate(Global.ts, FUN = mean)
# When you plot in TS, X is always the time, which is known in the plot by virtue of having already made what you are plotting a TS object
plot(Global.annual)

# To extract the monthly time intervals corresponding to the 36-year period. window is a generic function which extracts the subset of the object x observed between the times start and end. 
New.series <- window(Global.ts, start=c(1970,1))
# Time subsequently creates he vector of times at which a time series was sampled above. 
New.time <- time(New.series)
plot(New.series); abline(reg=lm(New.series~New.time))

## Fitting Time Series Models ##
# TS Forcast model: {xt:t=1,...,n},xhatt+k|t
# Additide Decomposition Model: xt=mt+st+zt where at time t, xt is the observed series, mt is the trend, st is the seasonal effect, and zt is an error term that is, in general, a sequence of correlated random variables with mean zero
# How do we extract the trend mt and the seasonal effect st? 
  # A moving average is an average of a specified number of time series values around each value in the time series, with the exception of the first few and last few terms. The goal is to average out the effects of the seasonal effects. What we want is a centered moving average. 

# Smoothing -> The centered moving average is an example of this
# stl uses a locally weighted regression technique known as loess, but it does not produce a formula that can be extracted for forecasting

# In R, the function decompose estimates trends and seasonal effects using a moving average method.
# So we can next stl within plot by doing plot(stl()) and show the smoothed annual trends with marks for the monthly trends. 
plot(decompose(Elec.ts))
Elec.decom <- decompose(Elec.ts, type = "mult")
plot(Elec.decom)
Trend <- Elec.decom$trend
Seasonal <- Elec.decom$seasonal
ts.plot(cbind(Trend,Trend*Seasonal),lty=1:2)
# We used a multiplicative model because the variance of the original series and trend increase with time. 

#Correlations
# The expected value, commonly abbreviated to expectation, E, of a variable, or a function of a variable, is its mean value in a population
# In r, we can calculate a sample covariance with denominator n-1 by using the function cov

www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/Herald.dat"
Herald.dat <- read.table(www, header=T)
attach(Herald.dat)

x <- CO; y <- Benzoa; n <- length(x)
sum((x-mean(x))*(y-mean(y)))/(n-1)
#OR
mean((x-mean(x))*(y-mean(y)))
#OR
cov(x,y)

# To calculate the correlation between the CO and the benzoapyrene measurements at Herald Square using cor
cov(x,y)/(sd(x)*sd(y))
cor(x,y)
# Although the correlation is small, there is a physical explanation for the correlation because both products are a result of incomplete combustion. 
# A time series model that is stationary in the mean is ergodic in the mean if the time average for a single time series tends to the ensemble mean as the length of the time series increases. 

# We can have many time series arising from the time series model - For xample, Number of time series being equal to number of prototypes for acceleration at the pilot seat of a new design of microlight aircraft in simulated random gusts in a wind tunnel where there are multiple prototypes

# A model is second-order stationary if the correlation between the variables depends only on the number of time series separating them. The number of time steps between them is known as the lag. A correlation of a variable with itself at different times is known as autocorrelation or serial correlation. 

www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/wave.dat"
wave.dat <- read.table(www, header=T); attach(wave.dat)
layout(1:2)
plot(ts(waveht))
plot(ts(waveht[1:60]))
# The first plot above shows the entire time series. The second one shows only the first 60 wave heights. 
# To find thr lag 1 autocorrelation for waveht:
acf(waveht)$acf[2]
# We can draw a plot corresponding to both 1 and 2 autocorrelation
plot(waveht[1:396],waveht[2:397])
# To find lag 1 autocovariance (Still not sure what autocovariance is)
acf(waveht,type=c("covariance"))$acf[2]

# For monthly series, a significant autocorrelation at lag 12 might indicate that the seasonal adjustment is not adequate
acf(AirPassengers)
# What we see here is a trend in the data that shows in the correlogram a slow decay in the autocorrelations, which are large and positive due to similar values in the series occurring close together in time. The main point of the correlogram is to detect autocorrelations in the time series afer we have removed an estimate of the trend and seasonal variation. 

# To seasonally adjust and remove the trend using decompose. 
data(AirPassengers)
AP <- AirPassengers
AP.decom <- decompose(AP, "multiplicative")
plot(ts(AP.decom$random[7:138]))
acf(AP.decom$random[7:138])

# Start at 7 because in drawing the correlogram, the first six and last six terms in the random component cannot be calculated and thus are stored in R as NA
sd(AP[7:138])
sd(AP[7:138]-AP.decom$trend[7:138])
sd(AP.decom$random[7:138])
# The reduction in the sd shows that the seasonal adjustment has been effective. 
# The main difference between the regression approach and using decompose is that the former assumes a linear trend, whereas the latter smooths the time series without assuming any particular form for the trend 
# Plotting the correlogram for this
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/Fontdsdt.dat"
Fontdsdt.dat <- read.table(www,header=T)
attach(Fontdsdt.dat)
plot(ts(adflow), ylab='adflow')
acf(adflow, xlab='lag (months)', main="")
# There is statistically significant correlation at lag 1 as evidenced by the correlogram. This implies that the inflow next month is more likely than not to be above average if the inflow this month is above average and same for below.

# Forecasts!!!!!!!!
# An efficient method of forecasting is to find a related variable that leads to it by one or more time intervals. 
# The data in this file include the total dwellings approved per month, averaged over the past three months and the value of work done over the three months. 

www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/ApprovActiv.dat"
Build.dat <- read.table(www,header=T); attach(Build.dat)
App.ts <- ts(Approvals, start=c(1996,1), freq=4)
Act.ts <- ts(Activity, start = c(1996,1), freq=4)
# We can use ts.plot to put two time series on the same plot. Here, we look at approvals and activity. 
ts.plot(App.ts, Act.ts, lty=c(1,3))
#The ts.union function binds time series with a common frequency, padding with "NAs" to the union of their time coverages. Can use with acf to get a correlogram for the two variables.
acf(ts.union(App.ts,Act.ts))


app.ran <- decompose(App.ts)$random
app.ran.ts <- window(app.ran, start=c(1996,3))
act.ran <- decompose (Act.ts)$random
# Not sure why below is not working... Missing values in object? 
act.ran.ts <- window(act.ran, start=c(1996,3))
acf(ts.union(app.ran.ts, act.ran.ts))
ccf(app.ran.ts, act.ran.ts)

# To get table for ccf?
print(acf(ts.union(app.ran.ts, act.ran.ts)))

# The Bass formula for the number of people Nt who have bought a product at time t depends on three parameters: the total number of people who eventually buy the product, M; the coefficient of innovation, p; and the coefficient of imitation, q

# Bass curve for yearly sales of VCRs in the US home market between 1980 and 1989 using R non-linear least squares function nls
# The time from 1979
T79 <- 1:10
Tdelt <- (1:100)/10
Sales <- c(840,1470,2110,4000,7590,10950,10530,9470,7790,5890)
# Cumsum is useful for monitoring changes in the mean level of the process
Cusales <- cumsum(Sales)
# Use the Bass formula and input m, p, and q
Bass.nls <- nls(Sales~M*(((P+Q)^2/P)*exp(-(P+Q)*T79))/(1+(Q/P)*exp(-(P+Q)*T79))^2, start=list(M=60630, P=0.03, Q=0.38))
summary(Bass.nls)
# To plot the data and fitted curve
Bcoef <- coef(Bass.nls)
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]

ngete<- exp(-(p+q)*Tdelt)
Bpdf <- m * ((p+q)^2/p) * ngete/(1+(q/p)*ngete)^2
# Bass sales curve fitted to sales of VCRs in the US Home Market, 1980-1989
plot(Tdelt,Bpdf,xlab="Year from 1979", 
     ylab="Sales per year", type='l')
points(T79, Sales)
Bcdf <- m * (1-ngete)/(1+(q/p)*ngete)
# Bass cumulative sales surve obtained as the integral of the sales curve and cumulative sales of VCRs in the US home market 1980-89
plot(Tdelt, Bcdf, xlab = "Year from 1979", ylab = "Cumulative Sales", type="l")
points(T79, Cusales)

# Exponential Smoothing and Holt-Winters
# We use exponential smoothing when our objective is to predict some future value xn+k, given a past history of obserations up to time n. 
# In examples below, we assume no systematic trend or seasonal effects
# The model: xt = ut + wt (ut is non-stationary mean of the process at time time t and wt are independent random deviations with a mean of 0 and a standard deviation of sigma)
# We use a smoothing parameter

www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/motororg.dat"
Motor.dat <- read.table(www, header=T)
attach(Motor.dat)
Comp.ts <- ts(complaints, start=c(1996,1), freq=12)
# Just the basic thing you do for every TS
plot(Comp.ts, xlab="Time/months", ylab="Complaints")

# Because there is no evidence of a systematic trend or seasonal effects, it seems reasonable to use exponential smoothing. Exponential smoothing is a special case of HoltWinters

Comp.hw1 <- HoltWinters(complaints, beta=0, gamma=F); Comp.hw1
# Had to make gamma False and omit seasonal component because I supposedly did not have enough information 
plot(Comp.hw1)
# The estimated value of the mean number of letters of complaint per month at the end of 1999 is 23 (alpha). 
Comp.hw1$SSE

Comp.hw2 <- HoltWinters(complaints, alpha=0.2, beta=0, gamma=F)
Comp.hw2
# Estimated value is now at 45.98 letters. This change of alpha deals with when there might be certain trends I think. 

# Holt Winters uses seasonally exponentially weighted moving averages to update estimates of the seasonally adjusted mean (called the level), slope, and seasonals

www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/wine.dat"
wine.dat <- read.table(www, header=T)
attach(wine.dat)

sweetw.ts <- ts(sweetw, start=c(1980,1), freq=12)
plot(sweetw.ts, xlab="Time(months)",ylab="sales (1000 litres)")
sweetw.hw <- HoltWinters(sweetw.ts, seasonal="mult")
sweetw.hw; sweetw.hw$coef; sweetw.hw$SSE

sqrt(sweetw.hw$SSE/length(sweetw))
sd(sweetw)

plot(sweetw.hw$fitted)
plot(sweetw.hw)

# The optimum values for the smoothing parameters, based on minimizing the one-step ahead prediction errors are 0.4107, .0001516, and 0.4695. The coefficients are estimated values of the level, slope, and multiplicative seasonals from January to December available at the latest time point. We use these for predictions. 

AP.hw <- HoltWinters(AP, seasonal="mult")
plot(AP.hw)
# We use this to make forecasts into the future based on previous trends. 
AP.predict <- predict(AP.hw, n.ahead=4*12)
ts.plot(AP, AP.predict, lty=1:2)

# Basic Stochastic Models 
# When we fit mathematical models to time series data, we refer to the discrepencies between fitted values, calculated from the model, and the data as residual error series 
# A fundamental non-stationary model based on discrete white noise is called a random walk, which is sometimes a good model for financial series 
# A time series is discrete white noise if the variables are independent and identically distributed with a mean of zero. 

# A fitted time series model can be used to simulate data -> Useful for bootstrapping
# Set.seed provides a starting point in the simulations, thus ensuring that the simulations can be reproduced
set.seed(1)
w <- rnorm(100)
plot(w, type="l")

# The below is a histogram of a Gaussian White Noise Series 
x <- seq(-3,3, length = 1000)
hist(rnorm(100), prob=T); points(x, dnorm(x), type="l")

# Will still have at least a little autocorrelation due to sampling variation
set.seed(2)
acf(rnorm(100))

# A random walk often provides a good fit to data with stochastic trends, although even better fits are usually obtained from more general model forumlations like ARIMA

# To simulate random walk data for x.
# We place a white noise series into w and use this series to initialize x. 
# The for loop generates the random walk. 
x <- w <- rnorm(1000)
for (t in 2:1000) x[t] <- x[t-1] + w[t]
plot(x, type = "l")

# Correlogram of the series can be used to assess whther a given series is reasonbly modeled as a random walk 
acf(diff(x))

# An additional term can be added to the random walk model using the Holt-Winters, allowing the parameter B to be non-zero but still forcing the seasonal term Lambda to be zero: 

Z.hw <- HoltWinters(Z.ts, alpha=1, gamma=0)
acf(resid(Z.hw))
# Correlelogram is more consistent with the hypothesis that the residual series is white noise. 

#The random walk model can be adapted to allow for the expectation company stockholders have for their investment to increase in value despite the volatility of financial markets. 

# GREAT EXAMPLE! GO BACK AND DO WITH THD ***************************************
# Closing prices (USD) for HP Company stock for 672 trading days up to June 7, 2007 are read into R and plotted. The lag 1 differences are calculated using diff() and plotted in Figure 4.9. The correlelogram of the difference is in Figure 4.10 and they appear to be well modelled as white noise. Since the confidence interval for the drift parameter [0.004, 0.075] does not include 0, we have evidence of a positive drift over this period. 

www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/HP.txt"
HP.dat <- read.table(www, header = T)
attach(HP.dat)
plot(as.ts(Price))
# To show lag 1 differences of the closing prices (below)
DP <- diff(Price)
plot(as.ts(DP))
# Autocorrelation function (ACF) of lag 1 differences of daily closing prices. 
acf(DP)

# To get the confidence interval of parameter drift. 
mean(DP)+c(-2,2) * sd(DP)/sqrt(length(DP))

# The random walk is a special case of AR(1) with alpha(1)=1
# The exponential smoothing model is a special case of alpha(1)=alpha(1-alpha)^i for i = 1,2... and p -> Infinity

# Correlogram of an AR(1) process
# The following example gives two correlograms for positive and negative values of alpha
rho <- function(k,alpha) alpha^k
layout(1:2)
plot(0:10, rho(0:10, 0.7), type="b")
plot(0:10, rho(0:10, -0.7), type="b")

# Simulating an AR(1) process
set.seed(1)
x <- w <- rnorm(100)
for (t in 2:100) x[t] <- 0.7 * x[t-1] +w[t]
plot(x, type="l")
acf(x)
pacf(x)
# For the above, the partial correlogram has no significant correlations except the value at lag 1. The pacf starts at lag 1 while the acf starts at lage 0. All of this is meant to show the discrepancies that have arisen due to sampling variation. 

# Model fitted to simulated series
# An AR(p) model can be fitted to data in R using the ar function. 

x.ar <- ar(x, method = "mle")
x.ar$order

x.ar$ar

x.ar$ar + c(-2,2) * sqrt(x.ar$asy.var)
# The method mle above is based on maximizing the likelihood function (the probability of obtaining the data given the model) with respect to unknown parameters  

# Akaike Information Criterion (AIC) penalizes models with too many parameters 

Z.ar <- ar(Z.ts)
mean(Z.ts)
Z.ar$order
Z.ar$ar
Z.ar$ar + c(-2,2) * sqrt(Z.ar$asy.var)
# The above doesn't work but instead says that recycling array of length 1 in vector-array arithmetic is deprecated (Not sure what this means)

acf(Z.ar$res[-1])

www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat"
Global <- scan(www)

Global.ts <- ts(Global, st = c(1856,1), end=c(2005, 12), fr = 12)
Global.ar <- ar(aggregate(Global.ts, FUN = mean), method = "mle")
mean(aggregate(Global.ts, FUN=mean))
# Ar above is used for fitting Autoregressive models to time series. What is great is that it can supposedly be used to get the best fitting AR(p) model. 

Global.ar$order
Global.ar$ar

acf(Global.ar$res[-(1:Global.ar$order)], lag=50)
# Here a stochastic model can explain the correlation and trends in the series. Stochastic means that it shows inexplicable changes in direction and we attribute apparent transient trends to high serial correlation with random error. Trends of this type can be simulated with random walk or autoregressive models 

## REGRESSIONS WITH TIME SERIES ########################################3 

# Time series regression differs from standard regression in that the residuals tend to be serially correlated, meaning that the estimated standard errors would tend to be less than their true value, which would in turn lead to erroneously high statistical significance being attributed to statistical tests as p-values will be smaller than they should be. 

# Linear models for time series are non-stationary when they include functions of time

set.seed(1)
z <- w <- rnorm(100, sd = 20)
for (t in 2:100) z[t] <- 0.8 * z[t-1] + w[t]
Time <- 1:100
x <- 50 + 3 * Time + z
plot(x, xlab = "time", type = "l")

x.lm <- lm(x ~ Time)
coef(x.lm)

sqrt(diag(vcov(x.lm)))
# The above gives us the standard error. 

# Again, with time series regression, the correlogram of residuals is key. 
acf(resid(x.lm))
pacf(resid(x.lm))

# Using the global climate data we loaded in above
temp <- window(Global.ts, start=1970)
temp.lm <- lm(temp ~ time(temp))
coef(temp.lm)
confint(temp.lm)
# The confint for the slope does not contain zero, but the bigger story is that below we see an underestimate of the standard error and too narrow of a confidence interval. 
acf(resid(lm(temp ~ time(temp))))

## Generalized Least Squares (Like in Panel Data Class)

# GLS accounts for autocorrelation of residuals 
library(nlme)
x.gls <- gls(x ~ Time, cor = corAR1(0.8))
coef(x.gls)
sqrt(diag(vcov(x.gls)))

# For historical series, the lag 1 autocorrelation would need to be estimated from the correlogram of the residuals of a fitted linear model (So we have to fit OLS first and get the residual correlogram from that).

temp.gls <- gls(temp ~ time(temp), cor = corAR1(0.7))
confint(temp.gls)

# Can look at seasonality by treating seasonal term st as a factor. 
Seas <- cycle(temp)
Time <- time(temp)
temp.lm <- lm(temp ~ 0 + Time + factor(Seas))
# The above creates the vector of times at which a time series is sampled. 
coef(temp.lm)
# We use 0 in the model to ensure that the model does not have an intercept because if it is included then one of the seasonal terms will be dropped and an estimate for the intercept will appear in the output. 

# The below is used to get a projection out into the future. 
new.t <- seq(2006, len = 2 * 12, by = 1/12)
alpha <- coef(temp.lm)[1]
beta <- rep(coef(temp.lm)[2:13], 2)
(alpha * new.t + beta)[1:4]

# The below is better and easier than the above. 
new.dat <- data.frame(Time = new.t, Seas = rep(1:12, 2))
predict(temp.lm, new.dat)

# If seasonal effects vary smoothly over the seasons, it is more useful to use a harmonic seasonal model 

data("AirPassengers")
AP <- AirPassengers
plot(AP)
plot(log(AP))

## FORECASTING FROM REGRESSION
# Best to think about forecast from regression as an expected value conditional on past trends continuing into the future. 

# To predict, you always need to make sure that the new data are properly defined and labelled in a data.frame
new.t <- time(ts(start = 1961, end = c(1970,12), fr = 12))
TIME <- (time(AP) - mean(time(AP)))/sd(time(AP))
SIN <- COS <- matrix(nr = length(AP), nc = 6)
for (i in 1:6) {
  SIN[,i] <- sin(2 * pi * i * time(AP))
  COS[,i] <- cos(2 * pi * i * time(AP))
}

SIN <- SIN[, -6]
new.dat <- data.frame(TIME = as.vector(TIME), SIN = SIN, COS = COS)
AP.pred.ts <- exp(ts(predict(AP.lm2, new.dat), st = 1961, fr = 12 c cxv cvx))


##### MV Time Series Analysis With R and Financial Applications ###

# In this book, we focus only on weakly stationary series and MV linear time series
# In forecasting, we like to express the time series zt as a function of its lagged values zt-1 for i > 0 plus new information at time t

# We usually need to obtain cross-correlation plots befor we do anything -> to easure the linear dynamic dependence of a stationary time series, we define its lag l cross-covariance matrix 

# To create a 2 x 2 identity matrix for use in the correlation matrix plot below
sig = diag(2)
# To generate random draws
library(LearnBayes)
x = rmnorm(300, rep(0,2), sig)
install.packages("MTS")
library(MTS)
# To obtain the time series plots 
MTSplot(x)
# For the covariance matrix and cross correlation plot
ccm(x)

# A basic test in MV time series analysis is to detect the existence of linear dynamic dependence in the data. This amounts to testing the null hypothesis H0: P1` = ... = pm = 0 versus the alternative hypothesis Halpha: pi != 0 for some i satisfying i <= i <= m, where m is a positive interger

# To forecast, we want to do an l-step ahead forecast of the series at the time index h. Here h is the forecast origin and l the forecast horizon

# Forecasts produced by an econometric model also depend on the loss function used. In this book, we follow the tradition by using the minimum mean square error (MSE) prediction. 

# The moving average representation (MA) is useful in forecasting, including computing the covariance of a forecast error and studying the impulse response function. `

############################# Github #############################
setwd("/Users/chris/Documents/newGitTest")
getwd()
