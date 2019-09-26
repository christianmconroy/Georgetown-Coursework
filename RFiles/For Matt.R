# Working with multiple time series
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat"
CBE <- read.table(www,header=T)
CBE[1:4,]
class(CBE)

# To create separate time series for each independent variable
Elec.ts <- ts(CBE[,3], start=1958, freq=12)
Beer.ts <- ts(CBE[,2], start=1958, freq=12)
Choc.ts <- ts(CBE[,1], start=1958, freq=12)

# Quarterly for Elec
Elec.quarterly <- aggregate(Elec.ts, nfrequency=4)
Elec.quarterly2 <- aggregate(elec.deseasoned , nfrequency=4)

#To plot multiple TS at same time. Alternatively, can use ts.intersect to obtain the intersection of the two series if they overlap in time.
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

# Additide Decomposition Model: xt=mt+st+zt where at time t, xt is the observed series, mt is the trend, st is the seasonal effect, and zt is an error term that is, in general, a sequence of correlated random variables with mean zero
# How do we extract the trend mt and the seasonal effect st? 
# A moving average is an average of a specified number of time series values around each value in the time series, with the exception of the first few and last few terms. The goal is to average out the effects of the seasonal effects. What we want is a centered moving average. 

# Smoothing -> The centered moving average is an example of this
# stl uses a locally weighted regression technique known as loess, but it does not produce a formula that can be extracted for forecasting

# In R, the function decompose estimates trends and seasonal effects using a moving average method.
plot(decompose(Elec.ts))
Elec.decom <- decompose(Elec.ts, type = "mult")
plot(Elec.decom)
Trend <- Elec.decom$trend
Seasonal <- Elec.decom$seasonal
ts.plot(cbind(Trend,Trend*Seasonal),lty=1:2)

# Deseasonalize
elec.deseasoned <- Elec.ts - Seasonal

# We used a multiplicative model because the variance of the original series and trend increase with time.
  # For monthly data, an additive model assumes that the difference between the January and July values is approximately the same each year. In other words, the amplitude of the seasonal effect is the same each year.
  # For example, in seasonal data, it might be more useful to model that the July value is the same proportion higher than the January value in each year, rather than assuming that their difference is constant. Assuming that the seasonal and other effects act proportionally on the series is equivalent to a multiplicative model. In many time series involving quantities (e.g. money, wheat production, ...), the absolute differences in the values are of less interest and importance than the percentage changes.
install.packages('imputeTS')
library(imputeTS)

xvals <- c(-50, 175, 149, 214, 247, 237, 225, 329, 729, NA,
       530, 489, 540, 457, 195, 176, NA, 239, 128, 102, 232, 429, 3,
       98, NA, -141, -77, -13, 125, NA, -45, 184)
x <- ts(xvals, start = c(1951, 1), end = c(1958, 4), frequency = 4)
x.withoutNA <- na.kalman(x)
m <- decompose(x.withoutNA)
deseasonalized <- x.withoutNA - m$seasonal
