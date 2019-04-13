## Statistical Programming Assignment 2 ##
# Part A # 
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/ProgrammingStats")
install.packages('tidyverse')
library(tidyverse)

household <- read_csv("households.csv")
head(household)

#a
str(household)
household$Household <- as.factor(household$Household)
household$Location <- as.factor(household$Location)
household$Household <- as.factor(household$Ownership)

# Categorical: Household, Location, Ownership
# Continuous: Family Size, First Income, Second Income, Monthly Payment, Utilities, Debt

#b
# Nominal: Household, Location, Ownership

#c
hist(household$Debt)
# The histogram tells you the distribution of household debt within your sample. In this case, it looks like the average is centered somewhere around 4300 with the minimum being close to 0 and the maximum being close to 10000. 

#d
summary(household$Debt)
# Minimum debt is $227 and maximum debt is $9104. 

#e
quantile(household$Debt, c(.25, .50, .70))
# 25%    50%    70% 
# 2948.5 4267.5 5427.6 

#f
IQR(household$Debt)
# The difference between the upper and lower quartiles (25% and 75%) for household indebtedness is $2727

# Part B #
supermarket <- read_csv("SuperMarketTransactions.csv")
# a
supermarket$`Purchase Date` <- as.character(as.Date(supermarket$`Purchase Date`, "%m/%d/%Y"))

supermarket_JanFeb2008 <- subset(supermarket, `Purchase Date` >= "2008-01-01" & `Purchase Date` <= "2008-02-29") 

ggplot(supermarket_JanFeb2008, aes(x= `Units Sold`)) + geom_bar(col = 'red', fill = 'green')
summary(supermarket_JanFeb2008$`Units Sold`)

ggplot(supermarket_JanFeb2008, aes(x= Revenue)) + geom_histogram(col = 'red', fill = 'green')
summary(supermarket_JanFeb2008$Revenue)

4.072/12.909
# .315

#b
supermarket_mfhc <- subset(supermarket, `Marital Status` == "M" & Gender == "F" & Homeowner == "Y" & `State or Province` == "CA")

ggplot(supermarket_mfhc, aes(x= `Units Sold`)) + geom_bar(col = 'red', fill = 'green')
summary(supermarket_mfhc$`Units Sold`)

ggplot(supermarket_mfhc, aes(x= Revenue)) + geom_histogram(col = 'red', fill = 'green')
summary(supermarket_mfhc$Revenue)

4.144/13.03
# .318

# Both subsets demonstrate fairly similar trends, with units sold centered around 4 with slight left skew. However, the first subset seems to show more concentration near the mean while the second subset shows slightly greater dispersion towards the min and max. The distributions of revenue also look very similar, with the second subset showing a slightly higher mean. 

## Part C ##
rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) }
set.seed(1239)
r1 <- rnorm2(100,25,4)
r2 <- rnorm2(50,10,3)
samplingframe <- c(r1,r2)
hist(samplingframe, breaks=20,col = "pink")

# While the visual disribution appears to show right skewness, as the CLT shows, as our sample size increases, our distribution becomes approximately normal. 

# b. Draw 50 samples of size 15 from the sampling frame in part a, and plot the sampling distribution of means as a histogram.
bsam <- replicate(50,sample(samplingframe, 15, replace = TRUE))
bsam <- as.data.frame(bsam)
bsammeans <- as.vector(sapply(bsam, mean))
hist(bsammeans, breaks=20,col = "pink")
# c. Draw 50 samples of size 45 from the sampling frame in part a, and plot the sampling distribution of means as a histogram.
csam <- replicate(50,sample(samplingframe, 45, replace = TRUE))
csam <- as.data.frame(csam)
csammeans <- as.vector(sapply(csam, mean))
hist(csammeans, breaks=20,col = "pink")
# d. Please ensure that the distributions in parts b and c are side-by-side on the same plot. Explain the three histograms in terms of their differences and similarities (in less than 25 words)
par(mfrow=c(1,2))
hist(bsammeans, breaks=20,col = "pink")
hist(csammeans, breaks=20,col = "pink")

# The sampling plot from part a as compared to those in parts b and cappears to be approaching the most "normal"  distribution, which makes sense given the higher sample size. 

# e. Explain CLT in your own words in one or two sentences.
# The CLT denotes the principle that distributions become approximately normal and sample size increases, that is to say, that the mean of all samples taken from the same population approach the approximate mean of the population. 

# f. Does this exercise help you understand CLT? If so why? If not, why not? Restrict your response to one or two sentences.

# Sure. It shows the "process" of reaching the approximately normal distribution, which is made clearer by seeing the distribution become approximately more normal as we are taking more and more samples from a population. 

