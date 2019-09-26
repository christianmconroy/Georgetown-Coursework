#################
## Homework #1 ##
#################

# Aggregate, dates, expand grid, merge, do.call

#Specify your name
my.name <- "Christian Conroy"
my.email <- "cmc454@georgetown.edu"

#Use this script to get the data
  library(rio)
  police <- import("https://github.com/GeorgetownMcCourt/data-science/blob/master/homework_data/LA-Crime_Data_from_2010_through_2015.csv.zip?raw=true")
  
#Write your code and annotation below
#As those records are ones you'll predict

# Roll up your observations by Crime.Code.Description and Date.Occurred (Long Form)
crimetypedate <- setNames(aggregate(DR.Number ~ Crime.Code.Description + Date.Occurred, data = police, length), c("Crime.Code.Description", "Date.Occurred", "Count"))


# Check that all days in 2010 to 2015 are represented 
# Unique Combos 
# Create variables derived from date variable 
crimetypedate$Date.Occurred <- as.Date(crimetypedate$Date.Occurred, "%m/%d/%Y")

dates <- setNames((seq(as.Date("2010-01-01"), as.Date("2015-12-31"), by= "day")), c("Date.Occurred"))
ucrime <- unique(crimetypedate$Crime.Code.Description)

full.frame <- expand.grid(Date.Occurred = dates, Crime.Code.Description = ucrime)

# crimetypedate$Date.Occurred <- as.Date(crimetypedate$Date.Occurred)

crimetypedate <- merge(crimetypedate, full.frame, by= c("Date.Occurred", "Crime.Code.Description"), all.x = TRUE, all.y = TRUE)

crimetypedate$Count[is.na(crimetypedate$Count)] <- 0

  # Month 
crimetypedate$month <- format(crimetypedate$Date.Occurred, "%m")

  # Weekday
crimetypedate$weekday <- weekdays(crimetypedate$Date.Occurred)

# Split data into two sets
crimetypedata_1014 <- subset(crimetypedate, crimetypedate$Date.Occurred>="2010-01-01" & crimetypedate$Date.Occurred<="2014-12-31")
crimetypedata_15 <- subset(crimetypedate, crimetypedate$Date.Occurred>="2015-01-01" & crimetypedate$Date.Occurred<="2015-12-31")

# Calculate the average and standard deviation by month-weekday-crime type combinations 
##
avsdcrimetype14 <- do.call(data.frame, aggregate(Count ~ month + weekday + Crime.Code.Description, crimetypedata_1014, function(x) c(mean = mean(x), sd = sd(x))))
names(avsdcrimetype14) <- c("month", "weekday", "Crime.Code.Description", "Mean", "SD")

# Calculate upper control limit for each combination
avsdcrimetype14$ulc <- avsdcrimetype14$Mean + (avsdcrimetype14$SD * 3) 

# Subset Outliers in 2015 and report out results 
my.result <- merge(avsdcrimetype14, crimetypedata_15, by=c("month","weekday", "Crime.Code.Description"))
str(my.result)
# 19616 unique combinations
my.result <- subset(my.result, my.result$Count > my.result$ulc)
my.result <-  subset(my.result, select = -c(month, weekday, Mean, SD, ulc, Count))
names(my.result) <- c("crime.type", "date")
print(my.result)
str(my.result)
# 2527 total observations that exceed the upper limit. 
