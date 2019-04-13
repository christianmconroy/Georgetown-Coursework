#################
## Homework #2 ##
#################

#Specify your name
my.name <- "Christian Conroy"
my.email <- "cmc454@georgetown.edu"

#Write your code and annotation below
#As those records are ones you'll predict

setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro")  

# 1
padding <- function(parameter1) {
x <- c(parameter1)
z <- sprintf("%02d", x)
return(z)  
}

#2
dates <- seq(as.Date("2000-01-01"), as.Date("2015-12-31"), by= "month")
dates2 <- paste0(sprintf("%02d", as.numeric(format(dates, "%y"))), padding(as.numeric(format(dates, "%m"))))
dates2 <- expand.grid(dates2 = dates2)
dates2 <- dates2$dates2

#3 

filelist <- as.data.frame(NULL)

for(i in 1:length(dates2)) {
  
  temp <- tempfile(pattern = "dates2", fileext = ".txt")
  
  download.file(url = paste0("https://www2.census.gov/econ/bps/Metro/", "ma",      dates2[i], "c.txt"), destfile = temp[i], mode = "wb")
  
  filelist <- rbind(filelist, read.table(file = temp[i], sep = ",", skip = 3)[, c(1,5,6)])
}

#4 

my.result <- setNames(aggregate(V6 ~ V5, data = filelist, mean), c("msa", "mean"))
mean(my.result$mean)
my.result
  #Your final submission should contain a data frame labeled ("my.result")

