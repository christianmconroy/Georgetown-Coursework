## Stat Programming HW 3 ##
install.packages('tidyverse')
library(tidyverse)

setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/ProgrammingStats") 

## Part A
# Download data
temp <- tempfile(fileext = ".csv")
download.file(url = paste0("https://www2.census.gov/programs-surveys/popest/tables/2010-2015/state/totals/", "nst-est2015-01.csv"), destfile = temp, mode = "wb")

# Read in data with appropriate rows
all_content <- readLines(temp[1])
skip_second <- all_content[c(4, 10:60)]
statepops <- read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE)

# Change column/variable names
colnames(statepops) = c("state", "census", "estbase", "est2010", "est2011", "est2012", "est2013", "est2014", "est2015")

## Problem 1
# Fix number comma issue (Selected over read_csv as it's simpler)
commade <- function(numbers) {
  as.numeric(gsub(",","", numbers))
}
statepops[2:9] <- lapply(statepops[2:9], commade)

## Problem 2
## Fix . before states issue. 
statepops$state <- sub(".","", statepops$state)

## Save the file 
write.csv(statepops, "clean.csv", row.names = FALSE)

## Part B
# Read in the dataframes
install.packages('dplyr')
library(dplyr)
areas <- read.csv("areas.csv", header=T)
areas <- subset(areas, select=c("State", "Land.sq.miles"))
# Can hyst di areas <- select(areas, State, 'Land sq miles')
colnames(areas) <- c("state", "land.sq.miles")
clean <- read.csv("clean.csv", header = T)

# Join the dataframes
joined1 <- left_join(clean, areas, by = "state")
# According to him, you don't have as much control with merge

# Add the mutated column 
joined1 <- mutate(joined1, popdens = est2015/land.sq.miles )

# Read in dataframe 
blueredstates <- read.csv("blueredstates.csv", header=T)
names(blueredstates) <- tolower(names(blueredstates))

# Join the dataframes 
joined2 <- left_join(joined1, blueredstates, by = "state")

# Piping exercise 
joined2 %>% 
  na.omit() %>%
  group_by(overall) %>% 
  summarise(mean(popdens), mean(medianhouseholdincome), mean(land.sq.miles))

## Part C 
#1 
clean2 <- read.csv("clean.csv", header = T)
#2
clean2 <- subset(clean2, select = -c(state, census, estbase))
#3
boxplot(clean2)
#4
colnames(clean2) <- c("2010", "2011", "2012", "2013", "2014", "2015")
means <- lapply(clean2, mean)
years <- as.numeric(colnames(clean2))
plot(years, means, panel.first = grid(), col = 'red', pch = 21, bg = 'red', main = 'Mean of populations across states over the years', xlab = 'Year', ylab = 'Population', ylim=c(6000000, 6500000))

# Could have used gathered here too! Go back and look at his solution file. It might've been quicker. 

