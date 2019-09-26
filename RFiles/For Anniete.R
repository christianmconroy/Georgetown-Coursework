# Set WD
setwd("~/GeorgetownMPPMSFS/McCourtMPP/RFiles")
# Load in Data

df <- read.csv("SampleForAnn.csv", stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
final <- df %>%
  gather(var, Response, X1980.Total:X2010.High) %>%  ## Makes wide data long
  separate(var, c("Year", "Skill"), sep = -6)    ## Splits up a column

final$Year <- substring(final$Year, 2, 5)