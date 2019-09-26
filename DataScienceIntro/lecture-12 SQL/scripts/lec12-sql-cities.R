############
#LECTURE 12#
############

# Parallelism and Parallel Example
# Tasks can be mapped. Figure out how many there are. Chunk them (reduce) and make them a batch of tasks that have to happen. 
  # Master nodes send it to slave nodes 
  # Batches get sent to different CPUs (The slave nodes)
    # They'll do the same set of tasks to the batches they've been assigned. 
    # Master node will then put it all back together after the CPUs let it know they are done. 
    # This can be just three lines of code in R!

  # When we create the different workers, R is opening a new session of R and assigning it to that machine. If you have 100 cores available, and take out 100 to do your process, r will be opening up 100 sessions of R. If you have any special libraries that you are using to do your work, that library has to be inside of your loop. 

# Load in the packages designed for parallel processing 
library(doParallel)
library(foreach)
# foreach is a forloop that goes through items in a list and can in this case do serial processing. OR we do it in parallel where we can say that we want 
# doParallel tells R that you want to register cores and CPUs to do your bidding. 

# Method #2 (List -- Fast)
placeholder <- list() # Trick for getting around slow loops 
start <- Sys.time()
for(i in 1:1000) {
  print(i)
n <- 10000
df <- data.frame(y = rnorm(n, 10, 100), x = rnorm(n, 10, 100))
obj <- lm(y ~ x, data = df)
yhat <- predict(obj, df)
temp <- data.frame(iteration = i, y = df$y, yhat = yhat)
placeholder[[i]] <- temp # To assign new objects to a list, you define an index aby setting two square brackets and a number.
}
end <- Sys.time()
end - start


# Method #3 (Parallel -- Faster)
# foreach is preferable to some of the other parallel options. 
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
start <- Sys.time()

foreach(i = 1:1000, .combine = rbind) %dopar% { #.combine gives us the form we want it to come back in. 
  n <- 10000
  df <- data.frame(y = rnorm(n, 10, 100), x = rnorm(n, 10, 100))
  obj <- lm(y ~ x, data = df)
  yhat <- predict(obj, df)
  temp <- data.frame(iteration = i, y = df$y, yhat = yhat)
  return(temp)
}
end <- Sys.time()
stopCluster(cl) # Can eat into operations and slow down your computer if you don't do this. 

#############################################################################

#"https://data.sfgov.org/Housing-and-Buildings/Electrical-Permits/ftty-kx6y"
#"https://data.sfgov.org/Housing-and-Buildings/Plumbing-Permits/a6aw-rudh"

#WD
  setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro/lecture-12/data")
  elec <- read.csv("Electrical_Permits.csv")
  plumb <- read.csv("Plumbing_Permits.csv")

#load library
  install.packages('sqldf')
  library(sqldf)
rm(pig.weights, plumb, pig.weights_wide, pig.weights_wideplot)
########
#SELECT#
########

#Basic select: 1 field
  out <- sqldf('SELECT Block 
                FROM elec')

#Basic select: 3 fields
  out <- sqldf('SELECT Block, Lot, "Permit.Number" as permitNumber 
                FROM elec')

#Basic select: 3 fields but first 10 records
  out <- sqldf('SELECT Block, Lot, "Permit.Number" as permitNumber
               FROM elec
               limit 10')

#Basic select: All fields, first 10 records
# The asterisk is the 'wildcard'
    out <- sqldf('SELECT *
                 FROM elec
                 limit 10')
  
  #This is the equiv as head(elec, 10)
  out
  head(elec, 10)
  
  
###########
#GROUP BY #
###########
#Group by is the equivalent to aggregate()
    
  #Unique list of Block and Lot
    out <- sqldf('SELECT Zipcode
                 FROM elec
                 GROUP BY  Zipcode')
    out
    
  
  #Count number of records in each zipcode
    out <- sqldf('SELECT Zipcode, COUNT(*) as count
                 FROM elec
                 GROUP BY  Zipcode')
    out
    
  #Count Block and Lot
    results <- sqldf('SELECT Zipcode, Block, Lot, COUNT(*) as count
                 FROM elec
                 GROUP BY  Zipcode, Block, Lot')
    results
    
    
##################
#  SUMMARY STATS #
##################
  
  #AVG: Get average number of electrical permits
    sqldf('SELECT AVG(count) as avg
           FROM results')
    
  #SUM: Get total number of electrical permits
    sqldf('SELECT SUM(count) as sum
           FROM results')
    
  #SD: Get SD 
    sqldf('SELECT STDEV(count) as sd
           FROM results')

  #Get all together as well as MIN and MAX
    sqldf('SELECT SUM(count) as sum, AVG(count) as avg, STDEV(count) as sd, MIN(count) as min, MAX(count) as max
          FROM results')
    
  #Get all together  by zipcode
    sqldf('SELECT Zipcode, SUM(count) as sum, AVG(count) as avg, STDEV(count) as sd, MIN(count) as min, MAX(count) as max
          FROM results
          GROUP BY Zipcode') 
    
    
##################
#  CONDITIONAL  #
##################
    
    #WHERE: Get only records in zipcode 94102 
    out <- sqldf('SELECT *
                  FROM elec
                  WHERE Zipcode = 94102')
    
    #WHERE: Get only records in zipcode 94102, 94127, 94109
    out <- sqldf('SELECT *
                 FROM elec
                 WHERE Zipcode IN (94102, 94127, 94109)')
    
    #COUNT WHERE: Count records in zipcode list of 94102, 94127, 94109
    sqldf('SELECT count(*) count
           FROM elec
           WHERE Zipcode IN (94102, 94127, 94109)')
    
    #COUNT WHERE: Count records < 94127
    sqldf('SELECT count(*) count
          FROM elec
          WHERE Zipcode > 94127')
    
    #WHERE: Two conditions - Zipcode 9419 and Status is complete
    out <- sqldf('SELECT *
                  FROM elec
                  WHERE Zipcode = 94109 AND Status = "complete"')

##########
#  JOIN  #
##########
# Same as merge()
    
  ##1) Join two aggregate tables
    #Pre-Prep
    elec_out <- sqldf('SELECT Zipcode, count(*) elec_cnt
                        FROM elec
                        GROUP BY Zipcode')
    plumb_out <- sqldf('SELECT Zipcode, count(*) plumb_cnt 
                        FROM plumb
                        GROUP BY Zipcode')
    
    #INNER JOIN summary tables together
    comb_out <- sqldf('SELECT elec_out.Zipcode, elec_out.elec_cnt, plumb_out.plumb_cnt
                       FROM elec_out
                       INNER JOIN plumb_out ON elec_out.Zipcode = plumb_out.Zipcode')
    
   
    
    