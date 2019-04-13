# Final Exam Question 2#

# Import Data 
data('mtcars')

# Create Function
carsummary <- function(df, transmission_type, no_of_cylinders) {
  if (nrow(df[df$am == transmission_type & df$cyl == no_of_cylinders,]) == 0) {
    return (NULL)
  } else {
    mtsubset <- df[df$am == transmission_type & df$cyl == no_of_cylinders,]
    retval <- c(mean(mtsubset$mpg), mean(mtsubset$disp))
    return (retval)
  }
}

# Assign to dataframe 
mydf <- mtcars

# Call carsummary 
carsummary(mtcars, 1, 6)
carsummary(mtcars, 0, 9)

# Provide formatted output
# Create New Function
carsummary2 <- function(df, transmission_type, no_of_cylinders) {
  if (nrow(df[df$am == transmission_type & df$cyl == no_of_cylinders,]) == 0) {
    return ("The combination of transmission type and number of cylinders is infeasible")
  } else {
    mtsubset <- df[df$am == transmission_type & df$cyl == no_of_cylinders,]
    retval <- c(mean(mtsubset$mpg), mean(mtsubset$disp))
    message <- sprintf("Average miles per gallon is %.2f Average displacement is %.2f", retval[1], retval[2])
    return (message)
  }
}


# Call carsummary 
carsummary2(mtcars, 1, 6)
carsummary2(mtcars, 0, 9)
