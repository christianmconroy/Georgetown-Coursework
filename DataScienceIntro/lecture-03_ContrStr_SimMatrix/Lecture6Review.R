n <- 1000
df <- data.frame(y = runif(n), x1 = rnorm(n, 100, 10), x2 = sin(runif(n)))
View(df)

# Create a function to calculate a mean 
myMean <- function(data, var) {
  #
  # Calculate mean of a specified variable 
  #
  # Args: 
  # data = data frame 
  # var = string value that indicates variabele 
  # 
  # Result: 
  #   a single numeric variable
  #
  
  res <- sum(data[[var]])/nrow(data)
  return(res)
  # Above we call a column out
}

myMean(df, "x1")
# When you put a variable name in, always treat it as a string. Double brackets will always include a string in the middle. 

## For Loop Example - Useful ##

myMeans <- function(data, var.vec) {
placeholder <- data.frame()

for(i in var.vec) {
  res <- sum(data[[i]])/nrow(data)
  
  placeholder <- rbind(placeholder, data.frame(var.name = i, avg = res))
}
return(placeholder)
}

myMeans(df, colnames(df))

# How to create a list of dataframes 
dfList <- list(df1 = df, df2 = df, df3 = df)
dfList

str(dfList)
dfList$df1

# do.call is necessary to go from list to data frame
a <- myMeans(df, colnames(df))
do.call(rbind, a)

# Formula Objects 
# Start with regression object 
spec <- as.formula("y ~ x1")
obj <- lm(y ~ x1, data = df)

# Strings containing variable names 
dep <- "y"
ind <- "x1"

# Spec 
spec <- as.formula(paste0(dep, "~", paste(ind, collapse = "+")))
obj <- lm(spec, data = df)

# Formula Objects 

# What happens when the order of variables is wrong for stepwise? If you have 100 variables that you're doing stepwise on, if you're order, 
