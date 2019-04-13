## Assignment 1 ##
## Part B ##

# Developing the input sequences for n and p
binom <- function(){
  n <- as.integer(readLines(n = 1)) # Use readlines to create the interaction with a user
  p <- seq(.050, .950, .10)

  # Creating the binomial probability function that will use the inputs.  
  binom <- function(x, N=n, prob = p){
    format(round((dbinom(x, size = N, prob = prob)), 3)) ## Could use pbinom or dbinom. Dbinom does not provide the same rounding issue on the last row (x = 7) that pbinom does. 
  }

# Laying out the output table  
  for(i in 0:(n+3)){
    if(i == 0){
      print(sprintf("n = %d", n)) # Use the + 3 because first three rows of outputs differ from the rest.
    }
    else if (i == 1) { 
      s <- "Probability of success --->"
      print(sprintf("        %s", s))
    }
    else if(i == 2) {
      print(paste0(paste(rep(' ',8), collapse = ''), paste(format(p, nsmall = 3), collapse = ' ')))
    }
    else{
      output <- paste0("x = ", i-3, " | ", paste(binom(x=i-3), collapse = ' '))
      print(output)
    }
  }
}

# Testing the function
binom()
7


