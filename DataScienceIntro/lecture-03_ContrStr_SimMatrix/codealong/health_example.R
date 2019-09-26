## Coding in class ahead of coding ##
earth <- "round"
if(earth == "round"){
  print("The circumference is 24,901 miles!")
}

earth <- "cubed"
if(earth == "round"){
  print("The Earth is a sphere!")
} else {
  print("Check again, buddy.")
}

# Why do I have a problem here?!?!
x <- 5

if(x>5){
  if(x>10){
    print("x > 10")
    } else {
    print("5 < x <= 10")
  }
} else {
  if(x<0) {
    if(x< -10) {
      print("x < 10")
    } else {
      print("-10 <= x < 0")
  } else {
    print("0 <= x <= 5")
  }
  }
}

# For Loops 
for(i in 1:10){
  print(i)
}

vec <- c("CA", "VA", "NY")
for(i in vec){
  print(i)
}

for(i in 1:10){
  for(j in 10:1){
    print(paste(j, i))
  }
}

x <- c()
for(i in 1:10){
  temp <- runif(i)
  x <- c(x, runif(i))
}
x

x <- matrix(NA, nrow = 100, 
            ncol = 100, byrow= T)

#Loop through, replacing items
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    x[i,j] <- runif(1)
    print(paste(i, j))
  }
}
x
length(x)

out <- lapply(1:10, function(x){
  return(runif(x))
})
out
unlist(out)

n0 <- 0
n1 <- 1
f <- 0
for(i in 1:99){
  f <- n0 + n1
  n0 <- n1
  n1 <- f
}
options(scipen=999)
f

temp <- 0
while(temp < 100){
  print(paste("It's ", temp, "F, still cool"))
  temp <- temp + 2
}
print("Too hot now.")

x <- 2

fibseq <- function (n0, n1, high) {
  f <- 0
  for( i in 1:(high - 1)) {
    f <- n0 + n1
    n0 <- n1
    n1 <- f
  }
 return(f)
}
fibseq(0,1,99)

###################################################
##Lecture 3: Simplified Example of CF Item-Item  ##
###################################################

# Underlying data for class slides example of Collaborative Filtering

#Write a cosine function
  cosSim <- function(a, b){
    #
    # Desc: 
    #   Calculates cosine similarity for two numeric vectors
    # 
    # Args:
    #   a, b = numeric vectors
    #
    # Returns: 
    #   Score between 0 and 1
    
    z <- sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
    return(z)
  }

# Set up the data
  health <- data.frame(syringe = c(1,1,0,0), 
                       alc = c(0,1,1,1),
                       band = c(0,0,1,1),
                       insul = c(1,1,0,0),
                       neo = c(0,0,0,1))
  
# Create matrix
  mat <- matrix(NA, 
                ncol = ncol(health), 
                nrow = ncol(health), 
                dimnames = list(colnames(health),colnames(health)))
  
  #Loop through data
  for(i in 1:5){
    for(k in 1:5){
      mat[i,k] <- cosSim(as.matrix(health[,i]), 
                         as.matrix(health[,k]))
    }
  }

#Write results out
write.csv(mat, "health.csv", row.names=FALSE)
