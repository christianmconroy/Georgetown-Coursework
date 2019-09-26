###################### Bootcamp - Lesson 1 ############################
# https://blackboard.gwu.edu/bbcswebdav/pid-8421973-dt-content-rid-22187071_2/courses/37259_201801/01-Data_Handling.html

a <- 2
b <- 4.876
c <- 'cat'
d <- T

print(a)
print(b)
print(c)
print(d)

typeof(a)
(a+b)^2

round((a+b)^2,2)

if(!d) {
  print('True')
} else {
  print('False')
}

e <- c(a,a^2,a^3,a^4)
f <- c(b, 2*b, 3*b, 4*b)
g <- c(c, paste(c,c))
h <- c(d, !(d), d)
e
f
g
h

# Accessing elements of a list #
e[3]
h[2]
i <- list(e,f,g,h)
i
# i is a list composed of more lists
i[[3]][2]

# Making a matrix - We use sequence to populate it with the first consecutive odd numbers
j <- matrix(seq(1,15,2),
            nrow = 4, ncol = 2,
            byrow = T,
            dimnames = list(c("R1", "R2", "R3", "R4"),
                            c("C1", "C2")))
j

colnames(j) <- c("Col1", "Col2")
j

rownames(j) <- c("Row1","Row2","Row3","Row4")
j

# The next data type is array. An array can have more than 2 dimensions.
k <- array(seq(1,24,1), dim=c(2,3,4))

k[,2,]
k[2,3,1]

# Clustering
age <- c(21, 23, 34, 45, 29, 27, 26, 67, 24, 25)
sex <- c("F","M","M","F","F","F","M","M","F","M")
level <- c(1,2,2,3,2,3,2,4,3,1)

# Combining vectors into a data frame. 
my.df <- data.frame(age, sex, level)
my.df

mean(age)
mean(my.df$age)
mean(my.df[[1]])
summary(my.df)

objects()
head(age)
tail(level, 3)
table(as.factor(my.df$level))

str(my.df)

# Simple Pie Graph - Make the labels first and then create the actual pie chart. 
my.labels <- c("Level 1", "Level 2", "Level 3", "Level 4")
pie(summary(my.df$level), labels = my.labels, main = "Proportion of employees at different organizational levels")

barplot(summary(my.df$level), main="Level Distribution", xlab="Organizational Level", ylab="Number of Employees", col="red")
# Not sure why the above is not working. 

hist(my.df$age, main = "Age Distribution", xlab="Age", ylab="Number of Employees", col="blue")

boxplot(age~sex, data=my.df, main="Age by Sex", xlab="Sex", ylab="Age", col="brown")

# Create data
age <- c(17,18,22,23,22,23,24,25,19,23,21,22,
         21,23,33,27,26,23,23,21,28,23,16,22,
         21,24,25,23,21,19,19,17,23,22,21,17,
         18,18,19,19,20,20,21,20,23,21,21,20)

# Draw histogram
hist(age, 
     prob=TRUE, 
     col='grey')

# Fit normal density function - dnorm adds the curve line over your histogram and the below adds the color and the size too. 
curve(dnorm(x, 
            mean=mean(age), 
            sd=sd(age)),
      add=TRUE, 
      col='red', 
      lwd=2)

# Draw density function that fits the empirical data - While above is to fit normal the blue here is to fit the empirical data that we have. 
lines(density(age), col='blue', lwd=2)

dir()

# Managing the R Workspace 
objects() # Lists the objects in the workspace
ls() # Same as objects()
remove() # Remove objects from the workspace
rm(list=ls()) #clearing memory space (The nuclear option!!!)
search() # Shows the loaded packages (These are the ones I've recently loaded in)
library() # Shows the installed packages
dir() # show files in the working directory

## If Statements! ##
if (salary <= 40000) {
  taxrate = .20
  tax = salary * taxrate
} else
  if(salary <= 100000) {
    taxrate = .25
    tax = salary * taxrate
  } else
  {
    taxrate = .23
    tax = salary * taxrate
  }
# In the above example, we're able to attach .25 based on the condition and then calculate the new thing based on that. 

#ifelse vs. for loops

# If you do not know how many times you want to repeat an operation, you typically use a while loop.

a <- c(11,5,4,6,9,2)
lengtha <- length(a)
i = 1
while (i <= length(a)) {
  print (a[i])
  i = i + 1
} 

# If you know how many times you want to repeat an operation, but you know that you need to perform that operation at least once, you typically use a repeat loop.

a <- c(11,5,4,6,9,2)
i = 1
repeat {
  print (a[i])
  if (a[i] == 6) {
    break
  }
  i = i +1
} 
# Still not really sure on difference between for, while, and repeat...


####################### Class 1 - 1/17/18 #################################
# Introductory Lecture
  # What is a TensorFlow use case? -> This is Google's machine learning        library
    # You can use this using R or Python
    # How do I monetize techniques like TensorFlow? 
    # Face recognition or image recognition is one application
  
  # Yelp: Food images data set
    # Can we recognize what kind of food it is?

  # Once you've developed the machine learning application, how do you       operationalize it?
  
  # scikit learn is another machine learning source
    # Supervised vs. non-supervised 
        # Dimenstionality reduction - If you have a large number of                  variables, the more processing it takes
            # Can we squeeze the usefulness of the data and reduce the                   number of columnns - Going from 100 variables to 7 or 8                    variables with the predictive power of 90-95% of the original               100 variables 
  # Why are we learning R and Python?
    # So that you can be versatile because R and Python account for 90% of     this kind of work

  # Prescriptive models - Optimization
    # Will probably use R more for this part and use a non-linear                optimization framework to minimize risk while aiming for a certain         rate of return 

  # Learn to use Unix because that is the dominant system according to him

  # Aim to spend 3-4 hours per assignment! Wow.
    # Quiz on blackboard every week. 
    # Individual project is R based and uses R Shiny
    # Group project is Python based
  
  # IDE = Integrated Development Environment

  # When you submit your assignments, you'll submit markdown documents 
  # Will learn how to publish to RPubcs

  # An object is something that has characters and atrributes as well as       behaviors
        #  An interger can only have a certain set of values; behaviors in           that it can do addition, subtract, etc. 

test.csv <- read.csv("https://stats.idre.ucla.edu/stat/data/test.csv", header = TRUE)
summary(test.csv)
# Imagine a race course where I have five horses, each named 1,2,3,4,5
  # Nominal
# The race begins and ends with horse 4 coming in first and 5 coming in second and so on 
  # Ordinal 
# Horse 3 wins over horse one by two horse lengths 
  # Interval
# Someone told you the length of the race course is one mile, so we can look at two horse lengths divided by one mile which is twice as much as one horse length divided by one mile 
  # Ratio
# The arrow with a dash is called an assignment operator

# Readr is a package by Wickham that helps you read files better!!!

# Tibbles are simplified data frames
install.packages('tidyverse')

# Vectors are also called compound data types 

myvec <- c(1,2,3,4,10)
myvec[5]
mean(myvec)
sort(myvec)
# Has five values and can be sorted 

myvec2 <- c(1,6,"San")
# Takes data type with lowest behavior and converts to that
# Lists allows me to preserve characteristics 

mylist <- list(3,6,"San")
mylist

# Class Exercise 
=
