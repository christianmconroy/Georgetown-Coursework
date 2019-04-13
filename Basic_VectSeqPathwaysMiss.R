# Vectors, Sequences, Files, Directories, Pathways, missing values, colnames, pasting

    # BASIC AND VECTORS

# When given two vectors of the same length, R simply performs the specified arithmetic operation (`+`, `-`, `*`, etc.) element-by-element. If the vectors are of different lengths, R 'recycles' the shorter vector until it is the same length as the longer vector. #

# You can type the first two letters of the variable name, then hit the Tab key (possibly more than once). Most programming environments will provide a list of variables that you've created that begin with 'my'. This is called auto-completion and can be quite handy when you have many variables in your workspace. #    

  # FILES, DIRECTORIES, AND PATHWAYS

#  List all the files in your working directory using list.files() or dir().

# For creating a recursive file pathway (Inside of R)
dir.create(file.path('testdir2', 'testdir3'), recursive = TRUE)


  # VECTORS AND SEQUENCES 

# If you want help on an operator, you have to put it in quotations. 

# Creating a vector of number with intervals of 0.5 or length of 30 numbers
seq(0,10,by=0.5)
seq(5,10,length=30)

# Creating a vector with a repeated number 
rep(0, times=40)

# Finally, let's say that rather than repeating the vector (0, 1, 2) over and over again, we
# want our vector to contain 10 zeros, then 10 ones, then 10 twos. We can do this with the
# each` argument. Try rep(c(0, 1, 2), each = 10).
rep(c(0,1,2), each=10)

# Vectors come in two different flavors: atomic vectors and lists. An atomic vector contains
# | exactly one data type, whereas a list may contain multiple data types. We'll explore atomic
# | vectors further before we get to lists.

# Character vectors are also very common in R. Double quotes are used to distinguish character
# | objects, as in the following example.

# The `collapse` argument to the paste() function tells R that when we join together the
# | elements of the my_char character vector, we'd like to separate them with single spaces.
paste(my_char, collapse = " ")


# | Vector recycling! Try paste(LETTERS, 1:4, sep = "-"), where LETTERS is a predefined variable
# | in R containing a character vector of all 26 letters in the English alphabet.
paste(LETTERS, 1:4, sep = "_")

# To create a vector containing 1000 draws from a
# | standard normal distribution:
y <- rnorm(1000).

  # MISSING VALUES AND NOT A NUMBERS

# NaN, which
# | stands for 'not a number'. To generate NaN, try dividing (using a forward slash) 0 by 0 now.

  # SUBSETS
# | The way you tell R that you want to select some particular elements (i.e. a 'subset') from a
# | vector is by placing an 'index vector' in square brackets immediately following the name of
# | the vector.

# To get R to give you just some elements or a conditional of elements in the data, you do: 
x[0]
x[c(3,5,7)]
x[!is.na(x) & x > 0]

  # MATRICES 

# The main difference, as you'll see, is that matrices can only contain a single class of data,
# | while data frames can consist of many different classes of data.

# The example that we've used so far was meant to illustrate the point that a matrix is simply an atomic vector with a dimension attribute

# It looks like the data.frame() function allowed us to store our character vector of names
# right alongside our matrix of numbers. That's exactly what we were hoping for!

# A good way to add column names. First create a vector and then use the colnames function

cnames <- c("patients", "weight", "bmi")
colnames(my_data) <- cnames


