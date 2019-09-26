nums <- 1:50
FizzBuzz <- function(nums) {
  for (i in 1:length(nums)){
    if (i%%3 == 0 & i%%5 !=0){
      print("Fizz")
    }
    else if (i%%3 != 0 & i%%5 ==0){
      print("Buzz")
    }
    if (i%%3 == 0 & i%%5 ==0){
      print("FizzBuzz")
    }
    else {
      print(i)
    }
  }
}

FizzBuzz(nums)