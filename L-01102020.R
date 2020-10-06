#format of a function

functionName = function(arg1, arg2) {
  return (c(arg1, arg2))
}

addTwoNumber = function(v1, v2) {
  return(v1 + v2)
}

#se

addTwoNumber(1, 2)

test = function(v1 = "optional") {
  return ("Test")
}

test1 = function(a1 = "a1", b1 = "b1") {
  message(paste(a1, b1))
}

# NAs introduced by coercion
value = as.integer(readline("Please enter a value :"))
valueType = ifelse(value > 0, "Positive", "Negative")
print(valueType)

#NAs is no longer there
checkType = function() {
  value = as.integer(readline("Please enter a value :"))
  valueType = ifelse(value > 0, "Positive", "Negative")
  return(valueType)
}
print(checkType())

v1 = 1:10
15 %in% v1 # special $in$ (existing) // TRUE if 15 is inside v1
for (i in v1) {
  print(i)
}

12 %% 5 # get the remaining value
12 / 5
12 %/% 5 #Ignore the remaining

15 / 4
round(15 / 4, 0)
ceiling(15 / 4)
ceiling(16 / 3)#Always round up !
floor(15 / 4)

factorial = function(v1) {
  if (is.null(v1)) {
    print("No Arugument !")
  }
  
  repeat {
    num = num * v1
    v1 = v1 - 1
    if (v1 <= 1) {
      break
    }
  }
  return(num)
}
factorial(1)

