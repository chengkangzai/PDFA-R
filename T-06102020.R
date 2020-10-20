radius = as.integer(readline(prompt = "Please give me a radius :"))

switch (
  as.character(readline(prompt = "A for Area , C for Circumference , D for Diameter")),
  "A" = message(c("Area : ", functionA())),
  "C" = message(c("Circumference : ", functionC())),
  "D" = message(c("Diameter : ", functionD())),
  print("NO!")
)

functionA = function() {
  return(radius * 2)
}
functionC = function() {
  return((2 * pi) * radius)
}
functionD = function() {
  return(pi * (radius * radius))
}

#############################################################################
#2
paste(functionReverse(as.character(
  readline(prompt = "Give me number between 1 and 9999")
)), collapse = '')

functionReverse = function(input) {
  return(as.integer(rev(strsplit(input, NULL)[[1]])))
}

#############################################################################
#3
message(getSalary(as.integer(readline(prompt = "You sale how much boi"))))

getSalary = function(sales) {
  return((sales * 15 / 100) + 100)
}

#############################################################################
#4
message(getFactoria(as.integer(readline(prompt = "Can Give me a number arh ?"))))

getFactoria = function(input) {
  fact = 1
  for (i in 1:input) {
    fact = fact * i
  }
  return(fact)
}

#############################################################################
## Q1 Update
q1 = function() {
  radius = as.integer(readline(prompt = "Enter radius: "))
  print(paste("Calculate: A = Area, B = Circumference, C = Diameter"))
  option = readline(prompt = "Enter calculation type: ")
  calculation = switch (
    option,
    "A" = 3.142 * radius * radius,
    "B" = 2 * 3.142 * radius,
    "C" = 2 * radius
    
  )
  print(paste("The calculation is", calculation))
}

q1()


