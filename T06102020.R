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
##miss understanding
paste(functionReverse(as.character(
  readline(prompt = "Give me sth! i will reverse it")
)), collapse = '')

functionReverse = function(input) {
  return(rev(strsplit(input, NULL)[[1]]))
}
