x = 1:5
maxNumber = 0
input = integer()

for (i in x) {
  print(paste("Number", i, ":"))
  
  input = scan(what = integer(), n = 1)
  if (maxNumber == 0) {
    maxNumber = input
  } else if (input > maxNumber) {
    maxNumber = input
  }
  
  if (i==max(x)) {
    message(c("Max number is ",maxNumber))
  }
}
