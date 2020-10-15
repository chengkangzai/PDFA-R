install.packages("ggplot2")
installed.packages()
library("ggplot2") ##load
library()
search("ggplot2")
detach("ggplot2")
remove.packages("ggplot2")
rm()#remove variable
readline(prompt = "Hello") #Read user input
summary(i)#descriptive vector
str(v1)
##Type of Data
character()
integer()
numeric()
logical()
##Data Structure
list() ## 1 dimension
vector() ## SAME type only  ## 1 dimension ## c()
matrix() ## SAME type only  ## 1/2 dimension
data.frame() ## 2 dimension
array() ## SAME type only ## more then 1 dimension

## Sample vs Run IF
##https://stackoverflow.com/questions/26978281/difference-between-runif-and-sample-in-r

##Create a Vector
c("", " ", "   ")
sample(1:10)
seq(1:10)
sequence(1:10)
runif(1:10)
rep(1:10)
vector(mode = "character", length = 5)

##Create a Matrix
matrix(data = sample(1:10))

##Create a Array
dim(as.array(letters))
array(1:3, c(2, 4))

##Create a List
list(1:10)

##Create a Data Frame
data.frame(1:10)
cbind() ## Add new column

## Able to use $ to access column
list()
data.frame()

matrix(1:24, 4, 6)

##Control Structure
if (condition) {
  
}
ifelse()
switch (object,
        case = action)

##Iterative Control Structure
for (variable in vector) {
  
}
while (condition) {
  
}

switch (as.character(readline(prompt = "1,2,?")),
        "1" = message(10),
        "2" = message(100),
        message(0))
tempHolder=1
for (i in 1:10) {
  tempHolder=tempHolder*i
}


foo = function(a1){
  return(a1)
}

x=1:4
x>2
v1=c(4,75,8,1,40,16,22,89,31,98)
c(9,8,10,8,9)

test = 20.01
as.integer(test)
ceiling(test)
remove(test)
length(v1)

genrate = function(){
  return(sample(1:10,replace = TRUE))
}

calulcate = function(input){
  number = 0
  for (element in input) {
    if (as.integer(element) <5) {
      number =number +as.integer(element)
    }
  }
  return(number)
}


