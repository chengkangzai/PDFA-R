search()
data() # show all dataset
install.packages("ggplot2")

data(package = "ggplot2")
data(presidential, package = "ggplot2") #load specific dataset from specific table
View(presidential)
class(presidential)
str(presidential)
force(presidential)

install.packages("crayon") # Colored terminal
#last line must be empty
read.csv(file = "C:\...", header = TRUE)
workingDirectory = getwd() + "/08102020.txt"
getwd()
#scan "every space is a new word"
mydata = scan(file = "08102020.txt", what = character())
View(mydata)

readLines("08102020.txt")
read.delim("08102020.txt", sep = "\n", header = FALSE)
read.delim("08102020.txt", sep = "\t", header = FALSE)

d1 = read.delim("08102020-1.csv", sep = ",", header = TRUE)
View(d1)
d2 = read.csv("08102020-1.csv")
d3 = read.table("08102020-1.csv")

library("readxl")
install.packages("reader") ## big file
install.packages("readxl") ## excel

d3 = read_excel(path = "08102020-1.xlsx", sheet = 1)
##tibble new data structure, it work like data frame
