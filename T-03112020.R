data(AirPassengers, package = "datasets")
print(AirPassengers)
View(AirPassengers)


data("economics", package = "ggplot2")
write(
  c(12000, 7000, 9000, 6000, 8000),
  file = "data.txt",
  append = FALSE,
  sep = "\n"
)

mydata = scan(file = "data.txt", what = integer())
min(mydata)
max(mydata)
mean(mydata)

csv = read.csv(file = "http://www.jaredlander.com/data/TomatoFirst.csv",header = TRUE)
nrow(csv)
ncol(csv)
min(csv["Sweet"])

Q3sum = csv[(csv$Source == "Whole Foods"),]
sum(Q3sum[,3])

install.packages("readxl") ## excel
library("readxl")
download.file(url = "http://www.jaredlander.com/data/ExcelExample.xlsx",destfile = "ExcelExample.xlsx",quiet = TRUE,mode = "wb")
d3 = read_excel(path = "ExcelExample.xlsx", sheet = 3)

