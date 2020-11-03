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
length(csv)


