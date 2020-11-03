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

csv = read.csv(file = "http://www.jaredlander.com/data/TomatoFirst.csv", header = TRUE)
nrow(csv)
ncol(csv)
min(csv["Sweet"])

Q3sum = csv[(csv$Source == "Whole Foods"), ]
sum(Q3sum[, 3])

install.packages("readxl") ## excel
library("readxl")
download.file(
  url = "http://www.jaredlander.com/data/ExcelExample.xlsx",
  destfile = "03112020/ExcelExample.xlsx",
  quiet = TRUE,
  mode = "wb"
)
excelDate = read_excel(path = "03112020/ExcelExample.xlsx", sheet = 3)

download.file(
  url = "http://www.jaredlander.com/data/diamonds.db",
  destfile = "03112020/diamonds.db",
  quiet = TRUE,
  mode = "wb"
)
install.packages("RSQLite")
library("RSQLite")

dbCon = dbConnect(drv = dbDriver("SQLite"), dbname = '03112020/diamonds.db')
#Then list the tables in this dataset and the fields in each table.
listOfTable = dbListTables(dbCon)
listOfTable
#b. Retrieve all the elements of all the rows of diamonds table.
fieldsinDiamonds = dbListFields(dbCon, listOfTable[2])
fieldsinDiamonds
diamondsData = dbGetQuery(dbCon, statement = "Select * from diamonds")
diamondsData
dbDisconnect(dbCon)

library("ggplot2")
data("economics", package = "ggplot2")
ggplot(data = economics, aes(x = date, y = pop)) + geom_line()
install.packages("dplyr")
library("dplyr")
data = filter(economics, date > "2005-01-01")
ggplot(data = data, aes(x = date, y = pop)) + geom_line()

data("diamonds", package = "ggplot2")
ggplot(data = diamonds, aes(cut)) + geom_bar()
ggplot(data = diamonds, aes(carat )) +geom_histogram(bins=30)+ geom_freqpoly(bins=40)
ggplot(data = diamonds, aes(carat)) + geom_histogram(aes(group=clarity,bins=40))
ggplot(data = diamonds, aes(x=carat,y=price)) + geom_jitter()
ggplot(data = diamonds, aes(x=carat,y=price)) + geom_point(aes(group=color))
ggplot(data = diamonds, aes(carat)) + geom_boxplot(aes(group=cut))

#a. Plot a bar chart for quality of the cut (cut).
ggplot(diamonds,aes(cut))+geom_bar()
#b. Plot a histogram and frequency polygon for weight of the diamond (carat).
ggplot(diamonds,aes(carat))+ geom_histogram(bins=40)+geom_freqpoly(bins=40)
#c. Plot a histogram for weight of the diamond (carat) grouped by diamond
#clarity (clarity).
ggplot(diamonds,aes(carat))+geom_histogram(aes(group=clarity),bins=40)
#d. Plot a scatterplot to display values for weight of the diamond (carat) and the price (price).
ggplot(diamonds,aes(carat,price))+geom_jitter()
ggplot(diamonds,aes(carat,price))+geom_point()
#e. Plot a scatterplot to display values for weight of the diamond (carat) and the price (price)
#grouped by diamond color (color).
ggplot(diamonds,aes(carat,price))+geom_point(aes(group=color))
#f. Plot a boxplot for weight of the diamond (carat) grouped by the quality of the cut (cut).
ggplot(diamonds,aes(carat))+geom_boxplot(aes(group=cut))
