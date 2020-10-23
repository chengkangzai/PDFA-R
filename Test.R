x = list()

class(x)

x = c("25", "29", "33", "37")
match(29, x, nomatch =  -1)

array(data = NA,
      dim = length(data),
      dimnames = NULL)

class(c(c(1:10), c("ioruib", "option")))

rm(list = ls())

## "REPEAT"

library("crayon")

sample(x = 100:300,
       replace = TRUE,
       size = 10)

as.Date("2019-01-20")

x = c(1, rep(TRUE, 3), seq(45, 66, 7), FALSE)
length(x)


x = c(1, 49, 48, 47, 46, 45, 44, 43, 45, 52, 59, 66)
y = c(x[2], x[3], x[4], x[5], x[10], x[11], x[12])
y = c(x[2:5], x[10:12])

values <- 1:10
locations <- c("Seri Kembangan", "Petaling Jaya", "Bukit Jalil")
data <- c(values, locations)
class(data)
str(values)
values = 1:10
str(values)

as.logical(c(1, 0, 1, 2, 4, 0, 1, 0))

TransactionNo =
  Amount =
  data.frame(
    TransactionNo = c(1001, 1002, 1003, 1004, 1005),
    Amount = c(15, 27, 39, 51, 63)
  )

x = c(1, 49, 48, 47, 46, 45, 44, 43, 45, 52, 59, 66)
data = x > 44 & x < 50
x[7]


x = matrix(c(10, 6, 7, 8, 1),
           c(10, 0, 2, 2, 8),
           c(7, 7, 9, 8, 4),
           c(9, 7, 2, 2, 10))
x[3, 5]

x = c(25, 29, 33, 37)
x %in% 29

mat = matrix(
  data = c(
    c(10,10,7,9),
    c(6,0,7,7),
    c(7,2,9,2),
    c(8,2,8,2),
    c(1,8,4,10)
  )
  , nrow = 4, ncol = 5)

mat[2:3,3:5]

mat = matrix(
  data = c(
    c(25,94,30,99,12),
    c(76,88,79,73,58),
    c(53,11,100,41,71),
    c(57,49,56,70,16),
    c(62,8,13,47,68),
    c(95,26,28,27,42)
  )
  , nrow = 5, ncol = 6)

cbind(mat[2:4,3],mat[2:4,1])
