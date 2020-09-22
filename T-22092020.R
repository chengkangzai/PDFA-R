m1 = matrix()
v <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
a = matrix(v, nrow = 3, ncol = 3)

v1 = 1:10

row_names <- (c('ROW1', 'ROW2'))
col_names <- (c('COL1', 'COL2', 'COL3', 'COL4', 'COL5'))
m1 = matrix(v1, 2, 5)
m2 = matrix(v1,
            nrow = 2,
            ncol = 2,
            dimnames = list(c("row1", "row2"), c("col1", "col2")))
dimnames = list(c("row1", "row2"), c("col1", "col2", "col3", "col4", "col5"))

m2 = matrix(v1,
            nrow = 2,
            ncol = 5,
            dimnames = dimnames)
colnames(m1) = c("c1", "c2", "c3", "c4", "c5")
rownames(m1) = c('r1', 'r2')

v2 = 1:6
m2 = matrix(v2,
            nrow = 2,
            ncol = 3,
            dimnames = list(c("row1", "row2"), c("Col1", "Col2", "Col3")))

v1 = 1:6
m1 = matrix(v1, 2, 3)
m2 = matrix(10:15, 2, 3)
madd = m1 + m2
msub = m1 - m2
mmul = m1 * m2
mdiv = m1 / m2

m1 = matrix(, 2, 3)
m1[1,] = c(1, 2, 3)
m = matrix(1:20, 4, 5)
m[2, 5]
m[2, 3]
m[2,]
m[, 3]

m1 = matrix(1:6, 2, 3)
m2 = matrix(1:12, 4, 3)
m3 = rbind(m1, m2)
m3 = cbind(m1, m2)
max = max(m2)
min = min (m2)

m1 = matrix(1:8, 2, 4)
which(m1 == max(m1), arr.ind = TRUE)

m1 = matrix(1:6, 3, 3)
m2 = matrix(1:12, 3, 3)
a1 = array(c(m1, m2),
           dim = c(3, 3, 2),
           dimnames = list(
             c("row1", "row2", "row3"),
             c("Col1", "Col2", "Col3"),
             c("Mat1", "Mat2")
           )))

values = 1:24
dim(values) <- c(3, 4, 2)

df = data.frame()
serialNumber = 1:5
gender = list("M", "F")
amount = sample(1000:3000, 5, replace = TRUE)

df1 = data.frame(serialNumber, gender, amount)

no = 1:5
gender = c('M', 'M', 'F', 'M', 'F')
name = c('Ali', 'Bakar', 'Catherine', 'Donald', "Elaine")
amount = c(sample(1000:2000, 5, TRUE))
df = data.frame(no, name, gender, amount)
df["name"]
df[1:2, ]

age = c(1, 2, 3, 4, 50)
df = data.frame(df, age)

age = c(1, 2, 3, 4, 50)
df = cbind(df, age)
rbind(df,
      data.frame(
        no = 6,
        name = "loo",
        gender = "F",
        age = 1,
        amount = 1000
      ))
df["name"] = NULL

list("a", 1, c(1, 2), c(3, 4), list(sample(1:10, 5)))

v1 = rep(TRUE, 3)
v2 = seq(1, 10, 2)
sublist = list(1:10, c(TRUE, FALSE, TRUE), c("Apple", "Orange"))
mylist = list('a', 1, v1, v2, df, sublist)
names(mylist) = c('a', 'b', 'c', 'd', 'e', 'f')
mylist[3]
mylist['e']
