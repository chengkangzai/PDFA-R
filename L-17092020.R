v <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
a = matrix(v, nrow = 3, ncol = 3)
a

v1 = 100:120
v1[1:3]
m1 = matrix(1:10, nrow = 5, ncol = 2)
m2 = matrix(1:10, nrow = 5, ncol = 2, TRUE)
m3 = matrix(
  byrow = TRUE,
  ncol = 2,
  nrow = 3,
  data = 1:6
)
#data will be repeat
m3 = matrix(
  byrow = TRUE,
  ncol = 3,
  nrow = 3,
  data = 1:6
)

m2 + m3
rbind(m2, m3)
#cbind must use same rule
cbind(m2, m1)
#m2[row, column]
m2[2, 1:2]

m2[5:8]

vector1 <- c(2, 18, 30)
vector2 <- c(10, 14, 17, 13, 11, 15, 22, 11, 33)
data <- array(c(vector1, vector2), dim = c(3, 2, 2))

vector1 <- c(2,18,30)
vector2 <- c(10,14,17,13,11,15,22,11,33)
row_names <- (c('ROW1', 'ROW2', 'ROW3'))
col_names <- (c('COL1', 'COL2', 'COL3', 'COL4'))
matrix_names <- (c('Matrix1', 'Matrix2'))
data <-
  array(
    c(vector1, vector2),
    dim = c(3, 4, 2),
    dimnames =list(row_names, col_names, matrix_names)
  )

################################
#array can be matrix
a1=array(10:21,dim = c(3,4))
#dimension,3 layer
a1=array(10:21,dim = c(2,2,3))

a1[,1,3]
#pull the data from the array and form another matrix
a2=matrix(c(a1[,1,3],a1[2,,2]),2,2)
