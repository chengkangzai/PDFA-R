#Mary Ting
##Q1

vector1 = c(1:5)
vector2 = vector()
for (i in 1:5) {
  vector2 = c(vector2, i)
}

##Q2
x = sample(1:50, replace = TRUE)
min(x)
max(x)
mean(x)
sum(x)
length(x)

## Q3
mat = matrix(1:12, nrow = 3, ncol = 4)

##Q4
nrow(mat)
sum(mat[, 2])
mat[3, 2] = 100
rbind(mat, sample(50:100, size = 4))
sum(mat)

## Q5
mylist = list(mat,x)

## Q6
myarray = array(dim = c(3,5),data =sample(1:50))
myarray[1,3]


## Q7
weight= sample(x = 40:100,size = 10)
age= sample(x = 1:70,size = 10) 
gender= sample(x = c("MALE","FEMALE"),size = 10,replace = TRUE)

dat1=data.frame(weight=weight,age=age,gender=gender)

##Give up this
#factor(dat1,ordered = TRUE,exclude = weight)

for (i in dat1) {
  print(i["gender"])
}







