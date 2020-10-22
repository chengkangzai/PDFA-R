install.packages("ggplot2")
installed.packages()
library("ggplot2")

weight= sample(x = 40:100,size = 100,replace = TRUE)
age= sample(x = 1:70,size = 100,replace = TRUE) 
gender= sample(x = c("MALE","FEMALE"),size = 100,replace = TRUE)

dat1=data.frame(weight=weight,age=age,gender=gender)
ggplot(data = dat1, aes(x=weight,y=age))+geom_point()

data(mpg,package="ggplot2")
View(mpg)
ggplot(data = mpg, aes(x=manufacturer,y=hwy))+geom_point()
ggplot(data = mpg, aes(x=manufacturer))+geom_bar()

data(diamonds,package="ggplot2")
ggplot(data = diamonds, aes(x=carat,y=price))+geom_line()
ggplot(data = diamonds, aes(x=carat,y=price))+geom_jitter()

data("economics",package="ggplot2")
ggplot(data = economics, aes(x=date,y=unemploy))+geom_line()
