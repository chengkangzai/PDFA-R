search()
data() # show all dataset
install.packages("ggplot2")

data(package ="ggplot2")
data(presidential, package="ggplot2") #load specific dataset from specific table
View(presidential)
class(presidential)
str(presidential)
force(presidential)

install.packages("crayon") # Colored terminal