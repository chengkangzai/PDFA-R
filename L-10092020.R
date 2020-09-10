








4 / 2 + 3 * 1 - 2 * 2

assign('j', 4)

rm() #remove variable
ls() #list all variable
is.numeric()
i = 5L
#i = 5L
is.integer(i)
#STring
x = "welcome"
x = 4
nchar(x)
#Date
date = as.Date("2020-09-10")
class(date)
#Logical
k = TRUE
class(k)

a = 10
b = "c"
c = "coconut"
d = FALSE
e = 10.1
f = 1L
class(a)
class(b)
class(c)
class(d)
class(e)
class(f)
is.numeric(f)
is.integer(f)
as.character(e) #convert data type

#Vector
x = 1:10
x[2] = 10

g = 2:9
h = rep(2, 4)
i = rep(times = 5, x = FALSE)
j = seq(2, 8, 3) #(FROM,TO,INCREASE_BY)
k = seq(to = 100, from = 70, by = 9)
l = c("Marry", "Tan", "Chong", "Henry", "Abudullah")
str(l)
str(k)
str(l)
str(i)
m = c(h, i, 1, TRUE)
#m = c(h, i, 1, TRUE,l)
str(m)

marks = c(80, 90, 76, 34, 92, 88, 67, 89, 76, 100)
cat(min(marks))
cat(max(marks))
cat(mean(marks))
#Still have other more in base

message = "Welcome to PFDA"
substr(message, 4, 7)
strsplit(message, " ")
print(message)
message(message)
cat(message) #concatenate
paste(message, l) #put data togather
print(paste(message, l)) #print it out and get the result, problably not important

x = c("A", "B", "AB", "A", "B")
y = factor(c("A", "B", "AB", "A", "B"))

age = sample(18:25, 250, TRUE)#FROM18->25,250 quantity,can be repeated
country = c("Malyaisan", "Indonesia", "Chianese", "Central Asia", "Middle East")
nationality = sample(country, 250, TRUE)
catNat = factor(nationality)
table(catNat)
tapply(age, catNat, max)
tapply(age, catNat, min)
tapply(age, catNat, mean)
View(age)
View(nationality)