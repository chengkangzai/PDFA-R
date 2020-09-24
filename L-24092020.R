print("Hello")
message("Hi and Bye")
cat("Good afternoon", "Kalvin") # concatenation
print(paste("how are you", "today"))
message(cat("how are you", "today"))
message(paste("How are you", "Today"))

v1 = scan(n = 4) # only same type
str(v1)
v2 = scan(what = character())
str(v2)
value = as.integer(readline(prompt = "Please enter a value : "))

grade = scan(what = character(), n = 1)
#Asian Grading Scale
x = switch (
  grade,
  A = "Averange",
  B = "Below Average",
  C = "Cant Have Dinner",
  D = "Dont have home",
  F = "Find a new Family",
  "Not a Human"
)
print(x)


d1 = data.frame(no = 1:3, name = c("ali", "Tan", "Aaren"))
d1 = cbind(d1, c("Bukit Jali", "Sri Pertaling", "Klang"))
names(d1)[3] = "Address"
d1 = rbind(d1, data.frame(no = 3, name = "Khalid", Address = "Serdang"))

a1 = array(10:21, dim = c(3, 4))
d = array(10:21, dim = c(2, 2, 3))
a2 = matrix(c(d[, 1, 3], d[2, , 2]), 2, 2)
l1 = list(v1, 'A', c(TRUE, FALSE, TRUE), m1)

a = matrix(-3:5, 3, 3)
a = ifelse(a < 0, 0, a)
