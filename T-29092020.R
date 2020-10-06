##Q1
x = 1:5
maxNumber = 0
input = integer()

for (i in x) {
  print(paste("Number", i, ":"))
  
  input = scan(what = integer(), n = 1)
  if (maxNumber == 0) {
    maxNumber = input
  } else if (input > maxNumber) {
    maxNumber = input
  }
  
  if (i == max(x)) {
    message(c("Max number is ", maxNumber))
  }
}

## Q2

x = TRUE
temp = 0
input = integer()
while (x == TRUE) {
  input = as.integer(readline(prompt = "Please enter an integer :"))
  
  if (input <= 0) {
    x = FALSE
    message(c("Total (product) :", temp))
  } else if (temp == 0) {
    temp = input
  } else {
    temp = temp * input
  }
}

## Q3

##You have to manually trigger it .. select and run
print("Hi Can i know what language you liked tht most?")
print("Enter 1 for C, 2 for Java, and 3 for VB.Net")
input = as.character(scan(what = character(), n = 1))
x = switch (input,
            "1" = "You like C!",
            "2" = "You like Java!",
            "3" = "You like VB.net",
            "Maye you like PHP ?")
print(x)

##  Q4
print(c("Enter the n Number"))
nNum = scan(what = integer(), n = 1)
sum = 0
if (nNum <= 0) {
  print("Come again")
}

while (nNum > 0) {
  sum = sum + nNum
  nNum = nNum - 1
}

message(sum)

#Q5
repeat {
  sum = sum + nNum
  nNum = nNum - 1
  if (nNum <= 0) {
    break
  }
}

#Q6
url = "http://stat.ethz.ch/Teaching/Datasets/WBL/legierung.dat"

d.alloy = read.table(url, header = TRUE)

#Which curing temperatures did the scientist use? temp
number = c(integer())
breaking.class = d.alloy

breaking.class = cbind(breaking.class, c(1:36))
names(breaking.class)[5] = "Strength"

d.alloy[which(data$breaking == max(data$breaking)), 2]

# B
d.alloy$breakingLevel = ifelse(d.alloy$breaking > mean(d.alloy$breaking), "High", "Low")


#for (element in d.alloy["temp"]) {
#  avg = mean(element)
#  if (element > avg) {
#    message("Its higher")
#  }else{
#    message("Its lower")
#  }
#}

#C
temp = 0
for (element in d.alloy["breaking"]) {
  if (temp < 250) {
    message("break!")
  } else{
    temp = temp + element
  }
}

tempp = 0
i = 1
while (tempp < 250) {
  if (tempp < 250) {
    tempp = tempp + d.alloy["breaking"][1]
  }
  i = i + 1
}
message("Stop it !")