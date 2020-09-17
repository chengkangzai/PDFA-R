weeks <- 4
hoursPerWeek = 40
hourlyRate = assign("hourlyRate", 50)
Week = 4L

bonuse = as.integer(1000)
tax = as.numeric(0.2)
grossSal = weeks * hoursPerWeek * hourlyRate
netSal = grossSal * (1 - tax)

name = "John Patrick"
class(name)
nchar(name)

hasInsurance = TRUE
class(hasInsurance)
is.logical(hasInsurance)

# how many number genrate ? min? max?
test1 = runif(1,0,30)
test2 = runif(1,0,30)
test3 = runif(1,0,30)
test4 = runif(1,0,30)
test5 = runif(1,0,30)

Exam1 = runif(1,0,70)
Exam2 = runif(1,0,70)
Exam3 = runif(1,0,70)
Exam4 = runif(1,0,70)
Exam5 = runif(1,0,70)

TotalMark1 = test1+Exam1
TotalMark2 = test2+Exam2
TotalMark3 = test3+Exam3
TotalMark4 = test4+Exam4
TotalMark5 = test5+Exam5

test1_Marks = sample(0:30,5,TRUE)
final_Marks = sample(0:70,5,TRUE)
total_Marks = test1_Marks + final_Marks

names(total_Marks) = c("Ali","Mohd", "Sarah", "Tan", "Muthu")
Pass = total_Marks>=50
print(Pass)

min(total_Marks)
max(total_Marks)
mean(total_Marks)
median(total_Marks)

data <-c(1,2,2,3,1,2,3,3,1,2,3,3,1)
