init = function(){
  reading = sample(c('PASS', 'FAIL'), 100, TRUE)
  writing = sample(c('PASS', 'FAIL'), 100, TRUE)
  listening = sample(c('PASS', 'FAIL'), 100, TRUE)
  stdid = 1001:1100
  dat1 <<- data.frame(
    StudentID = stdid,
    Reading = reading,
    Writing = writing,
    Listening = listening
  )  
}

init()
rm(list=ls())

#explore
View(dat1)
summary(dat1)
str(dat1)
names(dat1)
row.names(dat1)
nrow(dat1)
ncol(dat1)


#Transformation
#change the content of the reading by replacing the FAIL to 0 and PASS to 1
dat1$Reading = ifelse(dat1$Reading == "PASS", 1, 0)
class(dat1$Reading)

e=dat1["Reading"]
#Change from data frame to vector
b = dat1[["Reading"]]
class(b)

pacman::p_load(ggplot2)
count = c(nrow(filter(dat1, Reading == "PASS")), nrow(filter(dat1, Writing == "PASS")), nrow(filter(dat1, Listening == "PASS")))
x = data.frame(Subject = c("Reading", "Writing", "Listening"),count = count)

ggplot(data = x, aes(x = Subject, y = count)) +  geom_bar(stat = 'identity')
  
hist(c(x$Reading,x$Writing,x$Listening))


read.delim(file = "review.txt",sep = "\n")
