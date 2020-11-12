#Name : Ching Cheng Kang
#TP: TP051436

##Function Base!






#See what can we do ?
#plot(datas)
summary(datas)
cat("\014") ## Control L ...

#TESTING AREA
ggplot(data = datas, aes(x = month, y = visib)) +
  geom_bar(colour = "green", stat = 'identity')
plot(datas$temp, data$humid)  # Categorical variable

day = datas[datas$time >= "2013-01-02 00:00:00" &
              datas$time <= "2013-01-02 24:00:00", ]


any(is.na(datas))
table(is.na(datas))
colSums(is.na(datas))
nrow(datas)
View(datas)

#Wind Speed
##by count
a = ggplot(data = JFK, aes(wind_speed)) +
  geom_histogram(binwidth = 5,col = "white",fill = "blue") +
  labs(title = "Count of Wind Speed for JFK ",
       x = "Wind Speed", y = "Count")
b = ggplot(data = LGA, aes(wind_speed)) +
  geom_histogram(binwidth = 5,col = "white",fill = "green") +
  labs(title = "Count of Wind Speed for LGA ",
       x = "Wind Speed", y = "Count")
JFKvsLGA = marrangeGrob(list(a, b),nrow = 1,ncol = 2,top = "JFK vs LGA")

c = ggplot(data = datas, aes(wind_speed)) +
  geom_histogram(binwidth = 5,col = "white",fill = "blue") +
  labs(title = "Average Wind Speed for 2 air port",
       x = "Wind Speed", y = "Count")

##by month
a = ggplot(data = JFK, aes(x = month, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Yearly of Wind Speed for JFK ",
       x = "Month", y = "Wind Speed")


b = ggplot(data = LGA, aes(x = month, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Yearly of Wind Speed for LGA ",
       x = "Month", y = "Wind Speed")

JFKvsLGA = marrangeGrob(list(a, b), nrow = 1,ncol = 2,top = "JFK vs LGA")

c = ggplot(data = datas, aes(x = month, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Yearly of Wind Speed for Both Airport ",
       x = "Month", y = "Wind Speed")

##By Season
w = ggplot(data = winter, aes(x = time, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Winter Wind Speed for Both Airport ",
       x = "Time", y = "Wind Speed")

f = ggplot(data = fall, aes(x = time, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Fall Wind Speed for Both Airport ",
       x = "Time", y = "Wind Speed")

summary(fall)


#how much wind is average


##which month have more and less
ggplot(data = datas, aes(x = month, y = wind_speed)) + geom_bar(stat = 'identity')
ggplot(data = datas, aes(x = month, y = wind_gust)) + geom_bar(stat = 'identity')

ggplot(data = datas, aes(wind_gust)) + geom_histogram(bins = 30) +
  geom_freqpoly(bins = 40)

##
ggplot(data = datas, aes(x = month, y = temp)) + geom_bar(stat = 'identity')
ggplot(data = datas, aes(x = hour, y = precip)) + geom_bar(stat = 'identity')

##
ggplot(datas, aes(x = wind_speed, y = wind_gust)) +
  geom_point() +
  labs(title = "The co-variation between Wind Speed and Wind Gust",
       x = "Wind Speed", y = "Wind Gust")


##
ggplot(data = datas, aes(x = month, y = temp)) + geom_line() +
  labs(title = "Hourly Temperature for Year 2013",
       x = "Time", y = "Temperature(F)")

ggplot(data = datas, aes(x = time, y = temp)) + geom_line() +
  labs(title = "Hourly Temperature for Year 2013",
       x = "Time", y = "Temperature(F)")


## Get only one day and only JFK
test = datas[datas$time >= "2013-01-02 00:00:00" &
               datas$time <= "2013-01-02 24:00:00", ]

a = ggplot(data = JFK, aes(x = time, y = temp)) + geom_line() +
  labs(title = "Hourly Temperature for 2013-01-02 for JFK",
       x = "Time", y = "Temperature(F)")

b = ggplot(data = LGA, aes(x = time, y = temp)) + geom_line() +
  labs(title = "Hourly Temperature for 2013-01-02 for LGA",
       x = "Time", y = "Temperature(F)")

JFKvsLGA = marrangeGrob(list(a, b), nrow = 1, ncol = 2)
