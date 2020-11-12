#Name : Ching Cheng Kang
#TP: TP051436

#pre processing
datas = read.delim(
  "https://raw.githubusercontent.com/chengkangzai/PDFA-R/master/assignment/data.csv",
  sep = ",",
  header = TRUE
)
datas$time = as.POSIXct(datas[, 15], format = "%d/%m/%Y %H:%M")

#Preload Packages with pacman
install.packages(pacman)
pacman::p_load(pacman, ggplot2, gridExtra)
pacman::p_unload(all)

#See what can we do ?
plot(datas)
summary(datas)
cat("\014") ## Control L ...

#TESTING AREA
ggplot(data = datas, aes(x = month, y = visib)) +
  geom_bar(colour = "green", stat = 'identity')
plot(datas$temp, data$humid)  # Categorical variable

any(is.na(datas))
table(is.na(datas))
colSums(is.na(datas))
nrow(datas)
View(datas)

#how much wind is average
ggplot(data = datas, aes(wind_speed)) +
  geom_histogram(binwidth = 5,
                 col = "white",
                 fill = "blue")
ggplot(data = datas, aes(x = month, y = wind_speed)) + geom_bar(stat = 'identity')

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
JFK = test[1:47,]

LGA = test[8730:8754,]

a = ggplot(data = JFK, aes(x = time, y = temp)) + geom_line() +
  labs(title = "Hourly Temperature for 2013-01-02 for JFK",
       x = "Time", y = "Temperature(F)")

b = ggplot(data = LGA, aes(x = time, y = temp)) + geom_line() +
  labs(title = "Hourly Temperature for 2013-01-02 for LGA",
       x = "Time", y = "Temperature(F)")

JFKvsLGA = marrangeGrob(list(a, b), nrow = 1, ncol = 2)
