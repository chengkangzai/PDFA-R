#Name : Ching Cheng Kang
#TP: TP051436

##Function Base!
##Initialzation... can understand as Contructor
init = function() {
  getSeason <<- function(input.date) {
    numeric.date = 100 * month(input.date) + day(input.date)
    cuts <-base::cut(numeric.date, breaks = c(0, 319, 0620, 0921, 1220, 1231))
    levels(cuts) = c("Winter", "Spring", "Summer", "Fall", "Winter")
    return(cuts)
  }

  printDetail <<- function(input) {
    message(c("Mean : ", mean(input,na.rm = TRUE)))
    message(c("Min : ", min(input,na.rm = TRUE)))
    message(c("Max : ", max(input,na.rm = TRUE)))
  }
  
  initdata <<- function() {
    if (!exists("datas")) {
      remoteFileLink = "https://firebasestorage.googleapis.com/v0/b/chengkangzai.appspot.com/o/data.csv?alt=media&token=1e46df0c-b1a3-4bb6-8353-61c66d114bff"
      datas <<-read.delim(remoteFileLink,sep = ",",header = TRUE)
    }
  }
  
  initExtraCol <<- function(){
    datas$time <<- as.POSIXct(datas$time_hour, format = "%d/%m/%Y %H:%M")
    datas$season <<- getSeason(datas$time)
    datas$temp_cel <<- (datas$temp - 32) / 1.8
    datas$dewp_cel <<- (datas$dewp - 32) / 1.8
    datas$precip_cm <<- datas$precip * 2.54
  }
  
  initVariable <<- function() {
    JFK <<- filter(datas, origin == "JFK")
    LGA <<- filter(datas, origin == "LGA")
    winter <<- filter(datas, season == "Winter")
    fall <<- filter(datas, season == "Fall")
    spring <<- filter(datas, season == "Spring")
    summer <<- filter(datas, season == "Summer")
  }
  
  initLGATemp <<- function(){
    #the reason why we choose LGA is because JFK have one sensor might malfunction to misleading data
    LGA_winter <<- filter(winter, origin == "LGA")
    LGA_fall <<- filter(fall, origin == "LGA")
    LGA_spring <<- filter(spring, origin == "LGA")
    LGA_summer <<- filter(summer, origin == "LGA")
  }
  
  initDayValue <<- function(){
    winter_day<<- LGA[LGA$time >= "2013-01-20 00:00:00" & LGA$time <= "2013-01-20 24:00:00", ]
    fall_day<<- LGA[LGA$time >= "2013-10-20 00:00:00" & LGA$time <= "2013-10-20 24:00:00", ]
    summer_day<<-LGA[LGA$time >= "2013-07-20 00:00:00" & LGA$time <= "2013-07-20 24:00:00", ]
    spring_day<<-LGA[LGA$time >= "2013-04-20 00:00:00" & LGA$time <= "2013-04-20 24:00:00", ]
  }
  
  initEnv <<- function() {
    pacman::p_load(pacman, ggplot2, gridExtra, dplyr, lubridate,climatol)
  }
  
  destoryEnv <<- function() {
    pacman::p_unload(all)
  }
  
  clear <<- function(){
    dev.off(dev.list())
    cat("\014")
  }
  
  rm(list=ls())
  initEnv()
  initdata()
  initExtraCol()
  initVariable()
  initLGATemp()
  initDayValue()
}

init()

clear()
rm(list=ls())
plot(datas)
summary(datas)
 
#TESTING AREA ------
ggplot(data = datas, aes(x = month, y = visib)) +
  geom_bar(colour = "green", stat = 'identity')
plot(datas$temp, data$humid)  # Categorical variable

day = datas[datas$time >= "2013-01-02 00:00:00" &
              datas$time <= "2013-01-02 24:00:00", ]

dirbreak <- seq(-12.25,360,22.5)
dirdengji <-c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
index <- LGA$wind_dir > 347.25
LGA <- LGA[complete.cases(LGA$wind_dir),]
LGA$wind_dir[index] <- LGA$wind_dir[index] - 360
LGA$wind_dir2 <- cut(LGA$wind_dir,breaks = dirbreak,
                                labels = dirdengji,include.lowest = TRUE)
LGA$wind_speed2 <- cut_interval(LGA$wind_speed,4)
LGAwind <- as.data.frame.array(table(LGA$wind_speed2,LGA$wind_dir2))


par(family = "STKaiti")
rosavent(LGAwind, 4, 4, ang=-3*pi/16, margen=c(0,0,2,0),
         col=rainbow(4,0.5,0.92,start=0.1,end=0.9),key = FALSE)

glimpse(weather,60)

pacman::p_load(nycflights13)

clear()
any(is.na(LGA$wind_dir))
table(is.na(LGA$wind_dir))
colSums(is.na(datas))
nrow(datas)
View(datas)

#END TESTING

temp =summer$temp_cel
hist(temp)
printDetail(temp)

# Wind Speed ---------------------------------
a = ggplot(data = JFK, aes(wind_speed)) +
  geom_histogram(binwidth = 5,col = "white",fill = "cyan") +
  geom_freqpoly(bins = 30)+
  labs(title = "Count of Wind Speed for JFK ",
       x = "Wind Speed", y = "Count")
b = ggplot(data = LGA, aes(wind_speed)) +
  geom_histogram(binwidth = 5,col = "white",fill = "green") +
  geom_freqpoly(bins = 30)+
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

w = ggplot(data = winter, aes(x = time, y = wind_speed)) +  geom_line() +
    labs(title = "Winter Wind Speed for Both Airport ",
       x = "Time", y = "Wind Speed")

f = ggplot(data = fall, aes(x = time, y = wind_speed)) +  geom_line() +
  labs(title = "Fall Wind Speed for Both Airport ",
       x = "Time", y = "Wind Speed")

su = ggplot(data = summer, aes(x = time, y = wind_speed)) + geom_line() +
  labs(title = "Summer Wind Speed for Both Airport ",
       x = "Time", y = "Wind Speed")

sp = ggplot(data = spring, aes(x = time, y = wind_speed)) + geom_line() +
  labs(title = "Spring Wind Speed for Both Airport ",
       x = "Time", y = "Wind Speed")
printDetail(winter$wind_speed)
printDetail(fall$wind_speed)
printDetail(summer$wind_speed)
printDetail(spring$wind_speed)

seasons = marrangeGrob(list(w, f,su,sp), nrow = 2,ncol = 2,top = "4 seson")

# 
# Wind Gust ---------------------------------

##By Count
a = ggplot(data = JFK, aes(wind_gust)) +
  geom_histogram(binwidth = 5,col = "white",fill = "cyan") +
  geom_freqpoly(bins = 40)+
  labs(title = "Count of Wind Gust for JFK ",
       x = "Wind Gust", y = "Count")
b = ggplot(data = LGA, aes(wind_gust)) +
  geom_histogram(binwidth = 5,col = "white",fill = "green") +
  geom_freqpoly(bins = 40)+
  labs(title = "Count of Wind Gust for LGA ",
       x = "Wind Gust", y = "Count")
JFKvsLGA = marrangeGrob(list(a, b),nrow = 1,ncol = 2,top = "JFK vs LGA")

c = ggplot(data = datas, aes(wind_gust)) +
  geom_histogram(binwidth = 5,col = "white",fill = "blue") +
  labs(title = "Average Wind Gust for 2 air port",
       x = "Wind Gust", y = "Count")

##by month
a = ggplot(data = JFK, aes(x = month, y = wind_gust)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Yearly of Wind Gust for JFK ",
       x = "Month", y = "Wind Gust")


b = ggplot(data = LGA, aes(x = month, y = wind_gust)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Yearly of Wind Gust for LGA ",
       x = "Month", y = "Wind Gust")

JFKvsLGA = marrangeGrob(list(a, b), nrow = 1,ncol = 2,top = "JFK vs LGA")

c = ggplot(data = datas, aes(x = month, y = wind_gust)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Yearly of Wind Speed for Both Airport ",
       x = "Month", y = "Wind Gust")


# Wind Dir ---------------------------------
#By Count
a = ggplot(data = JFK, aes(wind_dir)) +
  geom_histogram(binwidth = 5,col = "white",fill = "blue") +
  labs(title = "Count of Wind Direction for JFK ",
       x = "Wind Gust", y = "Count")
b = ggplot(data = LGA, aes(wind_dir)) +
  geom_histogram(binwidth = 5,col = "white",fill = "green") +
  labs(title = "Count of Wind Gust for LGA ",
       x = "Wind Gust", y = "Count")

JFKvsLGA = marrangeGrob(list(a, b),nrow = 1,ncol = 2,top = "JFK vs LGA")

a = ggplot(data = JFK, aes(x = hour, y = wind_dir)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Yearly of Wind Gust for JFK ",
       x = "Month", y = "Wind Gust")


b = ggplot(data = LGA, aes(x = hour, y = wind_dir)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Yearly of Wind Gust for LGA ",
       x = "Month", y = "Wind Gust")

JFKvsLGA = marrangeGrob(list(a, b), nrow = 1,ncol = 2,top = "JFK vs LGA")


  dirbreak = seq(-12.25,360,22.5)
  dirdengji =c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
  index = LGA$wind_dir > 347.25
  LGA = LGA[complete.cases(LGA$wind_dir),]
  LGA$wind_dir[index] = LGA$wind_dir[index] - 360 & !is.na(LGA$wind_dir[index])
  LGA$wind_dir2 = cut(LGA$wind_dir,breaks = dirbreak,
                       labels = dirdengji,include.lowest = TRUE)
  LGA$wind_speed2 = cut_interval(LGA$wind_speed,4)
  LGAwind <<- as.data.frame.array(table(LGA$wind_speed2,LGA$wind_dir2))
  


par(rosavent(LGAwind,4,4,ang = -3 * pi / 16,margen = c(0, 0, 2, 0),
  col = rainbow(4, 0.5, 0.92, start = 0.1, end = 0.9),key = FALSE,main="LGA Airport 2013 wind-rose Plot"))

# Temp ---------------------------------
#TEmp with Airport
plot(datas$temp_cel,
     xlab = "Count",
     ylab = "Temp (C)",
     main = "Overall Count of Temperature in Celsius",
     type = 'o')

par(mfrow=c(1,2))

#one weird TEmperature, could be machine error
plot(JFK$temp_cel,
     xlab = "Count",
     ylab = "Temp (C)",
     main = "JFK Count of Temperature in Celsius",
     type = 'o')

plot(LGA$temp_cel,
     xlab = "Count",
     ylab = "Temp (C)",
     main = "LGA Count of Temperature in Celsius",
     type = 'o')

plot(winter$temp_cel,type='o')
clear()

#Yearly
ggplot(data = LGA, aes(x = month, y = temp_cel))  +
  geom_jitter() + stat_summary(fun=mean, colour="red", geom="line", size = 2)
  labs(title = "Yearly of Temperature for LGA ",
       x="Month",y="Temperature (C)")

#Season
a = ggplot(data = LGA_spring, aes(x = month, y = temp_cel)) +
    geom_jitter() + stat_summary(
      fun = mean,
      colour = "yellow",
      geom = "line",
      size = 2
    )+
  labs(title = "spring of Temperature for LGA ",
       x = "Month", y = "Temperature")
  
b = ggplot(data = LGA_summer, aes(x = month, y = temp_cel)) +
    geom_jitter() + stat_summary(
      fun = mean,
      colour = "red",
      geom = "line",
      size = 2
    )+
  labs(title = "summer of Temperature for LGA ",
       x = "Month", y = "Temperature")
  
c = ggplot(data = LGA_fall, aes(x = month, y = temp_cel)) +
    geom_jitter() + stat_summary(
      fun = mean,
      colour = "orange",
      geom = "line",
      size = 2
    )+
  labs(title = "fall of Temperature for LGA ",
       x = "Month", y = "Temperature")
  
d = ggplot(data = LGA_winter, aes(x = month, y = temp_cel)) +
    geom_jitter() + stat_summary(
      fun = mean,
      colour = "blue",
      geom = "line",
      size = 2
    )+
  labs(title = "winter of Temperature for LGA ",
       x = "Month", y = "Temperature")
  
  seasons = marrangeGrob(list(a,b,c,d), nrow = 2,ncol = 2,top = "4 season")
  
  
x = c(sum(winter$temp_cel),sum(fall$temp_cel),sum(summer$temp_cel),sum(spring$temp_cel))
labels <- c("Winter","Fall","Summer","Spring")
  ##Verify the Data Intergrity
test = pie(x,labels,main = "Total sum of Tempreature for every season in both Airports")
  
#day
a = ggplot(data = winter_day, aes(x = hour, y = temp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "blue",
    geom = "line",
    size = 2
  ) +  labs(title = "Hourly Temperature for 2013-01-20 (Winter)",
            x = "Hour", y = "Temperature")

b = ggplot(data = fall_day, aes(x = hour, y = temp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "blue",
    geom = "line",
    size = 2
  ) +  labs(title = "Hourly Temperature for 2013-10-20 (Fall)",
            x = "Hour", y = "Temperature")

c = ggplot(data = summer_day, aes(x = hour, y = temp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "blue",
    geom = "line",
    size = 2
  ) +  labs(title = "Hourly Temperature for 2013-07-20 (Summer)",
            x = "Hour", y = "Temperature")

d = ggplot(data = spring_day, aes(x = hour, y = temp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "blue",
    geom = "line",
    size = 2
  ) +  labs(title = "Hourly Temperature for 2013-07-20 (Spring)",
            x = "Hour", y = "Temperature")

seasons = marrangeGrob(list(a,b,c,d), nrow = 2,ncol = 2,top = "4 season")
  
clear()
# Dewp ---------------------------------
plot(datas$dewp_cel,
     xlab = "Count",
     ylab = "Temp (C)",
     main = "Overall Count of Temperature in Celsius",
     type = 'o')


#one weird TEmperature, could be machine error
par(mfrow=c(1,2))
plot(JFK$dewp_cel,
     xlab = "Count",
     ylab = "Temp (C)",
     main = "JFK Count of Temperature in Celsius",
     type = 'o')

plot(LGA$dewp_cel,
     xlab = "Count",
     ylab = "Temp (C)",
     main = "LGA Count of Temperature in Celsius",
     type = 'o')

clear()

##Season

a = ggplot(data = LGA_spring, aes(x = month, y = dewp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "yellow",
    geom = "line",
    size = 2
  )+
  labs(title = "spring of Temperature for LGA ",
       x = "Month", y = "Temperature")

b = ggplot(data = LGA_summer, aes(x = month, y = dewp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "red",
    geom = "line",
    size = 2
  )+
  labs(title = "summer of Temperature for LGA ",
       x = "Month", y = "Temperature")

c = ggplot(data = LGA_fall, aes(x = month, y = dewp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "orange",
    geom = "line",
    size = 2
  )+
  labs(title = "fall of Temperature for LGA ",
       x = "Month", y = "Temperature")

d = ggplot(data = LGA_winter, aes(x = month, y = dewp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "blue",
    geom = "line",
    size = 2
  )+
  labs(title = "winter of Temperature for LGA ",
       x = "Month", y = "Temperature")

seasons = marrangeGrob(list(a,b,c,d), nrow = 2,ncol = 2,top = "Temperature of 4 season")

#Day
a = ggplot(data = winter_day, aes(x = hour, y = dewp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "blue",
    geom = "line",
    size = 2
  ) +  labs(title = "Hourly Dewpoint for 2013-01-20 (Winter)",
            x = "Hour", y = "Dewpoint")

b = ggplot(data = fall_day, aes(x = hour, y = dewp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "blue",
    geom = "line",
    size = 2
  ) +  labs(title = "Hourly Dewpoint for 2013-10-20 (Fall)",
            x = "Hour", y = "Dewpoint")

c = ggplot(data = summer_day, aes(x = hour, y = dewp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "blue",
    geom = "line",
    size = 2
  ) +  labs(title = "Hourly Dewpoint for 2013-07-20 (Summer)",
            x = "Hour", y = "Dewpoint")

d = ggplot(data = spring_day, aes(x = hour, y = dewp_cel)) +
  geom_jitter() + stat_summary(
    fun = mean,
    colour = "blue",
    geom = "line",
    size = 2
  ) +  labs(title = "Hourly Dewpoint for 2013-07-20 (Spring)",
            x = "Hour", y = "Dewpoint")

seasons = marrangeGrob(list(a,b,c,d), nrow = 2,ncol = 2,top = "Dewpoint of 4 season")

#this will draw my attention to see the relationship between it
#Yearly
ggplot(data = LGA, aes(x = month, y = dewp_cel))  +
  geom_jitter(na.rm = TRUE) + geom_smooth(span = 0.3, method = "loess")+
  labs(title = "Yearly of Dewpoint for LGA ",
       x="Month",y="Dewpoint (C)")

# Temp vs Dewp ######################################
ggplot(data = LGA, aes(x = temp_cel, y = dewp_cel))  +
  geom_jitter(na.rm = TRUE) + 
  labs(title = " Dewpoint vs Temperature for LGA ",
       x = "Temperature (C)", y = "Dewpoint (C)")

#VS

ggplot(data = LGA, aes(x = temp_cel, y = dewp_cel))  +
  geom_jitter(na.rm = TRUE) + geom_smooth(span = 0.1, method = "loess") +
  labs(title = " Dewpoint vs Temperature for LGA ",
       x = "Temperature (C)", y = "Dewpoint (C)")
#By Day


# precip --------------------------------
#Season 
x = c(sum(winter$precip),sum(fall$precip),sum(summer$precip),sum(spring$precip))
labels <- c("Winter","Fall","Summer","Spring")
pie(x,labels,main = "Total sum of Precipitation for every season in both Airports")

#Yearly
#June marks highest, mean most probably be rain
ggplot(data = datas, aes(x=month,y=precip_cm)) +
  geom_histogram(stat = 'identity') +
  labs(title = "Count of precip for both Airports ",
       x = "Month", y = "Precipitation (cm)")

# Pressure ------------------------------
plot(datas$pressure,datas$wind_speed)
plot(datas$pressure,datas$temp)

# Visib ---------------------------------
ggplot(data = datas, aes(visib)) + geom_histogram(bins = 30) +
  geom_freqpoly(bins = 30)+
  labs(title = "Count of Wind Direction for JFK ",
       x = "Wind Gust", y = "Count")

