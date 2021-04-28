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
      remoteFileLink = "https://raw.githubusercontent.com/chengkangzai/PDFA-R/master/assignment/data.csv"
      datas <<-read.delim(remoteFileLink,sep = ",",header = TRUE)
    }
  }
  
  initExtraCol <<- function(){
    datas$time <<- as.POSIXct(datas$time_hour, format = "%d/%m/%Y %H:%M")
    datas$season <<- getSeason(datas$time)
    datas$temp_cel <<- (datas$temp - 32) / 1.8
    datas$dewp_cel <<- (datas$dewp - 32) / 1.8
    datas$precip_cm <<- datas$precip * 2.54
    datas$wind_speed <<- datas$wind_speed * 1.609344
    datas$wind_gust <<- datas$wind_gust * 1.609344
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
    #there
    pacman::p_load(pacman, ggplot2, gridExtra, dplyr, lubridate, climatol, plotrix)
  }
  
  destoryEnv <<- function() {
    pacman::p_unload(all)
  }
  
  clear <<- function(){
    par()
    plot(c(1,2,3),c("1","2","3"))
    dev.off(dev.list())
    cat("\014")
  }
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
JFK_wSpeed = ggplot(data = JFK, aes(wind_speed)) +
  geom_histogram(binwidth = 5,col = "white",fill = "cyan") +
  geom_freqpoly(bins = 30)+
  labs(title = "JFK ",
       x = "Wind Speed (Km/h)", y = "Count")

LGA_wSpeed = ggplot(data = LGA, aes(wind_speed)) +
  geom_histogram(binwidth = 5,col = "white",fill = "green") +
  geom_freqpoly(bins = 30)+
  labs(title = "LGA ",
       x = "Wind Speed (Km/h)", y = "Count")

JFKvsLGA = marrangeGrob(list(JFK_wSpeed, LGA_wSpeed), nrow = 1,ncol = 2,top = "Distribution of Wind Speed of both Airport")

ggsave(filename = "assignment/plot/01. Count of Wind Speed for JFK vs LGA.jpg",JFKvsLGA,dpi = 1000 ,width = ,height = 5)

ggplot(data = datas, aes(wind_speed)) +
  geom_histogram(binwidth = 5,col = "white",fill = "cyan") +
  geom_freqpoly(bins = 30)+
  labs(title = "Average Wind Speed for 2 air port",
       x = "Wind Speed (Km/h)", y = "Count")

clear()
printDetail(JFK$wind_speed)
printDetail(LGA$wind_speed)
printDetail(datas$wind_speed)

##by month
JFK_wSpeed = ggplot(data = JFK, aes(x = month, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "cyan", bins = 30) +
  labs(title = "JFK ",x = "Month", y = "Total Wind Speed (Km/h)")

LGA_wSpeed = ggplot(data = LGA, aes(x = month, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "cyan", bins = 30) +
  labs(title = "LGA ",x = "Month", y = "Total Wind Speed (Km/h)")

printDetail(filter(LGA,month==3)$wind_speed)

JFKvsLGA = marrangeGrob(list(JFK_wSpeed, LGA_wSpeed), nrow = 1,ncol = 2,top = "Yearly of Wind Speed for JFK and LGA")

ggsave(filename = "assignment/plot/02. Yearly of Wind Speed for JFK vs LGA.jpg",JFKvsLGA,dpi = 1000,width = 10,height = 5)

ggplot(data = datas, aes(x = month, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Yearly of Wind Speed for Both Airport ",
       x = "Month", y = "Wind Speed (Km/h)")



##By Season

w = ggplot(data = winter, aes(x = time, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "cyan", bins = 30) +
  labs(title = "Winter Wind Speed for Both Airport ",
       x = "Time", y = "Total Wind Speed  (Km/h)")

f = ggplot(data = fall, aes(x = time, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "cyan", bins = 30) +
  labs(title = "Fall Wind Speed for Both Airport ",
       x = "Time", y = "Total Wind Speed  (Km/h)")

su = ggplot(data = summer, aes(x = time, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "cyan", bins = 30) +
  labs(title = "Summer Wind Speed for Both Airport ",
       x = "Time", y = "Total Wind Speed  (Km/h)")

sp = ggplot(data = spring, aes(x = time, y = wind_speed)) +
  geom_histogram(stat = 'identity', fill = "cyan", bins = 30) +
  labs(title = "Spring Wind Speed for Both Airport ",
       x = "Time", y = "Total Wind Speed  (Km/h)")

seasons = marrangeGrob(list(w, f,su,sp), nrow = 2,ncol = 2,top = "Seasonal of Wind Speed for both Airport")

ggsave(filename = "assignment/plot/03. Seasonal of Wind Speed for JFK vs LGA.jpg",seasons,dpi = 1000,width = 10,height = 5)

printDetail(winter$wind_speed)
printDetail(fall$wind_speed)
printDetail(summer$wind_speed)
printDetail(spring$wind_speed)
clear()


# 
# Wind Gust ---------------------------------

##By Count
a = ggplot(data = JFK, aes(wind_gust)) +
  geom_histogram(binwidth = 5,col = "white",fill = "cyan") +
  geom_freqpoly(bins = 40)+
  labs(title = "Count of Wind Gust for JFK ",
       x = "Wind Gust (km/h)", y = "Count")
b = ggplot(data = LGA, aes(wind_gust)) +
  geom_histogram(binwidth = 5,col = "white",fill = "green") +
  geom_freqpoly(bins = 40)+
  labs(title = "Count of Wind Gust for LGA ",
       x = "Wind Gust (km/h)", y = "Count")

JFKvsLGA = marrangeGrob(list(a, b),nrow = 1,ncol = 2,top = "JFK vs LGA")

c = ggplot(data = datas, aes(wind_gust)) +
  geom_histogram(binwidth = 5,col = "white",fill = "blue") +
  labs(title = "Average Wind Gust for 2 air port",
       x = "Wind Gust (km/h)", y = "Count")

##by month
jfk_wGust = ggplot(data = JFK, aes(x = month, y = wind_gust)) +
  geom_jitter() + geom_smooth()+
  labs(title = "Yearly of Wind Gust for JFK ",
      x = "Month", y = "Wind Gust (km/h)")

lga_wGust = ggplot(data = LGA, aes(x = month, y = wind_gust)) +
  geom_jitter() + geom_smooth()+
  labs(title = "Yearly of Wind Gust for LGA ",
      x = "Month", y = "Wind Gust (km/h)")

JFKvsLGA = marrangeGrob(list(jfk_wGust, lga_wGust), nrow = 1,ncol = 2,top = "Yearly Wind Gust for both airports")

ggsave(filename = "assignment/plot/05. Yearly of Wind Gust for JFK vs LGA.jpg",JFKvsLGA,dpi = 1000,width = 10,height = 5)

printDetail(JFK$wind_gust)
printDetail(LGA$wind_gust)
clear()

c = ggplot(data = datas, aes(x = month, y = wind_gust)) +
  geom_histogram(stat = 'identity', fill = "blue", bins = 30) +
  labs(title = "Yearly of Wind Speed for Both Airport ",
       x = "Month", y = "Wind Gust (km/h)")


#Season
#here
w = ggplot(data = winter, aes(x = wind_speed, y = wind_gust)) +
  geom_jitter() + geom_smooth()+
labs(title = "Winter Wind Gust and Wind Gust for Both Airport ",
x = "Total Wind Speed (km/h)", y = "Total Wind Gust  (km/h)")

f = ggplot(data = fall, aes(x = wind_speed, y = wind_gust)) +
  geom_jitter() + geom_smooth()+
labs(title = "Fall Wind Gust and Wind Gust for Both Airport ",
x = "Total Wind Speed (km/h)", y = "Total Wind Gust  (km/h)")

su = ggplot(data = summer, aes(x = wind_speed, y = wind_gust)) +
  geom_jitter() + geom_smooth()+
labs(title = "Summer Wind Gust and Wind Gust for Both Airport ",
x = "Total Wind Speed (km/h)", y = "Total Wind Gust  (km/h)")

sp = ggplot(data = spring, aes(x = wind_speed, y = wind_gust)) +
  geom_jitter() + geom_smooth()+
labs(title = "Spring Wind Gust and Wind Gust for Both Airport ",
x = "Total Wind Speed (km/h)", y = "Total Wind Gust  (km/h)")

marrangeGrob(list(w, f,su,sp), nrow = 2,ncol = 2,top = "Relationship between Wind Speed and Wind Gust by Season")
par()
ggsave(filename = "assignment/plot/06. Relationship between Wind Speed and Wind Gust by Season.jpg",seasons,dpi = 1000,width = 10,height = 5)

# Wind Dir ---------------------------------
#By Count


#LGA
#there
dirbreak = seq(-12.25, 360, 22.5)
dirLevel =c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
index = winter$wind_dir > 347.25
LGA = LGA[complete.cases(LGA$wind_dir),]
LGA$wind_dir[index] = LGA$wind_dir[index] - 360 & !is.na(LGA$wind_dir[index])
LGA$wind_dir2 = cut(LGA$wind_dir,breaks = dirbreak,
                     labels = dirLevel,include.lowest = TRUE)
LGA$wind_speed2 = cut_interval(LGA$wind_speed,4)
LGAwind <<- as.data.frame.array(table(LGA$wind_speed2,LGA$wind_dir2))

jpeg("assignment/plot/07. Rose Wind Diagram for Wind Direction.jpg")
par()
rosavent(LGAwind,4,4,ang = -3 * pi / 16,margen = c(0, 0, 2, 0),
  col = rainbow(4, 0.5, 0.92, start = 0.1, end = 0.9),key = FALSE,
  main="LGA Airport 2013 wind-rose Plot")
dev.off()

par(mfrow=c(1,2))
jpeg("assignment/plot/08. Rose Wind Diagram for Win Direction")
jpeg("assignment/plot/test")
clear()
##Messy code with Wind direction ... lazy put in function anymore  ----
index = LGA_winter$wind_dir > 347.25
LGA_winter = LGA_winter[complete.cases(LGA_winter$wind_dir),]
LGA_winter$wind_dir[index] = LGA_winter$wind_dir[index] - 360 & !is.na(LGA_winter$wind_dir[index])
LGA_winter$wind_dir2 = cut(LGA_winter$wind_dir,breaks = dirbreak,
                       labels = dirLevel,include.lowest = TRUE)
LGA_winter$wind_speed2 = cut_interval(LGA_winter$wind_speed,4)
LGA_winter_Wind <<- as.data.frame.array(table(LGA_winter$wind_speed2,LGA_winter$wind_dir2))

par()
rosavent(LGA_winter_Wind,4,4,ang = -3 * pi / 16,margen = c(0, 0, 2, 0),
         col = rainbow(4, 0.5, 0.92, start = 0.1, end = 0.9),key = FALSE,
         main="LGA Airport 2013 wind-rose Plot during winter")
dev.off()
index = LGA_spring$wind_dir > 347.25
LGA_spring = LGA_spring[complete.cases(LGA_spring$wind_dir),]
LGA_spring$wind_dir[index] = LGA_spring$wind_dir[index] - 360 & !is.na(LGA_spring$wind_dir[index])
LGA_spring$wind_dir2 = cut(LGA_spring$wind_dir,breaks = dirbreak,
                           labels = dirLevel,include.lowest = TRUE)
LGA_spring$wind_speed2 = cut_interval(LGA_spring$wind_speed,4)
LGA_spring_Wind <<- as.data.frame.array(table(LGA_spring$wind_speed2,LGA_spring$wind_dir2))

par()
rosavent(LGA_spring_Wind,4,4,ang = -3 * pi / 16,margen = c(0, 0, 2, 0),
         col = rainbow(4, 0.5, 0.92, start = 0.1, end = 0.9),key = FALSE,
         main="LGA Airport 2013 wind-rose Plot during Spring")
dev.off()

#Summer 
index = LGA_summer$wind_dir > 347.25
LGA_summer = LGA_summer[complete.cases(LGA_summer$wind_dir),]
LGA_summer$wind_dir[index] = LGA_summer$wind_dir[index] - 360 & !is.na(LGA_summer$wind_dir[index])
LGA_summer$wind_dir2 = cut(LGA_summer$wind_dir,breaks = dirbreak,
                           labels = dirLevel,include.lowest = TRUE)
LGA_summer$wind_speed2 = cut_interval(LGA_summer$wind_speed,4)
LGA_summer_Wind <<- as.data.frame.array(table(LGA_summer$wind_speed2,LGA_summer$wind_dir2))

par()
rosavent(LGA_summer_Wind,4,4,ang = -3 * pi / 16,margen = c(0, 0, 2, 0),
         col = rainbow(4, 0.5, 0.92, start = 0.1, end = 0.9),key = FALSE,
         main="LGA Airport 2013 wind-rose Plot during Summer")
dev.off()


index = LGA_fall$wind_dir > 347.25
LGA_fall = LGA_fall[complete.cases(LGA_fall$wind_dir),]
LGA_fall$wind_dir[index] = LGA_fall$wind_dir[index] - 360 & !is.na(LGA_fall$wind_dir[index])
LGA_fall$wind_dir2 = cut(LGA_fall$wind_dir,breaks = dirbreak,
                           labels = dirLevel,include.lowest = TRUE)
LGA_fall$wind_speed2 = cut_interval(LGA_fall$wind_speed,4)
LGA_fall_Wind <<- as.data.frame.array(table(LGA_fall$wind_speed2,LGA_fall$wind_dir2))

par()
rosavent(LGA_fall_Wind,4,4,ang = -3 * pi / 16,margen = c(0, 0, 2, 0),
         col = rainbow(4, 0.5, 0.92, start = 0.1, end = 0.9),key = FALSE,
         main="LGA Airport 2013 wind-rose Plot during Fall")
# By Pie Chart
x = c(sum(LGAwind$N),sum(LGAwind$NNE),sum(LGAwind$NE),sum(LGAwind$ENE),sum(LGAwind$E),sum(LGAwind$ESE),sum(LGAwind$SE),sum(LGAwind$SSE),sum(LGAwind$S),sum(LGAwind$SSW),sum(LGAwind$SW),sum(LGAwind$WSW),sum(LGAwind$W),sum(LGAwind$WNW),sum(LGAwind$NW),sum(LGAwind$NNW))

jpeg("assignment/plot/09. Pie Chart for Wind Direction.jpg")
par()
pie(x,dirLevel,main = "Distribution of Wind Direction" ,col = rainbow(length(x)))
legend("topright", dirLevel, cex = 0.8,fill = rainbow(length(x)))
dev.off()
clear()

pie3D(x,labels=dirLevel,main = "Distribution of Wind Direction",explode = 0.1)

# Temp ---------------------------------
#TEmp with Airport

plot(datas$temp_cel,
     xlab = "Count",
     ylab = "Temp (C)",
     main = "Overall Count of Temperature in Celsius",
     type = 'o')

jpeg("assignment/plot/10. Count of Celsius for both Airport.jpg",width = 1000,height = 500)
par(mfrow=c(1,2))

#one weird TEmperature, could be machine error
plot(JFK$temp_cel,
     xlab = "Count",
     ylab = "Temperature (°C)",
     main = "JFK Count of Temperature in Celsius",
     type = 'o')

plot(LGA$temp_cel,
     xlab = "Count",
     ylab = "Temperature (°C)",
     main = "LGA Count of Temperature in Celsius",
     type = 'o')
dev.off()
clear()

#Yearly
#here
monthlyTemp=ggplot(data = LGA, aes(x = month, y = temp_cel))  +
  geom_jitter() + stat_summary(fun=mean, colour="red", geom="line", size = 2)+
  labs(title = "Yearly Temperature for LGA Airports",
       x="Month",y="Temperature (°C)")

ggsave(filename = "assignment/plot/11. Yearly of Temperature for LGA.jpg",monthlyTemp,dpi = 1000,width = 10,height = 5)

jpeg("assignment/plot/12. Pie Chart for Temperature for season.jpg",width = 500,height = 500)
x = c(sum(winter$temp_cel),sum(fall$temp_cel),sum(summer$temp_cel),sum(spring$temp_cel))
labels = c("Winter","Fall","Summer","Spring")
par()
##Verify the Data Intergrity
pie(x,labels,main = "Total sum of Tempreature for every season in both Airports")
dev.off()


#day
wi_temp_hour = ggplot(data = LGA_winter, aes(x = hour, y = temp_cel)) +
  geom_jitter() + stat_summary(fun = mean,colour = "blue",geom = "line",size = 2) +
  labs(title = "Hourly Temperature for Winter",x = "Hour", y = "Temperature (°C)")

fa_temp_hour = ggplot(data = LGA_fall, aes(x = hour, y = temp_cel)) +
  geom_jitter() + stat_summary(fun = mean,colour = "blue",geom = "line",size = 2) +
  labs(title = "Hourly Temperature for Fall",x = "Hour", y = "Temperature (°C)")

su_temp_hour = ggplot(data = LGA_summer, aes(x = hour, y = temp_cel)) +
  geom_jitter() + stat_summary(fun = mean,colour = "blue",geom = "line",size = 2) +
  labs(title = "Hourly Temperature for Summer",x = "Hour", y = "Temperature (°C)")

sp_temp_hour = ggplot(data = LGA_spring, aes(x = hour, y = temp_cel)) +
  geom_jitter() + stat_summary(fun = mean,colour = "blue",geom = "line",size = 2) +
  labs(title = "Hourly Temperature for Spring",x = "Hour", y = "Temperature (°C)")

seasons = marrangeGrob(list(wi_temp_hour,fa_temp_hour,su_temp_hour,sp_temp_hour),
          nrow = 2,ncol = 2,top = "Average Temperature for LGA Airports")

ggsave(filename = "assignment/plot/13. Average Temperature for LGA Airports.jpg",seasons,dpi = 1000,width = 10,height = 10)

  
clear()
# Temp vs Wind Speed ---------------------------------
##not used
w = ggplot(data = winter, aes(x = temp, y = wind_gust)) +
  geom_jitter()+stat_summary(fun=mean, colour="red", geom="line", size = 2)+
  labs(title = "Winter Wind Gust for Both Airport ",
       x = "Temperature (°C)", y = "Total Wind Gust  (Km/h)")

f = ggplot(data = fall, aes(x = temp, y = wind_gust)) +
  geom_jitter()+stat_summary(fun=mean, colour="red", geom="line", size = 2)+
  labs(title = "Fall Wind Gust for Both Airport ",
       x = "Temperature (°C)", y = "Total Wind Gust  (Km/h)")

su = ggplot(data = summer, aes(x = temp, y = wind_gust)) +
  geom_jitter()+stat_summary(fun=mean, colour="red", geom="line", size = 2)+
  labs(title = "Summer Wind Gust for Both Airport ",
       x = "Temperature (°C)", y = "Total Wind Gust  (Km/h)")

sp = ggplot(data = spring, aes(x = temp, y = wind_gust)) +
  geom_jitter()+stat_summary(fun=mean, colour="red", geom="line", size = 2)+
  labs(title = "Spring Wind Gust for Both Airport ",
       x = "Temperature (°C)", y = "Total Wind Gust  (Km/h)")

seasons = marrangeGrob(list(w, f,su,sp), nrow = 2,ncol = 2,top = "Seasonal of Wind Gust and temperature for both Airport")
ggsave(filename = "assignment/plot/14. Seasonal of Wind Gust and temperature for both Airport.jpg",seasons,dpi = 1000,width = 10,height = 8)


# Dewp ---------------------------------
plot(datas$dewp_cel,
     xlab = "Count",
     ylab = "Temp (C)",
     main = "Overall Dewpoint of Temperature in Celsius",
     type = 'o')


#one weird TEmperature, could be machine error

JFKDew=ggplot(data = JFK, aes(x = month, y = dewp_cel))  +
  geom_jitter() + stat_summary(fun=mean, colour="red", geom="line", size = 2)+
  labs(title = "Yearly Dewpoint for JFK Airports",
       x="Month",y="Dewpoint (°C)")

LGADew=ggplot(data = LGA, aes(x = month, y = dewp_cel))  +
  geom_jitter() + stat_summary(fun=mean, colour="red", geom="line", size = 2)+
  labs(title = "Yearly Dewpoint for LGA Airports",
       x="Month",y="Dewpoint (°C)")

yearly = marrangeGrob(list(LGADew,JFKDew), nrow = 1,ncol = 2,
          top = "Yearly Dewpoint for both Airport")

ggsave(filename = "assignment/plot/15.Yearly Dewpoint for both Airport.jpg",
       yearly,dpi = 1000,width = 10,height = 7)

dev.off()
clear()

##Season

d = ggplot(data = LGA_spring, aes(x = hour, y = dewp_cel)) +
  geom_jitter() + stat_summary(fun = mean,colour = "red",geom = "line",size = 1)+
  labs(title = "Spring of Dewpoint for LGA ",x = "Hour", y = "Dewpoint (°C)")

c = ggplot(data = LGA_fall, aes(x = hour, y = dewp_cel)) +
  geom_jitter() + stat_summary(fun = mean,colour = "red",geom = "line",size = 1)+
  labs(title = "Fall of Dewpoint for LGA ", x = "Hour", y = "Dewpoint (°C)")

b = ggplot(data = LGA_summer, aes(x = hour, y = dewp_cel)) +
  geom_jitter() + stat_summary(fun = mean,colour = "red",geom = "line",size = 1)+
  labs(title = "Summer of Dewpoint for LGA ",x = "Hour", y = "Dewpoint (°C)")

a = ggplot(data = LGA_winter, aes(x = hour, y = dewp_cel)) +
  geom_jitter() + stat_summary(fun = mean,colour = "red",geom = "line",size = 1)+
    labs(title = "Winter of Dewpoint for LGA ", x = "Hour", y = "Dewpoint (°C)")

seasons = marrangeGrob(list(a,b,c,d), nrow = 2,ncol = 2,top = "Dewpoint of 4 season for LGA airports")

ggsave(filename = "assignment/plot/16.Hourly Dewpoint for LGA Airport.jpg",
       seasons,dpi = 1000,width = 10,height = 7)

printDetail(LGA_winter$dewp_cel)
printDetail(LGA_fall$dewp_cel)
printDetail(LGA_summer$dewp_cel)
printDetail(LGA_spring$dewp_cel)

humidvsDewpoint = ggplot(data = LGA, aes(x = humid, y = dewp_cel)) +
  geom_jitter() + geom_smooth()+
  labs(title = "Humid and Dew point for LGA Airports",
       x = "Humid (%)", y = "Dew point (°C)")

ggsave(filename = "assignment/plot/17.Humid and Dew point for LGA Airports.jpg",
       humidvsDewpoint,dpi = 1000,width = 10,height = 5)

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
# Humid --------------------------------

JFK_humid = ggplot(data = JFK, aes(humid)) +
  geom_histogram(binwidth = 5,col = "white",fill = "cyan") +  geom_freqpoly(bins = 30)+
  labs(title = "Count of Humid for JFK Airport ",x = "Humid ", y = "Count")

LGA_humid =ggplot(data = LGA, aes(humid)) +
  geom_histogram(binwidth = 5,col = "white",fill = "cyan") +  geom_freqpoly(bins = 30)+
  labs(title = "Count of Humid of LGA Airport ",x = "Humid ", y = "Count")

JFKvsLGA = marrangeGrob(list(JFK_humid,LGA_humid),nrow = 1,ncol = 2,top = "JFK vs LGA")

ggsave(filename = "assignment/plot/18. Count of Humid for JFK vs LGA.jpg",JFKvsLGA,dpi = 1000 ,width = 10 ,height = 5)

total = ggplot(data = datas, aes(x = hour, y = humid)) +
  geom_jitter() + geom_smooth()+
  labs(title = "Hourly of Humid for JFK and LGA",
       x = "Month", y = "Humidity")

ggsave(filename = "assignment/plot/19. Hourly of Humid for JFK vs LGA.jpg",total,dpi = 1000 ,width = 10,height = 8)


# precip --------------------------------
#Season 
x = c(sum(winter$precip),sum(fall$precip),sum(summer$precip),sum(spring$precip))
labels <- c("Winter","Fall","Summer","Spring")
pie(x,labels,main = "Total sum of Precipitation for every season in both Airports")


x = c(sum(winter$visib),sum(fall$visib),sum(summer$visib),sum(spring$visib))
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

