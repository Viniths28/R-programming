setwd("C:/Users/vs0432/Documents/Rfolder/Lab4")
getwd()

library(lubridate)

 
# storing current date from system

stamp <- now()
class(stamp)

year(stamp)
year("2019-10-12")

x <- ("01/03/2019")
y  <- mdy(x)
wday(y)

minute(stamp)
month(stamp, label = TRUE)
month(stamp, label =TRUE, abbr = FALSE)
wday(stamp, label = TRUE, abbr = FALSE)
stamp-hours(2)-minutes(30)


# Time Interval

halloween <- ymd("2014-10-31") 
christmas <- ymd("2014-12-25") 
interval <- interval(halloween, christmas) 
interval
dweeks(2)
z <- interval / ddays(1)
z
dseconds(1)


# subtracting 1 billion Seconds from 1 epoch

origin <- ymd_hms("2001-09-09 01:46:40", tz="UTC") 
origin - seconds(10^9)


# Rounding Time

aprilmorning <- ymd_hms("2010-09-14 11:28:56")
round_date(aprilmorning,"day")

aprilaftrn <- ymd_hms("2010-09-14 14:25:59")
round_date(aprilaftrn,"day")

test <- ymd_hms("2010-12-16 12:00:00")
round_date(test,"month")

april20 <- ymd_hms("2010-08-07 11:33:29")
ceiling_date(april20, "month")-days(1)

ceiling_date(april20, "month")-months(5) 

x <- ymd_hms("2009-08-03 12:01:59.23")
round_date(x, ".5s")
round_date(x, "sec")
ceiling_date(x, "month")


# Sea Ice Extent
dataframe <- read.csv("NH_sea_ice_extent_2014-10-10.csv")
seaice <- dataframe

head(seaice)
class(seaice$date)
seaice$date <- dmy(seaice$date)
class(seaice$date)

# plot
library(ggplot2)
ggplot(seaice)+geom_line(aes(x=date, y=extent))

#creating variable
seaice$year <- year(seaice$date)
seaice$yearday <- yday(seaice$date)

ggplot(seaice)+geom_line(aes(x=yearday,y=extent))
ggplot(seaice)+geom_line(aes(x=yearday,y=extent,group=year))
ggplot(seaice)+geom_line(aes(x=yearday,y=extent,group=year,alpha=0.5))

ggplot(seaice)+
  geom_line(aes(x=yearday,y=extent,group=year,alpha=0.01,colour=year))

ggplot(data=seaice)+
  geom_line(aes(x=yearday,y=extent,group=year,colour=year))+
              scale_color_gradient(low = "red",high = "blue")+
  theme(legend.position ="right")

library(RColorBrewer)
palette <- brewer.pal(3,name="BrBG")

ggplot(seaice)+
  geom_line(aes(x=yearday,y=extent,group=year,colour=year),alpha=0.5)+
  scale_color_gradient(low=palette[1],high=palette[3])

palette <- brewer.pal(3,name="PiYG")
palette <- brewer.pal(3,name="PRGn")
palette <- brewer.pal(3,name="PuOr")
palette <- brewer.pal(3,name="RdGy")
palette <- brewer.pal(3,name="RdYLBu")
palette <- brewer.pal(3,name="RdYlGn")
palette <- brewer.pal(3,name="Spectral")



library(dplyr)
library(tidyverse)

seaice$month <- month(seaice$date)
class(seaice$month)
seaice_oct <- filter(seaice, month == 10) 

sea_ice_oct_grp <- group_by(seaice_oct, year)




