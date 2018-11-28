# This is an exploration of the bike data. The ultimate goal is to 
# create an application or visualizations where you can see how bike
# commuting varies according to: 
# 1. time of day
# 2. time of year
# 3. weather


# It could be interesting to just write a couple of notebooks exploring
# the data, which could be published on a blog or something. 

library(lubridate)
library(dplyr)
# Load the data 
for (file in list.files("data", "*.RData")) {
  load(file.path("data", file))
}

names(fremont)
fremont <- rename(fremont, west = `Fremont Bridge West Sidewalk`,
                  east = `Fremont Bridge East Sidewalk`)
names(second)
second <- rename(second, total = `2nd Ave Cycletrack`, north = NB, south = SB)
names(spokane)
spokane <- rename(spokane, total = `Spokane St. Bridge Total`)

# all files have a "Date" column and numbers for each direction. 
head(fremont$Date)
head(second$Date)
head(spokane$Date)
# Looks like "Date" is by hour for every one. 

range(fremont$Date, na.rm = TRUE) # 10/3/2012 - 10/31/2018
range(second$Date,  na.rm = TRUE) # 1/1/2015 - 10/31/2018
range(spokane$Date, na.rm = TRUE) # 1/1/2014 - 10/31/2018

# Get daily totals and plot those from 2012-2018
fremont_daily <- fremont %>%
  mutate(day = strftime(Date, "%Y-%m-%d")) %>%
  group_by(day) %>%
  summarise(east = sum(east), west = sum(west)) %>%
  mutate(day = as.POSIXct(strptime(day, "%Y-%m-%d")))


second_daily <- second %>%
  mutate(day = strftime(Date, "%Y-%m-%d")) %>%
  group_by(day) %>%
  summarise(north = sum(north), south = sum(south)) %>%
  mutate(day = as.POSIXct(strptime(day, "%Y-%m-%d")))

spokane_daily <- spokane %>%  
  mutate(day = strftime(Date, "%Y-%m-%d")) %>%
  group_by(day) %>%
  summarise(east = sum(East), west = sum(West)) %>%
  mutate(day = as.POSIXct(strptime(day, "%Y-%m-%d")))

par(mfrow = c(3,1), mar = c(2, 4, 2, 1))
plot(0,0, xlim = as.numeric(range(fremont$Date, na.rm = TRUE)), xlab = "", ylim = c(0, 3500), axes = F,
     ylab = "total daily trips")
lines(fremont_daily$day, fremont_daily$east)
box()
title(main = "Fremont")
axis(2)

plot(0,0, xlim = as.numeric(range(fremont$Date, na.rm = TRUE)), xlab = "", ylim = c(0, 3500), axes = F,
     ylab = "total daily trips")
lines(second_daily$day, second_daily$south)
box()
title(main = "Second Street")
axis(2)

plot(0,0, xlim = as.numeric(range(fremont$Date, na.rm = TRUE)), xlab = "", ylim = c(0, 3500), axes = F,
     ylab = "total daily trips")
lines(spokane_daily$day, spokane_daily$east)
box()
title(main = "Spokane Street")
axis(2)


axis.POSIXct(1, fremont_daily$day)


# More rides in the summer--no surprises there!
# Let's look at number of rides over one day. Start with Fremont. Giant spaghetti plot, 
# with lines colored by season


spring <- c(4, 5, 6)
summer <- c(7, 8, 9)
fall <- c(10, 11, 12)
winter <- c(1, 2, 3)

wintercolor <- "#51aeff"
springcolor <- "#50a500"
summercolor <- "#bf0000"
fallcolor <- "#ffd800"

seasoncolor <- function(date) {
  ifelse(month(date) %in% spring, springcolor,
         ifelse(month(date) %in% summer, summercolor,
                ifelse(month(date) %in% fall, fallcolor,
                       ifelse(month(date) %in% winter, wintercolor, NA))))
}

fremont_byday <- fremont %>%
  mutate( seasoncolor = seasoncolor(Date)) %>%
  mutate(day = strftime(Date, "%Y-%m-%d")) %>%
  group_by(day, seasoncolor) %>%
  nest()

dev.off()
plot(-50,0, xlim = c(0, 24), ylim = c(0, 720), ann = F)
plotDay <- function(row) {
  data <- row$data[[1]]
  hour <- hour(data$Date)
  lines(hour, data$east, col = adjustcolor(row$seasoncolor, .05), lwd = 2)
}
for (i in seq_len(nrow(fremont_byday)) ) {
  row <- fremont_byday[i,]
  plotDay(row)
}
legend("topleft", legend = c("winter", "spring", "summer", "fall"),
       lty = 1,
       col = c(wintercolor, springcolor, summercolor, fallcolor),
       lwd = 2)
title(main = "All east-bound Fremont bike rides")

plot(-50,0, xlim = c(0, 24), ylim = c(0, 720), ann = F)
plotDay <- function(row) {
  data <- row$data[[1]]
  hour <- hour(data$Date)
  lines(hour, data$west, col = adjustcolor(row$seasoncolor, .05), lwd = 2)
}
for (i in seq_len(nrow(fremont_byday)) ) {
  row <- fremont_byday[i,]
  plotDay(row)
}
legend("topleft", legend = c("winter", "spring", "summer", "fall"),
       lty = 1,
       col = c(wintercolor, springcolor, summercolor, fallcolor),
       lwd = 2)
title(main = "All west-bound Fremont bike rides")


# Same thing, but just 2017
fremont_byday <- filter(fremont_byday, year(strptime(day, "%Y-%m-%d")) == 2017)

plot(-50,0, xlim = c(0, 24), ylim = c(0, 720), ann = F)
plotDay <- function(row) {
  data <- row$data[[1]]
  hour <- hour(data$Date)
  lines(hour, data$east, col = adjustcolor(row$seasoncolor, .1), lwd = 2)
}
for (i in seq_len(nrow(fremont_byday)) ) {
  row <- fremont_byday[i,]
  plotDay(row)
}
legend("topleft", legend = c("winter", "spring", "summer", "fall"),
       lty = 1,
       col = c(wintercolor, springcolor, summercolor, fallcolor),
       lwd = 2)
title(main = "All east-bound Fremont bike rides in 2017")

plot(-50,0, xlim = c(0, 24), ylim = c(0, 720), ann = F)
plotDay <- function(row) {
  data <- row$data[[1]]
  hour <- hour(data$Date)
  lines(hour, data$west, col = adjustcolor(row$seasoncolor, .1), lwd = 2)
}
for (i in seq_len(nrow(fremont_byday)) ) {
  row <- fremont_byday[i,]
  plotDay(row)
}
legend("topleft", legend = c("winter", "spring", "summer", "fall"),
       lty = 1,
       col = c(wintercolor, springcolor, summercolor, fallcolor),
       lwd = 2)
title(main = "All west-bound Fremont bike rides in 2017")

## Now separate out weekday and weekend rides
weekends <- filter(fremont_byday, wday(strptime(day, "%Y-%m-%d")) %in% c(1, 7))
weekdays <- filter(fremont_byday, !wday(strptime(day, "%Y-%m-%d")) %in% c(1, 7))

par(mar = c(0,0,0,0))
plot(-50,0, xlim = c(0, 24), ylim = c(0, 720), ann = F, axes = F)
plotDay <- function(row) {
  data <- row$data[[1]]
  hour <- hour(data$Date)
  lines(hour, data$east, col = adjustcolor(row$seasoncolor, .1), lwd = 2)
}
for (i in seq_len(nrow(weekdays)) ) {
  row <- weekdays[i,]
  plotDay(row)
}
legend("topleft", legend = c("winter", "spring", "summer", "fall"),
       lty = 1,
       col = c(wintercolor, springcolor, summercolor, fallcolor),
       lwd = 2)
title(main = "All week-day west-bound Fremont bike rides in 2017")

plot(-50,0, xlim = c(0, 24), ylim = c(0, 720), ann = F)
plotDay <- function(row) {
  data <- row$data[[1]]
  hour <- hour(data$Date)
  lines(hour, data$west, col = adjustcolor(row$seasoncolor, .1), lwd = 2)
}
for (i in seq_len(nrow(weekends)) ) {
  row <- weekends[i,]
  plotDay(row)
}
legend("topleft", legend = c("winter", "spring", "summer", "fall"),
       lty = 1,
       col = c(wintercolor, springcolor, summercolor, fallcolor),
       lwd = 2)
title(main = "All weekend west-bound Fremont bike rides in 2017")

svg("bike.svg", 7, 7)

png("bike.png", width = 550, height = 550)
par(mar = c(0,0,0,0), bg = "transparent")
plot(-50,0, xlim = c(0, 24), ylim = c(0, 720), ann = F, axes = F)
plotDay <- function(row) {
  data <- row$data[[1]]
  hour <- hour(data$Date)
  lines(hour, data$east, col = adjustcolor(row$seasoncolor, .2), lwd = 5)
}
for (i in seq_len(nrow(weekdays)) ) {
  row <- weekdays[i,]
  plotDay(row)
}
dev.off()

