library(xts)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(TTR)
library(forecast)

weather$TemperatureC <- (weather$TemperatureF - 32) * 5/9
weather$TemperatureC <- as.numeric(weather$TemperatureC)
weather$Time <- as.POSIXct(weather$Time,"%Y-%m-%d")

blore <- weather %>%
  filter(TemperatureC > 0, City == "bangalore") %>%
  select(Time,TemperatureC)

blore <- as.xts(blore[,-1],order.by = blore$Time)
endpoint <- endpoints(blore, on = "days")
mean_blore <- period.apply(blore, endpoint, mean)
plot(mean_blore)
mean_blore_SMA8 <- SMA(mean_blore,n=50)
plot(mean_blore_SMA8)
blore_fore <- HoltWinters(mean_blore,beta = FALSE,gamma = FALSE)
blore_forecast <- forecast.HoltWinters(blore_fore,h=4)
plot(blore_forecast)




