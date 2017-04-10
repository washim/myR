library(xts)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(TTR)
library(forecast)
library(scales)
args <- commandArgs(TRUE)
tsfixmyseries <- function(df){
  ts <- xts(df[,-1], order.by = df$Time)
  storage.mode(ts) <- "numeric"
  ts <- to.period(ts, period = "months", indexAt = "firstof", OHLC = FALSE)
  minyear <- as.numeric(format(min(time(ts)),"%Y"))
  maxyear <- as.numeric(format(max(time(ts)),"%Y"))
  ts_new_ts <- list()
  for (year in seq(minyear,maxyear)) {
    gen_months <- seq(as.Date(start(ts[paste(year)])), length = 12, by = "+1 month")
    year_data <- merge(ts[paste(year)], gen_months, join = "right", fill = NA)
    names(year_data)<- c("Temperature")
    ts_new_ts[[year]] <- year_data
  }
  ts_new_ts <- Filter(Negate(is.null),ts_new_ts)
  ts_new_ts <- do.call(rbind.data.frame, ts_new_ts)
  ts_new_ts <- as.xts(ts_new_ts)
  ts_new_ts <- na.locf(ts_new_ts)
  return(ts_new_ts)
}

seasonal <- function(ts) {
  minyear <- as.numeric(format(min(time(ts)),"%Y"))
  maxyear <- as.numeric(format(max(time(ts)),"%Y"))
  winter <- list()
  summer <- list()
  monsoon <- list()
  season <- list()
  for(year in seq(minyear,maxyear)) {
    winter[[year]] <- ts[paste(year,"-01/",year,"-02",sep = "")]
    summer[[year]] <- ts[paste(year,"-03/",year,"-05",sep = "")]
    monsoon[[year]] <- ts[paste(year,"-06/",year,"-12",sep = "")]
  }
  return(season)
}

load("/home/washim/timeseries/weather6years.RData")
weather$TemperatureC <- (weather$TemperatureF - 32) * 5/9
weather$TemperatureC <- as.numeric(weather$TemperatureC)
weather$Time <- as.POSIXct(weather$Time,"%Y-%m-%d")

blore <- weather %>%
  filter(TemperatureC > 0, City == "bangalore", Time >= args[1]) %>%
  select(Time,TemperatureC)

bloretemp <- tsfixmyseries(blore)

yearly_endpoint <- endpoints(bloretemp, on = "years")
yearly_avg <- period.apply(bloretemp, yearly_endpoint, mean)

yearly_diff <- diff(bloretemp, lag = 1, differences = 1)
yearly_diff <- na.locf(yearly_diff, fromLast = TRUE)

png(file = paste("/var/www/html/",args[2],sep = ""), bg = "transparent")
g <- ggplot(data = bloretemp, aes(x = index(bloretemp), y = bloretemp))
g <- g + geom_line() + geom_point()
g <- g + labs(title = "Actual Trend")
g <- g + scale_y_continuous(name='TemperatureC') + scale_x_datetime(name = "Year")
g
dev.off()














