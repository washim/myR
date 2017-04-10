.libPaths("/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(shiny)
library(datasets)
library(dygraphs)
library(xts)
library(tidyverse)

shinyServer(function(input, output) {
  access_log <- read.table("/home/washim/analytics-shiny/austres/assets/access_log")
  actual_access_log <- access_log[,c(1,4,7)]
  actual_access_log$V4 <- substring(actual_access_log$V4,2)
  names(actual_access_log) <- c("ip","time","status")
  actual_access_log$time <- as.POSIXct(strptime(actual_access_log$time, "%d/%b/%Y:%H:%M:%S"),"%d/%b/%Y:%H:%M:%S")
  actual_access_log <- actual_access_log %>%
    group_by(time,status) %>%
    summarise(frequency = n())
  
  df <- xts(actual_access_log[-1], order.by = actual_access_log$time)
  ts <- to.period(df, period = "minutes", indexAt = "firstof", OHLC = FALSE)
    
  output$dygraph <- renderDygraph({
    dygraph(ts)
  })
})
