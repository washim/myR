library(ggplot2, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(plotly, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(shinydashboard, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(dplyr, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(tidyr, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(dygraphs, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(xts, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")

function(input, output, session) {
  access_log <- read.table("/home/washim/analytics-shiny/operations/assets/access_log")
  
  actual_access_log <- access_log[,c(1,4,7)]
  actual_access_log$V4 <- substring(actual_access_log$V4,2)
  names(actual_access_log) <- c("ip","time","status")
  actual_access_log$time <- as.POSIXct(strptime(actual_access_log$time, "%d/%b/%Y:%H:%M:%S"),"%d/%b/%Y:%H:%M:%S")
  
  df <- xts(actual_access_log, order.by = actual_access_log$time)
  ts <- to.period(df, period = "minutes", indexAt = "firstof", OHLC = FALSE)
  
  hermes <- data.frame(
    site.serial       = seq(1:200),
    site.name         = rep("Hermes", 200),
    site.content      = sapply(1:200,function(i){i + sample(1:60,1)}),
    site.users        = sample(1:10, 200, replace = TRUE),
    site.content.date = sample(seq(as.Date('2016/01/01'), as.Date('2017/01/01'), by="day"), 200)
  )
  
  chauffeur <- data.frame(
    site.serial       = seq(1:200),
    site.name         = rep("Chauffeur", 200),
    site.content      = sapply(1:200,function(i){i + sample(1:30,1)}),
    site.users        = sample(1:10, 200, replace = TRUE),
    site.content.date = sample(seq(as.Date('2016/01/01'), as.Date('2017/01/01'), by="day"), 200)
  )
  
  matrix <- data.frame(
    site.serial       = seq(1:200),
    site.name         = rep("Matrix", 200),
    site.content      = sapply(1:200,function(i){i + sample(1:20,1)}),
    site.users        = sample(1:10, 200, replace = TRUE),
    site.content.date = sample(seq(as.Date('2016/01/01'), as.Date('2017/01/01'), by="day"), 200)
  )
  
  sports <- data.frame(
    site.serial       = seq(1:200),
    site.name         = rep("Sports", 200),
    site.content      = sapply(1:200,function(i){i + sample(1:10,1)}),
    site.users        = sample(1:10, 200, replace = TRUE),
    site.content.date = sample(seq(as.Date('2016/01/01'), as.Date('2017/01/01'), by="day"), 200)
  )
  
  ContentGrow <- rbind(hermes,chauffeur,matrix,sports)
  
  output$plotContentGrow <- renderPlotly({
    p <- plot_ly(ContentGrow, x = ~site.serial) %>%
    add_lines(y = ~site.content, line = list(color = "green"), data = filter(ContentGrow, site.name == "Hermes"), name = "Hermes") %>%
    add_lines(y = ~site.content, line = list(color = "blue"), data = filter(ContentGrow, site.name == "Chauffeur"), name = "Chauffeur") %>%
    add_lines(y = ~site.content, line = list(color = "black"), data = filter(ContentGrow, site.name == "Matrix"), name = "Matrix") %>%
    add_lines(y = ~site.content, line = list(color = "red"), data = filter(ContentGrow, site.name == "Sports"), name = "Sports")
  })
}