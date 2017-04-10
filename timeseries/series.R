library(twitteR)
library(qdap)
library(tm)
library(tidyverse)
library(x)
setup_twitter_oauth("dwWjhhkM1rDDgE4pK42BMtr3U","5mGxL413I3Oe2fay98s43QahjCNXgtQ523d1PqhPyAjDJPSfBO","765042304478355456-mrXdYF4AZNV45tAK7F8XdgockvesA3D","EzcPB8W3ic0YkLnz1JaDJvVaXLNayLsvrl8S584zTQPNH")

keywords <- searchTwitter("modi",n = 1000)
keywords <- do.call("rbind",lapply(keywords, as.data.frame))

keywords_clean <- keywords %>%
  select(replyToSN,created) %>%
  drop_na()

data <- data.frame(category = rep('modi',nrow(keywords_clean)),replyToSN = keywords_clean$replyToSN,created = keywords_clean$created)
xts(data,order.by = as.POSIXlt(data$created))
data <- table(data)
data <- as.data.frame(data)

data <- data %>%
  filter(Freq > 0)