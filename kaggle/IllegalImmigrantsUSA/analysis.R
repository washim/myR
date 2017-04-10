library(tidyverse)
library(ggmap)
library(ggplot2)

arrests <- read.csv("~/kaggle/IllegalImmigrantsUSA/df_arrests.csv", stringsAsFactors = F)
summary(arrests)
sapply(arrests, function(x) sum(is.na(x))/length(x)*100)
arrests <- na.omit(arrests)
arrests <- arrests[,4:9]

arrestsum <- arrests %>%
  group_by(Latitude,Longitude,Identity) %>%
  summarise(arrested = mean(no_of_arrest))

usa <- get_map("USA", zoom = 4)
ggmap(usa) +
  geom_point(data = arrestsum, aes(x = Longitude, y = Latitude, size = arrested), color = "red", alpha = 0.5)

arrestSummary <- arrests %>%
  group_by(City) %>%
  summarise(arrested = mean(no_of_arrest)) %>%
  arrange(desc(arrested))

ggplot(data = arrestSummary, aes(x = Year, y = arrested, fill = Identity, size = City)) +
  geom_bar(stat = "identity")








