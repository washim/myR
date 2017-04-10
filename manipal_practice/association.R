library(arules)
library(arulesViz)
library(dplyr)
setwd("~/manipal_practice")
accident <- read.table(file = "accidents.dat", col.names = 1:50, fill = TRUE)
new_accident_data <- accident[,c(23,25)]
new_accident_data <- na.omit(new_accident_data)
colnames(new_accident_data) <- c("FactorsRoadUser","TypeOfCollision")

new_accident_data$FactorsRoadUser <- ifelse(new_accident_data$FactorsRoadUser == 49, 
                                            "through red light", new_accident_data$FactorsRoadUser)
new_accident_data$FactorsRoadUser <- ifelse(new_accident_data$FactorsRoadUser == 44, 
                                            "over white line", new_accident_data$FactorsRoadUser)
new_accident_data$FactorsRoadUser <- ifelse(new_accident_data$FactorsRoadUser == "through red light" | 
                                            new_accident_data$FactorsRoadUser == "over white line", 
                                            new_accident_data$FactorsRoadUser, NA)
new_accident_data$FactorsRoadUser <- ifelse(new_accident_data$TypeOfCollision == 61, 
                                            "multiple collision", new_accident_data$TypeOfCollision)
new_accident_data$FactorsRoadUser <- ifelse(new_accident_data$TypeOfCollision == 49, 
                                            "collision no obstacle", new_accident_data$TypeOfCollision)
new_accident_data$FactorsRoadUser <- ifelse(new_accident_data$TypeOfCollision == 56, 
                                            "collision obstacle outside roadway", new_accident_data$TypeOfCollision)
new_accident_data$FactorsRoadUser <- ifelse(new_accident_data$TypeOfCollision == "multiple collision" |
                                            new_accident_data$TypeOfCollision == "collision no obstacle" |
                                            new_accident_data$TypeOfCollision == "collision obstacle outside roadway",
                                            new_accident_data$TypeOfCollision, NA)
new_accident_data <- na.omit(new_accident_data)
