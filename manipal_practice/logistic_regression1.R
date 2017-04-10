library(caret)
mydata <- read.csv("binary.csv")
trainIndex <- createDataPartition(y=mydata$admit, p=.8, list=FALSE, times=1)
myDataTrain <- mydata[trainIndex,]
myDataTest <- mydata[-trainIndex,]
mylogit <- glm(admit ~ gre + gpa + rank, data = myDataTrain, family = "binomial")
myDataTest$myDataOutput <- predict(mylogit, myDataTest, type="response")
table(round(myDataTest$myDataOutput), myDataTest$admit)
