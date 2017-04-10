library(caret)


mydata <- read.csv("binary.csv")
## view the first few rows of the data

trainIndex <- createDataPartition(y=mydata$admit, p=.8, list=FALSE, times=1)

head(trainIndex)

myDataTrain <- mydata[trainIndex,]
myDataTest <- mydata[-trainIndex,]

#To get the basic derivatives of data
summary(myDataTrain)

#to get the standard deviations of data
sapply(myDataTrain, sd)

attach(myDataTrain)

## two-way contingency table of categorical outcome and predictors
## we want to make sure there are not 0 cells
xtabs(~ admit + rank, data = myDataTrain)

#Logistic regression needs a categorical output variable

myDataTrain$rank <- factor(myDataTrain$rank)

mylogit <- glm(admit ~ gre + gpa + rank, data = myDataTrain, family = "binomial")

#Checking the model created
summary(mylogit)

levels(myDataTrain$rank) <- levels(myDataTest$rank)

myDataTest$rank <- factor(myDataTest$rank)

myDataTest$myDataOutput <- predict(mylogit, myDataTest, type="response")

#Confusion Matrix
table(round(myDataTest$myDataOutput), myDataTest$admit)

