# Classification Tree with rpart
library(rpart)
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
fit <- rpart( admit ~ gre + gpa + rank, method="class", data=myDataTrain)
# grow tree 
#fit <- rpart(Kyphosis ~ Age + Number + Start,
#            method="class", data=kyphosis)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for marks")
text(fit, use.n=TRUE, all=TRUE, cex=.8)