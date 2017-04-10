library(tidyverse)
cancer <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data")
colnames(cancer) <- c("code","ClumpThick1ness","Uniformity_of_Cell_Size","Uniformity_of_Cell_Shape","Marginal_Adhesion","Single_Epithelial_Cell_Size","Bare_Nuclei","Bland_Chromatin","Normal_Nucleoli","Mitoses","Class")

cancer <- data.matrix(cancer)
cancer <- data.frame(cancer)

cancer$Class <- gsub(2, 0, cancer$Class)
cancer$Class <- gsub(4, 1, cancer$Class)
cancer$Class <- factor(cancer$Class)

trainIndex <- createDataPartition(y=cancer$Class, p=.8, list=FALSE, times=1)
myDataTrain <- cancer[trainIndex,]
myDataTrain$Class <- factor(myDataTrain$Class)
myDataTest <- cancer[-trainIndex,]
myDataTest$Class <- factor(myDataTest$Class)

mylogit <- glm(myDataTrain$Class ~ myDataTrain$ClumpThick1ness + myDataTrain$Uniformity_of_Cell_Size, data = myDataTrain, family = "binomial")

myDataTest$myDataOutput <- predict(mylogit, myDataTest, type="response")

summary(mylogit)
table(round(myDataTest$myDataOutput), myDataTest$Class)
