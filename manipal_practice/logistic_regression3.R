library(tidyverse)
cancer <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data")
colnames(cancer) <- c("code","ClumpThick1ness","Uniformity_of_Cell_Size","Uniformity_of_Cell_Shape","Marginal_Adhesion","Single_Epithelial_Cell_Size","Bare_Nuclei","Bland_Chromatin","Normal_Nucleoli","Mitoses","Class")
cancer <- data.matrix(cancer[-1])
cancer <- data.frame(cancer)
cancer <- lapply(cancer, as.factor)

trainIndex <- createDataPartition(y = cancer$Class, p =.8, list = FALSE, times = 1)

trained <- cancer[trainIndex,]
test <- cancer[-trainIndex,]

fit <- rpart( trained$ ~ gre + gpa + rank, method = "class", data=myDataTrain)