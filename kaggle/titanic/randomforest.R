library(ggplot2)
library(randomForest)

set.seed(1)
# Get train data
train <- read.csv("~/titanic/train.csv", stringsAsFactors = FALSE)

# Preapare features extraction function from variables
featuresExtract <- function(data) {
  features <- c("Pclass",
                "Sex",
                "Age",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked")
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm = TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  return(fea)
}

# Create random forest model in trained data
rf <- randomForest(featuresExtract(train), as.factor(train$Survived), ntree = 100, importance = TRUE)

# Get test data
test <- read.csv("~/titanic/test.csv", stringsAsFactors = FALSE)

submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, featuresExtract(test))

imp <- importance(rf, type = 1)
featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[,1])

ggplot(data = featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  xlab("") +
  ylab("Importance") + 
  coord_flip()

plot(rf)
legend('topright', colnames(rf$err.rate), col=1:3, fill=1:3)








