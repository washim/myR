library(rpart)
library(rpart.plot)
set.seed(1)
train <- read.csv("~/titanic/train.csv", stringsAsFactors = FALSE)
test <- read.csv("~/titanic/test.csv", stringsAsFactors = FALSE)
test$Survived <- 0

combi <- rbind(train,test)
combi$Title <- sapply(combi$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$PassengerId == 797] <- 'Mrs' # female doctor
combi$Title[combi$Title %in% c('Lady', 'the Countess', 'Mlle', 'Mee', 'Ms')] <- 'Miss'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Col', 'Jonkheer', 'Rev', 'Dr', 'Master')] <- 'Mr'
combi$Title[combi$Title %in% c('Dona')] <- 'Mrs'
combi$Title <- factor(combi$Title)

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
combi$family_size <- combi$SibSp + combi$Parch + 1

predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(predicted_age, combi[is.na(combi$Age),])

### CREATING MODEL ###
train_new <- combi[1:891,]
test_new <- combi[892:1309,]
test_new$Survived <- NULL

# Find Cabin Class 
train_new$Cabin <- substr(train_new$Cabin,1,1)
train_new$Cabin[train_new$Cabin == ""] <- "H"
train_new$Cabin[train_new$Cabin == "T"] <- "H"

test_new$Cabin <- substr(test_new$Cabin,1,1)
test_new$Cabin[test_new$Cabin == ""] <- "H"

train_new$Cabin <- factor(train_new$Cabin)
test_new$Cabin <- factor(test_new$Cabin)

my_tree <- rpart(Survived ~ Age + Sex + Pclass + family_size, data = train_new, method = "class", control=rpart.control(cp=0.0001))
prp(my_tree)

train_new[train_new$Sex == "male" & train_new$Age >= 53 & train_new$Pclass >= 1.5, 'Survived']


my_prediction <- predict(my_tree, test_new, type = "class")
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)



