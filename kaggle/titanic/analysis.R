library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('gridExtra') # ggplot grid

setwd("~/titanic")
TitanicData <- read.csv("train.csv", stringsAsFactors = F)

# Grab title from passenger name
TitanicData$Title <- gsub("(.*, )|(\\..*)", "", TitanicData$Name)

# Investigate insights
str(TitanicData)
table(TitanicData$Ticket)

# Show title counts by sex
table(TitanicData$Sex, TitanicData$Title)

# Title with very lower counts move into rare title
rare_title <- c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir", "the Countess")
TitanicData$Title[TitanicData$Title %in% rare_title] <- "Rare"
TitanicData$Title[TitanicData$Title == "Mlle"] <- "Miss"
TitanicData$Title[TitanicData$Title == "Mme"] <- "Mrs"
TitanicData$Title[TitanicData$Title == "Ms"] <- "Miss"

# Get surname from Name
TitanicData$Surname <- sapply(TitanicData$Name, function(x){strsplit(x,split = "[,.]")[[1]][1]})

# Get total unique surnames
nlevels(factor(TitanicData$Surname))

# Prepare family
TitanicData$familysize <- TitanicData$SibSp + TitanicData$Parch + 1
TitanicData$family <- paste(TitanicData$Surname, TitanicData$familysize, sep = "_")

# What does our family looks like
ggplot(data = TitanicData[1:nrow(train),], aes(x = familysize, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = 1:11)

# Create discrete family size
TitanicData$FimilyD[TitanicData$familysize == 1] <- "singleton"
TitanicData$FimilyD[TitanicData$familysize > 1 & TitanicData$familysize < 5] <- "small"
TitanicData$FimilyD[TitanicData$familysize > 4] <- "large"

# Show family size of survival using mosaic plot
mosaicplot(table(TitanicData$FimilyD,TitanicData$Survived), shade=TRUE)

# Get cabin number
TitanicData$CabinH <- sapply(TitanicData$Cabin, function(x){strsplit(x,NULL)[[1]][1]})

# Replace missing Embarked
TitanicData$Embarked[c(62,830)] <- "C"

# Get plot to get median fare from embarkment
ggplot(data = TitanicData, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format())

grid.arrange(
ggplot(data = TitanicData[TitanicData$Pclass == 1,], aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha = 0.4) +
  scale_x_continuous(labels = dollar_format(), breaks = seq(0, 500, 30)) +
  geom_vline(aes(xintercept = median(Fare)), colour='red', linetype='dashed') +
  theme_few(),
ggplot(data = TitanicData[TitanicData$Pclass == 2,], aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha = 0.4) +
  scale_x_continuous(labels = dollar_format(), breaks = seq(0, 80, 15)) +
  geom_vline(aes(xintercept = median(Fare)), colour='red', linetype='dashed') +
  theme_few(), 
ggplot(data = TitanicData[TitanicData$Pclass == 3,], aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha = 0.4) +
  scale_x_continuous(labels = dollar_format(), breaks = seq(0, 60, 10)) +
  geom_vline(aes(xintercept = median(Fare)), colour='red', linetype='dashed') +
  theme_few(), ncol = 2
)

# Check how many missing value exist
sapply(TitanicData, function(x){sum(is.na(x))/length(x)*100})

# Convert to factor
TitanicData <- sapply(TitanicData, as.factor)

# Perform mice imputation
mice_mod <- mice(TitanicData[,c('Age','Sex', 'Fare')], method = 'rf')
mice_output <- complete(mice_mod)

Age <- as.numeric(TitanicData[,c('Age')])
mice_output$Age <- as.numeric(mice_output$Age)
par(mfrow=c(1,2))
hist(Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))











