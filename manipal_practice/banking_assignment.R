library(xlsx)
library(tidyverse)
banking <- read.xlsx("~/manipal_practice/Bank.xls",1,startRow = 2)
labels <- NULL
for (year in seq(2015,2005,-1)) {
  labels <- c(labels, paste(year,"Rural"),paste(year,"Semi-urban"),paste(year,"Urban"),paste(year,"Metropolitian"))
}
colnames(banking)[1:3] <- c("Group","Group1","Bank")
colnames(banking)[4:47] <- labels
banking <- banking[-2]
banking <- na.locf(banking)

banking_actual <- banking %>%
  gather("Type","Values", 3:46) %>%
  separate("Type", c("Year","Category"))

banking_actual$Year <- as.numeric(banking_actual$Year)
banking_actual$Values <- as.numeric(banking_actual$Values)
write.csv(banking_actual, file = "branches.csv")

forex <- read.xlsx("~/manipal_practice/forex.xls",1, startRow = 2)
colnames(forex)[1:ncol(forex)] <- c("Year","Foreign Currency Assets-INR","Foreign Currency Assets-Dollar","Gold-INR","Gold-Dollar","Reserve Tranch Position-INR","Reserve Tranch Position-Dollar","SDRs-INR","SDRs-Dollar")

forex <- forex %>%
  gather("Category","Value",2:ncol(forex)) %>%
  separate("Category", c("Category", "Currency"), sep = "-")

write.csv(x = forex, file = "~/manipal_practice/forex.csv")








