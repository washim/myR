library(xlsx)
library(tidyverse)
library(ggplot2)
branches <- read.xlsx("~/manipal_practice/banking.xls",1)
colnames(branches)[1] <- "Serial"
colnames(branches)[6] <- "Branch"

branches$Branch <- as.numeric(branches$Branch)

branches_by_year_and_group <- branches %>%
  group_by(Group,Year) %>%
  summarise(avg = mean(Branch), median = median(Branch), sumcount = sum(Branch))
  
g <- ggplot(data = branches_by_year_and_group, aes(x = Year))
g <- g + geom_smooth(aes(y = sumcount, color = Group), se = FALSE, method = 'loess')
g <- g + ylab("No of Branches")
g <- g + scale_x_continuous(breaks = 2005:2015)
g

branches_by_year_and_group_category <- branches %>%
  group_by(Group,Year,Category) %>%
  summarise(avg = mean(Branch), median = median(Branch), sumcount = sum(Branch))

g <- ggplot(data = branches_by_year_and_group_category, aes(x = Year))
g <- g + geom_smooth(aes(y = sumcount, color = Group), se = FALSE, method = 'loess')
g <- g + ylab("No of Branches")
g <- g + facet_wrap(~ Category)
g <- g + scale_x_continuous(breaks = 2005:2015)
g

gva <- read.xlsx("~/manipal_practice/banking.xls",2)
gva_by_year <- gva %>%
  gather("Dept","Value", 3:ncol(gva)) %>%
  group_by(Year,Dept) %>%
  summarise(Growth = sum(Value))

g <- ggplot(data = gva_by_year, aes(x = Year))
g <- g + geom_smooth(aes(y = Growth, color = Dept), se = FALSE, method = 'loess')
g <- g + ylab("Gross Economic Value Added")
g <- g + scale_x_continuous(breaks = 2011:2016)
g

forex <- read.xlsx("~/manipal_practice/banking.xls",3)
forexts <- forex[-1]
forexts$Value <- as.numeric(forexts$Value)

g <- ggplot(data = forexts, aes(x = Year))
g <- g + geom_smooth(aes(y = Value, color = Category), se = FALSE, method = 'loess')
g












