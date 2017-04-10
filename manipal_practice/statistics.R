library(ggplot2)
library(Hmisc)
SP500 <- read.csv(file = "SP500.csv")

g <- ggplot(data = SP500)
g <- g + geom_point(aes(x=log(Market.Cap),y=log(EBITDA)))
g

SP500 <- read.csv(file = "SP500.csv",stringsAsFactors=FALSE)
SP500_matrix <- as.matrix(SP500[,c(4:14)])
rcorr(SP500_matrix)

cor.test(SP500$Market.Cap,SP500$EBITDA)
