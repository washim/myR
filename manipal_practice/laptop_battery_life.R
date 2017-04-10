batery <- read.csv("https://s3.amazonaws.com/hr-testcases/399/assets/trainingdata.txt")
input <- 0.09
colnames(batery) <- c("charged","lasted")
mymodel <- lm(lasted ~ charged, data = batery)
output <- trunc(predict(mymodel,data.frame(charged=input)))
output
