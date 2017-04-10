crime_data <- read.csv("D:/neelima/Manipal/crime_data.csv")

crime_data <- na.omit(crime_data)

crime <- data.matrix(crime_data)
 


#Find the optimal value of K
#Optimal value of K is which gives us converged clusters with minimum distortion
#Distortion can be calculated from withinss
kmeans.wss.k <- function(crime, k) {
  km = kmeans(crime,k)
  return (km$tot.withinss)
  
}


#Plotting the elbow graph with different values of km$tot.withinss
kmeans.dis <- function(crime, maxk){
  dis=(nrow(crime)-1)*sum(apply(crime,2,var))
  dis[2:maxk]=sapply (2:maxk, kmeans.wss.k, crime=crime)
  return(dis)
  }
maxk = 10
dis = kmeans.dis(crime, maxk);
plot(1:maxk, dis, type='b', xlab="Number of Clusters",
       ylab="Distortion",
       col="blue")

cl <- kmeans(crime,4)



