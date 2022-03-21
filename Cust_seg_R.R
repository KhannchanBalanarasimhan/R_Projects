data <- read.csv(choose.files())
head(data)
str(data)
summary(data)
dim(data)
colSums(is.na(data))
# No missing data available 
data1<- data[,4:5]
head(data1)
colSums(is.na(data1))
set.seed(123)
wcss = vector()
for (i in 1:15) wcss[i] = sum(kmeans(data1, i)$withinss)
wcss
plot(1:15, wcss , type = "b",main = paste("The elbow method"),xlab = "No of Clusters",ylab = "wcss")
# k = 5
set.seed(123)
kmeans = kmeans(x=data1,centers = 5)
y_means = kmeans$cluster
y_means
install.packages("cluster")
library(cluster)
clusplot(data1,y_means,lines=0,shade = T, color = T ,main= paste("Cluster of customer basis Income and Score"),xlab = "Annual Income",ylab = "Spending score of Credit Card")
getwd()
data <- cbind(data1,y_means)
write.csv(data,"best_cust_details.csv")
