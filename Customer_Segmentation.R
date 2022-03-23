# Clustering - Customer Segmentation for Retail Business
##############################################
install.packages('stringr')
install.packages('reshape2')
library(stringr)
library(dplyr)
library(reshape2)

df <- read.csv(choose.files())
head(df)
str(df)
dim(df)
summary(df)
View(df)

class(df)
Date_Time <- str_split_fixed(df$InvoiceDate, ' ', 2)
Invoice_Date <- Date_Time[,1]
Invoice_Time <- Date_Time[,2]
str(df)
df <- cbind(df, Invoice_Date, Invoice_Time)
df$Invoice_Date <- as.Date(df$Invoice_Date,'%d/%m/%Y')
df <- df[, c(-5,-10)]
head(df)

range(df$Invoice_Date) # gives the range of start and end date
# we are getting NA in range because the dataset has missing data

#Data Preprocessing

# Missing Values
any(is.na(df))
colSums(is.na(df))
# ignoring na values
df_na <- na.omit(df)

range(df_na$Invoice_Date)
today <- as.Date('2011-12-11', format='%Y-%m-%d')

df_na$Monetary <- df_na$Quantity * df_na$UnitPrice
head(df_na)



df_RFM <- df_na %>%
  group_by(CustomerID) %>%
  mutate(frequency = n(),recency = as.numeric(today - Invoice_Date),
         Monetary  = sum(Monetary)) %>%
  filter(Invoice_Date == max(Invoice_Date)) %>%
  filter(CustomerID == max(CustomerID))

head(df_RFM)
dim(df_RFM)
df_RFM[,c(9,10,11)]

# Data for Segmentation
df_RFM_seg <- df_RFM[,c(6,9,10,11)]
df_RFM_seg
df_RFM_seg <- unique(df_RFM_seg)

df_RFM_seg1 <- df_RFM_seg[,-1]
df_RFM_seg_scale <- scale(df_RFM_seg1)

# Calculating the Z-score for every observation
# (obs - mean)/ SD

# Optimal number of clusters
df1 <- df_RFM_seg_scale
wss <- (nrow(df1)-1) * sum(apply(df1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df1,centers=i)$withinss)
plot(1:15, wss, type ='b', xlab ='Number of Clusters',ylab ='Within groups sum of squares')

dim(df_RFM_seg_scale)
kmeans <- kmeans(df_RFM_seg_scale,6)
kmeans
cluster_num <- kmeans$cluster
cluster_num

df_RFM_seg <- as.data.frame(df_RFM_seg)

df_RFM_seg_Final <- cbind(df_RFM_seg,cluster_num)
tapply(df_RFM_seg_Final$Monetary,df_RFM_seg_Final$cluster_num,mean)
tapply(df_RFM_seg_Final$frequency,df_RFM_seg_Final$cluster_num,mean)
tapply(df_RFM_seg_Final$recency,df_RFM_seg_Final$cluster_num,mean)
