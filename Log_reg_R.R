# Logistic_regression
# Project- Diabetes
# Import the data set
data <-read.csv(choose.files())
# Scope:
#??? Check missing values and outliers and treat them accordingly
#??? Feature Selection and Data Pre-processing
#??? Evaluating the model with various metrics like Accuracy, AUC ROC, Precision, etc. 
#and improve the score using statistical analysis over time
head(data)
tail(data)
summary(data)
# see the dataset for better understanding
str(data)
# Dependent variable  = Outcome(Binary class)
# rest all are independent and all are numeric and hence we dont need to handle encoding
# Preprocessing
# part1 : Missing value
colSums(is.na(data))
summary(data)
min(data$Glucose)
# Glucose BP and SkinThickness / Insulin/BMI cant have values as 0 so we have to consider it as missing value
boxplot(data$Glucose)
median(data$Glucose)
data$Glucose<-ifelse(data$Glucose==0,117,data$Glucose)
min(data$Glucose)
# BP
boxplot(data$BloodPressure)
median(data$BloodPressure)
data$BloodPressure<-ifelse(data$BloodPressure==0,72,data$BloodPressure)
min(data$BloodPressure)
#SkinThickness
boxplot(data$SkinThickness)
median(data$SkinThickness)
data$SkinThickness<-ifelse(data$SkinThickness==0,23,data$SkinThickness)
min(data$SkinThickness)
#Insulin
boxplot(data$Insulin)
median(data$Insulin)
data$Insulin<-ifelse(data$Insulin==0,30.5,data$Insulin)
min(data$Insulin)
# BMI
boxplot(data$BMI)
median(data$BMI)
data$BMI<-ifelse(data$BMI==0,32,data$BMI)
min(data$BMI)
#### OUTLIER 
## PP1 missing value treatment completed
# Since its logit Reg (classification ) we dont need to worry abt the outlier as there is no impact on out model
# However for learning purpose we are going to handle outlier 
summary(data$SkinThickness)
Q1 = 23
Q3 = 32
IQR = Q3-Q1
pos = Q3 +1.5*IQR
neg = Q1 -1.5*IQR
print(pos)
print(neg)
data$SkinThickness<-ifelse(data$SkinThickness<=9.5,10,data$SkinThickness)
data$SkinThickness<-ifelse(data$SkinThickness>=45.5,44,data$SkinThickness)
boxplot(data$SkinThickness)
#3 Encoding # there is no car variable given in the data set 
# 4 Feature Scaling it's classification problem 
# data<-scale(data[-9])
# check imbalance dataset
unique(data$Outcome)
table(data$Outcome)
# MAjorityclass>2*Minorityclass = Imbalanced 
# Data is balanced no ammendment is required 
# Note : Preprocessing part completed
cor(data)
library(corrgram)
heatmap(cor(data))
# split the data into training and test
install.packages("caTools")
library(caTools)
set.seed(101)
split <- sample.split(data$Outcome,SplitRatio = 0.70)
split
table(split)
training<-subset(data,split==TRUE)
testing<-subset(data,split==FALSE)
print(nrow(training))
print(nrow(testing))
print(table(split))
# Building Generalised Linear Model - mle - "Maximum Likelihood Estimator"
names(data)
logit <- glm(Outcome~.,data=training,family =binomial)
logit
# To find the best model STEP function 
step(logit)
logit_1 <- glm (Outcome ~ Pregnancies + Glucose + Insulin + BMI + 
  DiabetesPedigreeFunction, family = binomial, data = training)
logit_1
summary(logit_1)
# Predicted variable test dataset 
y_pred <-predict(logit_1,newdata = testing , type ="response")
y_pred
# Using " MLE " concept to evaluate the model accuracy 
y_pred_res <- ifelse(y_pred>=0.5,1,0)
y_pred_res
cbind_test_pred <-cbind(testing$Outcome,y_pred_res)
head(cbind_test_pred)
# Confusion Matrix 
cm<-table(testing$Outcome,y_pred_res)
cm
accuracy = (106+39)/(106+19+28+39)
accuracy
# to check confusion matrix 
library(caret)
confusionMatrix(cm)
# for threshold 0.5 for 0.7552083
y_pred_res1 <- ifelse(y_pred>=0.6,1,0)
y_pred_res1
# Confusion Matrix 
cm1<-table(testing$Outcome,y_pred_res1)
cm1
confusionMatrix(cm1)
# for threshold 0.6 for 0.7652083
y_pred_res3 <- ifelse(y_pred>=0.7,1,0)
y_pred_res3
# Confusion Matrix 
cm2<-table(testing$Outcome,y_pred_res3)
cm2
confusionMatrix(cm2)
# for threshold 0.7 for 0.7852083
# 0.6 is the threshold 
###### RAndom Forest Model
install.packages("randomForest")
library(randomForest)
rf<-randomForest(Outcome~.,data=training,ntree=500)
rf_pred<-predict(rf,newdata = testing)
rf_pred<-ifelse(rf_pred>=0.6,1,0)
cm<-table(testing$Outcome,rf_pred)
confusionMatrix(cm)
# AUC and ROC 
# Area under the curve and Reciever Operating Characterics 
install.packages("ROCR")
library(ROCR)
ROCR_p <- prediction(y_pred_res1,testing$Outcome)
ROCR_p
ROCvalue<-performance(ROCR_p,'tpr','fpr')
ROCvalue
plot(ROCvalue)
abline(a=0,b=1)
