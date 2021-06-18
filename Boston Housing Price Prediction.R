## Step1: Load libraries
library(MASS);library(car);library(corrplot)
library(funr);library(openxlsx);library(dplyr)
library(caret);library(psych);library(plyr)
library(ggplot2);library(zoo);library(lmtest)
library(graphics)

## Data Cleaning
#Read two files
getwd()
setwd("/Users/zhangluyu/Desktop/CS555_Term Project")
df <- read.table("housing.data", header = FALSE, sep = "")
n1 <- read.fwf("housing.names", skip = 30, n = 17, widths = c(-7,8,-60))
#Extract&Remove spaces
n2 <- as.character(n1[-c(3,6,15),])
n2 <- gsub(" ", "", n2)
#Assign the column names(n2) to df
names(df) <- n2

## Step 2: Import data
df <- read.csv("housingdata.csv",header = T)
train <- df[1:400,]
test <- df[-(1:400),]
View(df)
sum(is.na(df)) 
write.csv(df,"/Users/zhangluyu/Desktop/CS555_Term Project/housingdata.csv", row.names = FALSE)

## Step 3: Checking correlation between variables
cor<-cor(df$MEDV,df[c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")])
corrplot(cor(df), method="number", type = "upper", diag = FALSE)

#From the correlation matrix, I observed:
#1.There are no missing or duplicate values
#2.CRIM is strongly associated with RAD and TAX. It means as RAD or TAX increases, the CRIM increases
#3.INDUS has a strong positive correlation with NOX,which implies the NOX is high in industrial areas
#4.NOX increases with increase in INDUS and AGE
#5.There is a strong positive correlation between RAD and TAX, which the value is 0.91. It implies that if the RAD increases, the TAX also increases
#6.MEDV increases as RM increases, and it decreases if percent of LSTAT increases
#7.LSTAT and RM have the highest correlation with house prices.


## Step 4: Building Linear Regression Model
lm1<-lm(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT,data=df)
summary(lm1)
#Looking at the lm1 summary, we know that INDUS and AGE are insignificant

##Constructing a new linear model by analyzing the correlation matrix
#Based on the correlation analysis,the following factors are highly correlated with housing prices, which are "INDUS", "RM", "NOX", "TAX", "PTRATIO","LSTAT"
lm2<-lm(MEDV~INDUS+RM+NOX+TAX+PTRATIO+LSTAT,data=df)
summary(lm2) 
#The summary of lm2 shows that INDUS,NOX and TAX are insignificant

##Model Diagnostics for lm2 - to check the operation of the model
layout(matrix(c(1,2,3,4),2,2))
plot(lm2)

#Residuals vs Fitted: difference between estimated value and true value
#The plot of Residuals vs Fitted shows that the MEDV and predictors is not a completely linear
#Normal Q-Q: It's used to detect whether the residuals are normally distributed
#The Normal Q-Q plot shows that the residuals are not distributed normally.
#Scale-location: To check the equivariance hypothesis
#Residuals vs Leverage: To check whether there are extreme points in the data set


## Step 5: Model testing - to analyze the variance of two models(verification)
anova(lm1,lm2) 
#Model 2 is more meaningful than model 1

## Step 6: Model fitting
train.pred <- predict(lm2,se.fit=TRUE)
par(mfrow=c(1,1))
plot(train$MEDV,col="lightblue",
     pch=15,xlab=expression("num"),ylab="MEDV",
     main="Fitting results of MEDV")
lines(train.pred$fit,col="blue")

#The output is the prediction of the housing prices. It shows by the fitting degree.
#The model trained by the training set is the linear combination of housing price and model2. 
#Then,use the trained model to predict the housing price of the test data set
#The graph is the relationship between the actual housing price(MEDV) and the fitting housing price of training set

#Model1
par(mfrow=c(1,1))
test.pred <- predict(lm1,newdata= test,se.fit=TRUE)
plot(test$MEDV,col="lightblue",
     pch=15,xlab=expression("num"),ylab="MEDV",
     main="Model1:Predicting results of MEDV")
lines(test.pred$fit,col="blue")
estimateError1 <- (test$MEDV-test.pred$fit)
plot(estimateError1,main="Model1:Predicting errors of MEDV")

#Model2
test.pred <- predict(lm2,newdata= test,se.fit=TRUE)
par(mfrow=c(1,1))
plot(test$MEDV,col="lightblue",
     pch=15,xlab=expression("num"),ylab="MEDV",
     main="Model2:Predicting results of MEDV")
lines(test.pred$fit,col="blue")
estimateError2 <- (test$MEDV-test.pred$fit)
plot(estimateError2,main="Model2:Predicting errors of MEDV")

#The graph "Predicting results of MEDV" shows the true value of testing set and the fitting value of two models
#By comparing the graph "Fitting results of MEDV", model1 is less volatile
#For the two graphs are about predicting errors,we see that most of them are distributed below 0.
