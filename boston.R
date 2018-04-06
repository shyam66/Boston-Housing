setwd("D:\\FDM\\01 Boston Housing")
data<-read.csv("BostonHousingDescription.csv")
#install.packages("lmtest", repos = "http://cran.us.r-project.org")
# seeing the data for some missing values
summary(data)
#No missing values found

for (i  in 1:length(data))
  {if(!is.factor(data[,i]))
  {
  boxplot(data[,i],main = paste("box plot of",names(data)[i]))
}
}
#to find variables are catogorical or numerical
str(data)

for(i in 1:length(data))
{
  if(length(levels(as.factor(data[,i])))<10)
     {
       data[,i]<-as.factor(data[,i])
  }
}


str(data)
#install.packages("mlr")
library(mlr)       
View((summarizeColumns(data)))

barplot(table(data$CAT.MEDV))

prop.table(table(data$CAT.MEDV))
fac<-c(4,9,15)
#to calculate the correlation between the variables with median house value
View(cor(data[,-fac],data$MEDV))

hist(data$MEDV)
plot(data[,c(3,5,6,11,13,14)],pch=3)


library(caret)
#nzv <- nearZeroVar(data, saveMetrics = TRUE)
#sum(nzv$nzv)
data1<-data[,-c(14:15)]
data
house.scale<-data.frame()
house.scale <- cbind(scale(data1[-fac]),data[fac],data[14])
#dividing the data into train and test
inTrain <- createDataPartition(y = data$MEDV, p = 0.70, list = FALSE)
train<-data[inTrain,]
test<-data[-inTrain,]

#creating linear regression model
fit<- lm(MEDV~.,data = train)
#to check coefficients
as.data.frame(round(fit$coefficients))
# predicting the model based on the fit
pred<- predict(fit, newdata = test)
#calculating RMSE
RMSE_fit<- sqrt(sum((pred - test$MEDV)^2)/
                             length(test$MEDV))
#to check summary stats of the fit model
summary(fit)

#after checking summary we find that many models are not contributing much as 
#they have a low significance and it can be due to multicollinearity
install.packages("car")
library(car)
vif(fit)
#as the medv was skewed towards right we apply a log transformation  and check whether
#we can increase R^2 value
fit.1 <- lm(formula = MEDV ~ CRIM + CHAS + NOX + RM + DIS + PTRATIO + 
                TAX + B + LSTAT, data = train)

pred.1<- predict(fit.1, newdata = test)
RMSE_fit.1<- sqrt(sum((pred.1 - test$MEDV)^2)/
                  length(test$MEDV))
#checking summary for the new model after removing chas
summary(fit.1)
#checking vif 
vif(fit.1)
#removing Tax from the model
fit.2 <- lm(formula = MEDV ~ CRIM + CHAS + NOX + log(RM) + DIS + PTRATIO + 
                B + LSTAT, data = train)
pred.2<- predict(fit.2, newdata = test)
summary(fit.2)
RMSE_fit.2<- sqrt(sum((pred.2 - test$MEDV)^2)/
                    length(test$MEDV))
#fitting new model with log transform
fit1 <- lm(log(MEDV)~ CRIM + CHAS + NOX + RM + DIS + PTRATIO + 
             B + LSTAT,data = train)
pred1<- predict(fit1, newdata = test)
RMSE_fit1<- sqrt(sum((exp(pred1) - test$MEDV)^2)/
                   length(test$MEDV))
#checking summary stats of fit1
summary(fit1)
plot(fit1)
#new model adding logs for all outliers
fit2 <- lm(log(MEDV)~ log(CRIM) + CHAS + NOX + log(RM) + DIS + PTRATIO + 
             log(B) + log(LSTAT) ,data = train)

summary(fit2)
pred2<- predict(fit2, newdata = test)
RMSE_fit2<- sqrt(sum((exp(pred2) - test$MEDV)^2)/
                    length(test$MEDV))
plot(fit2)
# trying Random Forest to check if accuracy is more
#install.packages("randomForest")
library(randomForest)
fitrf<-randomForest(formula = MEDV~.,data=train)
predictrf<-predict(fitrf,test)
rmse.rf <- sqrt(sum(((predictrf) - test$MEDV)^2)/
                  length(test$MEDV))

### Data is scaled as we have different units in the independent variables
train1<-house.scale[inTrain,]
test1<-house.scale[-inTrain,]
fitlog <- lm(log(MEDV)~ CRIM + CHAS + NOX + RM + DIS + PTRATIO + 
             B + LSTAT,data = train1)
predlog<- predict(fitlog, newdata = test1)
rmselog <- sqrt(sum((exp(predlog) - test1$MEDV)^2)/
                  length(test1$MEDV))
summary(fitlog)
summary(fit1)

#trying cross validaton to improve accuracyas the dataset is very small
install.packages("DAAG")
library(DAAG)
fitcv<-lm(log(MEDV)~ CRIM + CHAS + NOX + RM + DIS + PTRATIO + 
            B + LSTAT,data = data)
a<-cv.lm(data,fitcv,m=10)
a$