#setwd("D:\\FDM\\01 Boston Housing")
getwd()
data<-read.csv("BostonHousingDescription.csv")

rid<-read.csv("BostonHousingDescription.csv")
# Checking the data for some missing values
summary(data)
#No missing values found

#Data Distribution
for (i  in 1:length(data))
  {if(!is.factor(data[,i]))
  {
  boxplot(data[,i],main = paste("box plot of",names(data)[i]))
}
}
#To find variables are catogorical or numerical and convert them to factor
str(data)

for(i in 1:length(data))
{
  if(length(levels(as.factor(data[,i])))<10)
     {
       data[,i]<-as.factor(data[,i])
  }
}


str(data)
install.packages("mlr")
library(mlr)       
View((summarizeColumns(data)))

barplot(table(data$CAT.MEDV))

prop.table(table(data$CAT.MEDV))
fac<-c(4,9,15)
#To calculate the correlation between the variables with median house value
View(cor(data[,-fac],data$MEDV))

hist(data$MEDV)
plot(data[,c(3,5,6,11,13,14)],pch=3)


library(caret)

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
#Complete model gives a RMSE of 3.945
#to check summary stats of the fit model
summary(fit)

#after checking summary we find that many variables are not contributing much as 
#they have a low significance and it can be due to multicollinearity
install.packages("car")
library(car)
vif(fit)

#Rad have very high VIF. Creating fit.1 without rad
fit.1 <- lm(formula = MEDV ~ CRIM + CHAS + NOX + RM + DIS + PTRATIO + 
                TAX + B + LSTAT, data = train)

pred.1<- predict(fit.1, newdata = test)
RMSE_fit.1<- sqrt(sum((pred.1 - test$MEDV)^2)/
                  length(test$MEDV))
#RMSE increased to 4.737
#checking summary for the new model after removing Rad
summary(fit.1)
#checking vif 
vif(fit.1)
#No further removal needed as Vif is below 5 in all cases
##as the medv was skewed towards right we apply a log transformation  and check whether
#we can increase R^2 value
#fitting new model with log transform
fit1 <- lm(log(MEDV)~ CRIM + CHAS + NOX + RM + DIS + PTRATIO + 
             B + LSTAT,data = train)
pred1<- predict(fit1, newdata = test)
RMSE_fit1<- sqrt(sum((exp(pred1) - test$MEDV)^2)/
                   length(test$MEDV))
#RMSE decreased to 4.109
#checking summary stats of fit1
summary(fit1)
plot(fit1)

# Creating random forest model
#install.packages("randomForest")
library(randomForest)
fitrf<-randomForest(formula = MEDV~.,data=train)
predictrf<-predict(fitrf,test)
rmse.rf <- sqrt(sum(((predictrf) - test$MEDV)^2)/
                  length(test$MEDV))

#RMSE has reduced to 2.842

### Data is scaled as we have different units in the independent variables
train1<-house.scale[inTrain,]
test1<-house.scale[-inTrain,]
fitscale <- lm(log(MEDV)~ CRIM + CHAS + NOX + RM + DIS + PTRATIO + 
             B + LSTAT,data = train1)
predscale<- predict(fitscale, newdata = test1)
rmsescale <- sqrt(sum((exp(predscale) - test1$MEDV)^2)/
                  length(test1$MEDV))
#No difference in RMSE after scaling the data
summary(fitlog)
summary(fit1)

#trying cross validaton to improve accuracyas the dataset is very small
#install.packages("DAAG")
#library(DAAG)
#fitcv<-lm(log(MEDV)~ CRIM + CHAS + NOX + RM + DIS + PTRATIO + 
            #B + LSTAT,data = data)
#a<-cv.lm(data,fitcv,m=10)
rid1 <- rid[,-15] 
#Ridge and lasso
View(model.matrix(rid1$MEDV~.,rid1))
x <- model.matrix(rid$MEDV ~., rid1)[,-1]
y <- rid1$MEDV
lambda <- 10^seq(10, -2, length = 100)
#loading package for ridge and lasso
library(glmnet)

#Test and train
set.seed(489)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]
#OLS model
coef(fit)
#ridge model with lambda 0
#ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
#predict(ridge.mod, s = 0, exact = T, type = 'coefficients')
#ridge model with Cross validated Lamba
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
summary(ridge.mod)
bestlam <- cv.out$lambda.min

#make predictions
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
RMSE_ridge<- sqrt(sum((ridge.pred - ytest)^2)/
                    length(ytest))
#Rmse for ridge is 5 higher then model with all variables
#ridge coeffcients . Ridge have reduced B coeff reducing variance and increasing generalizability
ridge.min <- glmnet(x[train,], y[train], alpha = 0, lambda = bestlam)
coef(ridge.min)

#Lasso
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)

lasso.mod1 <- glmnet(x[train,], y[train], alpha = 1, lambda = bestlam)
lasso.pred <- predict(lasso.mod, s = bestlam , newx = x[test,])

#RMSE for lasso increased to 5.44
RMSE_lasso<- sqrt(sum((lasso.pred - ytest)^2)/
                    length(ytest))
#Lasso made coefficients of Zn,Indus,Nox,Age,Dis,Rad and Tax zero
coef(lasso.mod1)


