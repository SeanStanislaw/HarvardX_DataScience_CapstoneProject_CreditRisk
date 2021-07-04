library(randomForest)
require(caTools)
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(descr)
library(ggplot2)
library(ISLR)
library(pROC)
library(ROCR)
library(Metrics)
library(ROSE)
Credit_Risk_Tree <- read.csv("creditrisk.csv")
#Cleaning NA
is.na(Credit_Risk_Tree)
credit_risk_new <- na.omit(Credit_Risk_Tree)
credit_risk_new$loan_status <- as.factor(credit_risk_new$loan_status)
str(credit_risk_new)
# Call CrossTable() on Credit_Risk
CrossTable(credit_risk_new$loan_status)
#Split dataset into train and test
# creates a value for dividing the data into train and test. In this case the value is defined as 75% 
#of the number of rows in the dataset
smp_siz = floor(0.75*nrow(credit_risk_new))
# shows the value of the sample size
CR_Train <- sample(seq_len(nrow(credit_risk_new)),size = smp_siz)
#creates the training dataset with row numbers stored in CR_Train
train =credit_risk_new[CR_Train,]
#creates the training dataset with row numbers stored in CR_Train
test=credit_risk_new[-CR_Train,]
# Solve imbalnce problem in dataset using over sampling
Credit_Risk_Tree_balanced <- ovun.sample(loan_status~ ., data = train, p=0.5, seed=1,method="over")$data
table(Credit_Risk_Tree_balanced$loan_status)
 
# Execute random forest package using over sampling

rf_1 <- randomForest(loan_status ~ .,data=Credit_Risk_Tree_balanced)
rf_1
Predict_Rf <- predict(rf_1, newdata = test,type = "response")


# Solve imbalnce problem in dataset using both Over and under sampling
Credit_Risk_Tree_balanced_both <- ovun.sample(loan_status~ ., data = train, p=0.5, seed=1,method="both")$data
table(Credit_Risk_Tree_balanced_both$loan_status)

# Execute random forest package using both method
rf_3 <- randomForest(loan_status ~ .,data=Credit_Risk_Tree_balanced_both)
rf_3
Predict_Rf_1 <- predict(rf_3, newdata = test,type = "response")

# Solve imbalnce problem in dataset using under sampling
Credit_Risk_Tree_balanced_under <- ovun.sample(loan_status~ ., data = train, ,method="under",p=0.5, seed=1)$data
table(Credit_Risk_Tree_balanced_under$loan_status)
rf_4 <- randomForest(loan_status ~ .,data=Credit_Risk_Tree_balanced_under)
rf_4
Predict_Rf_2 <- predict(rf_4, newdata = test,type = "response")


#Evaluating accuracy
#ROC curve over sampling
roc.curve(test$loan_status, Predict_Rf)

#ROC curve both
roc.curve(test$loan_status, Predict_Rf_1)

#ROC curve under sampling
roc.curve(test$loan_status, Predict_Rf_2)


