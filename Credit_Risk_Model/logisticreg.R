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
getwd()
Credit_Risk <- read.csv("creditrisk.csv")
head(Credit_Risk)
#Cleaning NA
is.na(Credit_Risk)
credit_risk_new <- na.omit(Credit_Risk)
str(credit_risk_new)
# Call CrossTable() on Credit_Risk
CrossTable(credit_risk_new$loan_status)

# Call CrossTable() on grade and Credit_Risk
CrossTable(credit_risk_new$loan_grade, credit_risk_new$loan_status, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

# Call CrossTable() on grade and Credit_Risk
CrossTable(credit_risk_new$person_home_ownership, credit_risk_new$loan_status, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

# Look at summary
summary(credit_risk_new)
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

#Building Logit Model
logit_model_multi <- glm(loan_status ~ person_age + person_income + loan_grade + person_home_ownership + loan_percent_income , family = "binomial", data = train)
# Obtain significance levels using summary()
summary(logit_model_multi)
#Predict using Test data set
pred_log_regression_model <- predict(logit_model_multi, newdata = test, type = "response")
# Look at the predictions range
range(pred_log_regression_model)
# Use a cut-off of 20% to make binary predictions-vectors
cutoff_1 <- 0.20
cutoff_2 <- 0.35
cutoff_3 <- 0.50
class_pred_logit_1 <- ifelse(pred_log_regression_model > cutoff_1, 1, 0)
class_pred_logit_2 <- ifelse(pred_log_regression_model > cutoff_2, 1, 0)
class_pred_logit_3 <- ifelse(pred_log_regression_model > cutoff_3, 1, 0)
# Construct the objects containing ROC-information
ROC_logit_1<- roc(test$loan_status, class_pred_logit_1)
ROC_logit_2<- roc(test$loan_status, class_pred_logit_2)
ROC_logit_3<- roc(test$loan_status, class_pred_logit_3)
#Plot ROC Curve
plot(ROC_logit_1)
plot(ROC_logit_2)
plot(ROC_logit_3)
# Compute the AUCs
auc(test$loan_status, class_pred_logit_1)
auc(test$loan_status, class_pred_logit_2)
auc(test$loan_status, class_pred_logit_3)
#Confusion matrix
cf_matrix_1 <- table(test$loan_status, class_pred_logit_1)
cf_matrix_2 <- table(test$loan_status, class_pred_logit_2)
cf_matrix_3 <- table(test$loan_status, class_pred_logit_3)
#Print CF matrix 
cf_matrix_1
cf_matrix_2
cf_matrix_3

#sensitivity
sensitivity(cf_matrix_1)
sensitivity(cf_matrix_2)
sensitivity(cf_matrix_3)
# Specificity 
specificity(cf_matrix_1)
specificity(cf_matrix_2)
specificity(cf_matrix_3)
#Specificity
#Assessing model accuracy
mean(class_pred_logit_1 == test$loan_status)
mean(class_pred_logit_2 == test$loan_status)
mean(class_pred_logit_3 == test$loan_status)
#library(devtools)