library(randomForest)
require(caTools)
Credit_Risk_Tree <- read.csv("creditrisk.csv")
#Cleaning NA
is.na(Credit_Risk_Tree)
credit_risk_new <- na.omit(Credit_Risk_Tree)
credit_risk_new$loan_status <- as.factor(credit_risk_new$loan_status)
str(credit_risk_new)
smp_siz <- floor(0.75*nrow(credit_risk_new))
# shows the value of the sample size
CR_Train <- sample(seq_len(nrow(credit_risk_new)),size = smp_siz)
#creates the training dataset with row numbers stored in CR_Train
train <- credit_risk_new[CR_Train,]
#creates the training dataset with row numbers stored in CR_Train
test <- credit_risk_new[-CR_Train,]
# initialize Rndom forest
rf <- randomForest(loan_status ~ .,data=train)
rf
Predict_Rf <- predict(rf, newdata = test)


rf_1 <- randomForest(loan_status ~ .,data=train, mtry = 10)
rf_1

Predict_rf_1<- predict(rf_1, newdata = test)

