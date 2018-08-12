#name:   Amirthy Tejeshwar
#email:  tejuamirthi@gmail.com
#paymentid:  MOJO8310005A9756217 


#importing the data into the environment

mbankloantest<-read.csv("C:/Users/Teju/Desktop/ML/Major/Qs/Loan_Test.csv",stringsAsFactors = FALSE)
dim(mbankloantest)

mbankloantrain<-read.csv("C:/Users/Teju/Desktop/ML/Major/Qs/Loan_Train.csv",stringsAsFactors = FALSE)
dim(mbankloantrain)

View(mbankloantrain)

#because there was an 614th row which was having all NA values except one column
mbankloantrain<-mbankloantrain[1:613,]
View(mbankloantrain)


#creating a function to fill some values in the missing values
changena <- function(v){
  uniq <- unique(v)
  uniq[which.max(tabulate(match(v, uniq)))]
}


#MISSING VALUES BEING FILLED IN TRAIN DATASET

mbankloantrain$Gender <- as.factor(mbankloantrain$Gender)
mbankloantrain$Gender[mbankloantrain$Gender == ""] <- changena(as.factor(mbankloantrain$Gender))
mbankloantrain$Gender

mbankloantrain$Married <- as.factor(mbankloantrain$Married)
mbankloantrain$Married[mbankloantrain$Married == ""] <- changena(as.factor(mbankloantrain$Married))
mbankloantrain$Married

mbankloantrain$Dependents <- as.factor(mbankloantrain$Dependents)
mbankloantrain$Dependents[mbankloantrain$Dependents == ""] <- changena(as.factor(mbankloantrain$Dependents))
mbankloantrain$Dependents

mbankloantrain$Education <- as.factor(mbankloantrain$Education)
mbankloantrain$Education[mbankloantrain$Education == ""] <- changena(as.factor(mbankloantrain$Education))
mbankloantrain$Education

mbankloantrain$Self_Employed <- as.factor(mbankloantrain$Self_Employed)
mbankloantrain$Self_Employed[mbankloantrain$Self_Employed == ""] <- changena(as.factor(mbankloantrain$Self_Employed))
mbankloantrain$Self_Employed

mbankloantrain$LoanAmount[is.na(mbankloantrain$LoanAmount)] <- round(mean(mbankloantrain$LoanAmount, na.rm = TRUE))
mbankloantrain$LoanAmount

mbankloantrain$Loan_Amount_Term[is.na(mbankloantrain$Loan_Amount_Term)] <- changena(mbankloantrain$Loan_Amount_Term)
mbankloantrain$Loan_Amount_Term

mbankloantrain$Credit_History[is.na(mbankloantrain$Credit_History)] <- changena(mbankloantrain$Credit_History)
mbankloantrain$Credit_History


mbankloantrain$Loan_Status <- as.factor(mbankloantrain$Loan_Status)
mbankloantrain$Loan_Status[mbankloantrain$Loan_Status == ""] <- changena(as.factor(mbankloantrain$Loan_Status))
mbankloantrain$Loan_Status

#MISSING VALUES BEING FILLED IN TEST DATASET

mbankloantest$Gender <- as.factor(mbankloantest$Gender)
mbankloantest$Gender[mbankloantest$Gender == ""] <- changena(as.factor(mbankloantest$Gender))
mbankloantest$Gender

mbankloantest$Married <- as.factor(mbankloantest$Married)
mbankloantest$Married[mbankloantest$Married == ""] <- changena(as.factor(mbankloantest$Married))
mbankloantest$Married

mbankloantest$Dependents <- as.factor(mbankloantest$Dependents)
mbankloantest$Dependents[mbankloantest$Dependents == ""] <- changena(as.factor(mbankloantest$Dependents))
mbankloantest$Dependents

mbankloantest$Education <- as.factor(mbankloantest$Education)
mbankloantest$Education[mbankloantest$Education == ""] <- changena(as.factor(mbankloantest$Education))
mbankloantest$Education

mbankloantest$Self_Employed <- as.factor(mbankloantest$Self_Employed)
mbankloantest$Self_Employed[mbankloantest$Self_Employed == ""] <- changena(as.factor(mbankloantest$Self_Employed))
mbankloantest$Self_Employed

mbankloantest$LoanAmount[is.na(mbankloantest$LoanAmount)] <- round(mean(mbankloantest$LoanAmount, na.rm = TRUE))
mbankloantest$LoanAmount

mbankloantest$Loan_Amount_Term[is.na(mbankloantest$Loan_Amount_Term)] <- changena(mbankloantest$Loan_Amount_Term)
mbankloantest$Loan_Amount_Term

mbankloantest$Credit_History[is.na(mbankloantest$Credit_History)] <- changena(mbankloantest$Credit_History)
mbankloantest$Credit_History




#checking and converting the character classes to factor for train data
lapply(mbankloantrain,class)
mbankloantrain['Property_Area','Loan_Status']<-lapply(mbankloantrain['Property_Area','Loan_Status'],factor)


#checking and converting the character classes to factor for test data
lapply(mbankloantest,class)
mbankloantest['Property_Area']<-lapply(mbankloantest['Property_Area'],factor)


#setting the seed and importing the required caTools Library to split
set.seed(102)
library(caTools)

split <- sample.split(mbankloantrain, SplitRatio = 0.70)
Ttrain <- subset(mbankloantrain[2:13], split == T)
Ttest <- subset(mbankloantrain[2:13] , split == F)
head(Ttrain)
head(Ttest)
dim(Ttrain)
dim(Ttest)
dim(mbankloantrain)
dim(mbankloantest)

#MODEL BY USING RANDOM FOREST
library(randomForest)

#checking and converting the character classes to factor for train data
lapply(Ttrain,class)
Ttrain$Property_Area<-as.factor(Ttrain$Property_Area)
Ttrain$Loan_Status<-as.factor(Ttrain$Loan_Status)
View(Ttrain)


#### extra ------Ttrain$Loan_Status<-ifelse(Ttrain$Loan_Status=='Y',1,0)
mbl_rf <- randomForest( Loan_Status~ApplicantIncome+CoapplicantIncome+LoanAmount+Credit_History+Dependents+Property_Area, data = Ttrain)
mbl_rf
importance(mbl_rf)
summary(mbl_rf)

#converting the character clauses to factor for test data
lapply(Ttest, class)
Ttest$Property_Area<-as.factor(Ttest$Property_Area)

#predicting the output for the test data
pred_rf <- predict(mbl_rf , Ttest[1:11], type = "class")
pred_rf

#creating the confusionmatrix for this model
conf_rf<-table(Ttest$Loan_Status , pred_rf)
conf_rf

#calculating the accuracy of the model
accuracy_rf <- sum(diag(conf_rf))/sum(conf_rf)
accuracy_rf



#using naiveBayes classification But lesser accuracy than randomForest
library(e1071)
mbl_nbclassifier<-naiveBayes(Ttrain[,1:11],Ttrain[,12])

pred_mblnb <- predict(mbl_nb , Ttest[1:11], type = "class")
pred_mblnb

#creating the confusionmatrix for this model
conf_mblnb<-table(Ttest$Loan_Status , pred_mblnb)
conf_mblnb

#calculating the accuracy of the model
accuracy_mblnb <- sum(diag(conf_mblnb))/sum(conf_mblnb)
accuracy_mblnb


# SVM model to forecast the loan_status of the customers
## predict by svm for mbankloantest because of more accuracy than random forest
svm_model <- svm(Loan_Status ~ ApplicantIncome+CoapplicantIncome+LoanAmount+Credit_History+Dependents+Property_Area, data = Ttrain)
svm_model

preds_svm <- predict(svm_model, Ttest)
preds_svm

conf_svm<-table(Ttest$Loan_Status , preds_svm)
conf_svm

accuracy_svm <- sum(diag(conf_svm))/sum(conf_svm)
accuracy_svm


pred_svm <- predict(svm_model, mbankloantest)
pred_svm
#after comapring the accuracy results of both random forest and svm ----SVM gives better accuracy
mbankloantest$Loan_Status <- pred_svm
View(mbankloantest)










