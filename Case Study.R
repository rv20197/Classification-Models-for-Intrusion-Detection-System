IDS_CaseStudy=read.csv("D:/PGDA Files/R/Case Studies/IDS Case Study/Intrusion Detection System.csv",header = T)
sum(is.na(IDS_CaseStudy))
head(IDS_CaseStudy)
str(IDS_CaseStudy)
summary(IDS_CaseStudy)
IDS_CaseStudy$protocol_type=factor(IDS_CaseStudy$protocol_type)
IDS_CaseStudy$service=factor(IDS_CaseStudy$service)
IDS_CaseStudy$flag=factor(IDS_CaseStudy$flag)
IDS_CaseStudy$land=factor(IDS_CaseStudy$land)
IDS_CaseStudy$logged_in=factor(IDS_CaseStudy$logged_in)
IDS_CaseStudy$is_host_login=NULL
IDS_CaseStudy$num_outbound_cmds=NULL
dim(IDS_CaseStudy)
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
Normalized_data=normalize(dplyr::select_if(IDS_CaseStudy, is.numeric))
Normalized_data$class=IDS_CaseStudy$class
Case_study_Index=sample(nrow(Normalized_data),0.75*nrow(Normalized_data))
train=Normalized_data[Case_study_Index,]
test=Normalized_data[-Case_study_Index,]

#RandomForest
library(randomForest)
library(e1071)
library(caret)
RF_model_IDS=randomForest(class~.,data = train,ntree=10)
plot(RF_model_IDS)
pred_train=predict(RF_model_IDS,train)
confusionMatrix(train$class,pred_train)
pred_test=predict(RF_model_IDS,test)
confusionMatrix(test$class,pred_test)


#Naive Bayes
naive_model_IDS=naiveBayes(class~.,data = train)
predi_train_naive=predict(naive_model_IDS,train)
predi_test_naive=predict(naive_model_IDS,test)
confusionMatrix(train$class,predi_train_naive)
confusionMatrix(test$class,predi_test_naive)


#SVM
svm_casestudy=svm(class~.,data = train,kernel="linear",scale = F)
predict_svm_casestudy=predict(svm_casestudy,train)
confusionMatrix(train$class,predict_svm_casestudy)
predict_svm_casestudy_train=predict(svm_casestudy,test)
confusionMatrix(test$class,predict_svm_casestudy_train)