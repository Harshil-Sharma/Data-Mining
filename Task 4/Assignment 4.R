install.packages("caret")
install.packages("rpart.plot")
install.packages("rpart.plot")
install.packages("ggplot2")
#select the above packages before further proceeding.
getwd()
mydata<- read.csv(file.choose(), header = T)
mydata$Pass_Fail<-ifelse(mydata$G3<10,"Fail","Pass")
mydata=subset(mydata,select=-c(G3))
mydata$Pass_Fail=factor(mydata$Pass_Fail,levels = c("Fail","Pass"), labels = c(0,1))
Train_Data <- createDataPartition(y = mydata$Pass_Fail,p = 0.70,list = FALSE)
training_data <- mydata[Train_Data,]
testing_data <- mydata[-Train_Data,]
#Naive Bayes decision
set.seed(8975566)
NB_algo=train(training_data[,-31],training_data$Pass_Fail,'nb',trControl=trainControl(method='cv',number=277))
Predict_test<-predict(NB_algo,newdata = testing_data)
confusionMatrix(Predict_test,testing_data$Pass_Fail)
#Decision Tree
set.seed(8975566)
Model<-rpart(Pass_Fail~ .,training_data,method = 'class')
rpart.plot(Model)
set.seed(8975566)
predict<- predict(Model,testing_data, type = 'class')
table(predict,testing_data$Pass_Fail)
confusionMatrix(predict,testing_data$Pass_Fail)