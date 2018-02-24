#Car_Evaluation Individual Project
#By- Priyanka Jaiswal

#Required Library
rm(list=ls(all=TRUE))
library(e1071)
library(caret)
library(nnet)
library(ggplot2)

#Loading the Data
Cars_data<- read.csv("Desktop/Individual_Project/Cars_Data.csv",header=F)
colnames(Cars_data) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "Car_Acceptability")
View(Cars_data)

#Data Cleaning
str(Cars_data)
any(is.na(Cars_data))#Check if there is any missing values are there


#EDA
Cars_data$Car_Acceptability<-factor(Cars_data$Car_Acceptability,levels=c("unacc","acc","good","vgood"),ordered=TRUE)
frequency<-table(Cars_data$Car_Acceptability)
data.frame(frequency)#giving the frequency of each level of class

ggplot(Cars_data,
       aes(x =Car_Acceptability, fill =Car_Acceptability ) ) +
  geom_bar(position = "dodge")+xlab("Car_Acceptability")+
  ylab("Frequecny")+ 
  theme(axis.text.x = element_text(angle = 90))

ggplot(Cars_data, aes(x =safety, fill =Car_Acceptability )) + 
  geom_bar(position="dodge") +xlab("Safety")+ 
  ylab("Frequecny")+ 
  theme(axis.text.x = element_text(angle = 90))

ggplot(Cars_data, aes(x =persons, fill =Car_Acceptability )) + 
  geom_bar(position="dodge",width=0.5) + xlab("Persons")+
  ylab("Frequecny")+ 
  theme(axis.text.x = element_text(angle = 90))

ggplot(Cars_data,
       aes(x = buying, fill =Car_Acceptability ) ) +
  geom_bar(width=0.5) +xlab("Buying")+
 ylab("Frequecny")+ 
  theme(axis.text.x = element_text(angle = 90))
  

ggplot(Cars_data, aes(x =maint , fill =Car_Acceptability )) + 
  geom_bar(width=0.5) +xlab("maint")+
  ylab("Frequency")+ theme(axis.text.x = element_text(angle = 90))


ggplot(Cars_data, aes(x =doors , fill =Car_Acceptability )) + 
  geom_bar(width=0.5) + xlab("Doors")+
  ylab("Frequecny")+ 
  theme(axis.text.x = element_text(angle = 90))


ggplot(Cars_data, aes(x =lug_boot, fill =Car_Acceptability )) + 
  geom_bar(width=0.5) +xlab("lug_boot")+
  ylab("Frequecny")+ 
  theme(axis.text.x = element_text(angle = 90))



#Dividing the data in test and train set
N<-nrow(Cars_data)
set.seed(1234)
pd<-sample(2,nrow(Cars_data),replace=TRUE,prob=c(0.8,0.2))
train<-Cars_data[pd==1,]
test<-Cars_data[pd==2,]


#Best Subset Selection
install.packages("leaps")
library(leaps)
regit.full=regsubsets(Car_Acceptability~.,data=train,nvmax=6)
reg.summary<-summary(regit.full)
names(reg.summary)

#RSQ and RSS
plot(reg.summary$rsq,xlab="Number of Variables", ylab="RSquare",type="l")
plot(reg.summary$rss,xlab="Number of Variable",ylab="RSS",type="l")

#Adjusted R sqaure
which.max(reg.summary$adjr2)
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
points(6,reg.summary$adjr2[6],col="red",cex=2,pch=20)

#cp
which.min(reg.summary$cp)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
points(6,reg.summary$cp[6],col="red",cex=2,pch=20)

#BIC
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Varaibles",ylab="BIC",type="l")
points(5,reg.summary$bic[5],col="red",cex=2,pch=20)

plot(regit.full, scale ="bic")
plot(regit.full, scale ="r2")
plot(regit.full, scale ="adjr2")
plot(regit.full, scale ="Cp")



# Multinomial Logistic Regression with Class_Acceptability and buying
fit1 <- multinom(Car_Acceptability~buying,data=train)
summary(fit1)
#Confusion Matrix
predict_func<-predict(fit1,test,types="prob")
predict_CarAcceptability1<-predict(fit1,test)#predict based on class
tab1<-table(predict_CarAcceptability1,test$Car_Acceptability)
confusionMatrix(predict_CarAcceptability1,test$Car_Acceptability)
#print the Accuracy of the model and Calculate Missclassfication Rate 
Accuracy_classifier_1<-sum(diag(tab1))/sum(tab1)
paste("Accuracy is =",round(Accuracy_classifier_1,4)*100,"%")
error<-mean(as.character(predict_CarAcceptability1)!=as.character(test$Car_Acceptability))
paste("Misclassification error",round(error,4)*100,"%")

# Multinomial Logistic Regression with Class_Acceptability and Safety
fit2 <- multinom(Car_Acceptability~safety,data=train)
summary(fit2)
#Confusion Matrix
predict_func2<-predict(fit2,test,types="probs")
predict_CarAcceptability2<-predict(fit2,test)#predict based on class
tab2<-table(predict_CarAcceptability2,test$Car_Acceptability)
confusionMatrix(predict_CarAcceptability2,test$Car_Acceptability)
#print the Accuracy of the model and Calculate Missclassfication Rate 
Accuracy_classifier_2<-sum(diag(tab2))/sum(tab2)
paste("Accuracy is =",round(Accuracy_classifier_2,4)*100,"%")
error<-mean(as.character(predict_CarAcceptability2)!=as.character(test$Car_Acceptability))
paste("Misclassification error",round(error,4)*100,"%")

# Multinomial Logistic Regression with Class_Acceptability and Maint
fit3 <- multinom(Car_Acceptability~maint,data=train)
summary(fit3)
#Confusion Matrix
predict_func3<-predict(fit3,test,types="probs")
predict_CarAcceptability3<-predict(fit3,test)#predict based on class
tab3<-table(predict_CarAcceptability3,test$Car_Acceptability)
confusionMatrix(predict_CarAcceptability3,test$Car_Acceptability)
# print the Accuracy of the model Calculate Missclassfication Rate 
error_classifier_3<-sum(diag(tab3))/sum(tab3)
paste("Accuracy is =",round(error_classifier_3,4)*100,"%")
error<-mean(as.character(predict_CarAcceptability3)!=as.character(test$Car_Acceptability))
paste("Misclassification error",round(error,4)*100,"%")

# Multinomial Logistic Regression with Class_Acceptability and Doors
fit4 <- multinom(Car_Acceptability~doors,data=train)
summary(fit4)
#Confusion Matrix
predict_func3<-predict(fit4,test,types="probs")
predict_CarAcceptability4<-predict(fit4,test)#predict based on class
tab4<-table(predict_CarAcceptability4,test$Car_Acceptability)
confusionMatrix(predict_CarAcceptability4,test$Car_Acceptability)
#print the Accuracy of the model and Calculate Missclassfication Rate 
error_classifier_4<-sum(diag(tab4))/sum(tab4)
paste("Accuracy is =",round(error_classifier_4,4)*100,"%")
mean(as.character(predict_CarAcceptability4)!=as.character(test$Car_Acceptability))
paste("Misclassification error",round(error,4)*100,"%")

# Multinomial Logistic Regression with Class_Acceptability and lug_boot
fit5 <- multinom(Car_Acceptability~lug_boot,data=train)
summary(fit5)
#Confusion Matrix
predict_func5<-predict(fit5,test,types="probs")
predict_CarAcceptability5<-predict(fit5,test)#predict based on class
tab5<-table(predict_CarAcceptability5,test$Car_Acceptability)
confusionMatrix(predict_CarAcceptability5,test$Car_Acceptability)
#print the Accuracy of the model and Calculate Missclassfication Rate 
error_classifier_5<-sum(diag(tab5))/sum(tab5)
paste("Accuracy is =",round(error_classifier_5,4)*100,"%")
error<-mean(as.character(predict_CarAcceptability5)!=as.character(test$Car_Acceptability))
paste("Misclassification error",round(error,4)*100,"%")

# Multinomial Logistic Regression with Class_Acceptability and safety and Person
fit6 <- multinom(Car_Acceptability~safety+persons,data=train)
summary(fit6)
#Confusion Matrix
predict_func6<-predict(fit6,test,types="probs")
predict_CarAcceptability6<-predict(fit6,test)#predict based on class
tab6<-table(predict_CarAcceptability6,test$Car_Acceptability)
confusionMatrix(predict_CarAcceptability6,test$Car_Acceptability)
#Print the Accuracy of the model and Calculate Missclassfication Rate and 
error_classifier_6<-sum(diag(tab6))/sum(tab6)
paste("Accuracy is =",round(error_classifier_6,4)*100,"%")
error<-mean(as.character(predict_CarAcceptability6)!=as.character(test$Car_Acceptability))
paste("Misclassification error",round(error,4)*100,"%")

# Multinomial Logistic Regression with Class_Acceptability and safety+Person+Buying
fit7 <- multinom(Car_Acceptability~safety+persons+buying,data=train)
summary(fit7)
#Confusion Matrix
predict_func7<-predict(fit7,test,types="probs")
predict_CarAcceptability7<-predict(fit7,test)#predict based on class
tab7<-table(predict_CarAcceptability7,test$Car_Acceptability)
confusionMatrix(predict_CarAcceptability7,test$Car_Acceptability)
#Print the Accuracy of the model and Calculate Missclassfication Rate 
error_classifier_7<-sum(diag(tab7))/sum(tab7)
paste("Accuracy is =",round(error_classifier_7,4)*100,"%")
error<-mean(as.character(predict_CarAcceptability7)!=as.character(test$Car_Acceptability))
paste("Misclassification error",round(error,4)*100,"%")

# Multinomial Logistic Regression with Class_Acceptability and safety+Person+Buying+maint
fit8 <- multinom(Car_Acceptability~safety+persons+buying+maint,data=train,method="glm")
summary(fit8)
#Confusion Matrix
predict_func8<-predict(fit8,test,types="probs")
predict_CarAcceptability8<-predict(fit8,test)#predict based on class
tab8<-table(predict_CarAcceptability8,test$Car_Acceptability)
confusionMatrix(predict_CarAcceptability8,test$Car_Acceptability)
#Print the Accuracy of the model and Calculate Missclassfication Rate 
error_classifier_8<-sum(diag(tab8))/sum(tab8)
paste("Accuracy is =",round(error_classifier_8,4)*100,"%")
error<-mean(as.character(predict_CarAcceptability8)!=as.character(test$Car_Acceptability))
paste("Misclassification error",round(error,4)*100,"%")

# Multinomial Logistic Regression with Class_Acceptability and safety+Person+Buying+maint+lug_boot
fit9 <- multinom(Car_Acceptability~safety+persons+buying+maint+lug_boot,data=train)
summary(fit9)
#Confusion Matrix
predict_func9<-predict(fit9,test,types="probs")
predict_CarAcceptability9<-predict(fit9,test)#predict based on class
tab9<-table(predict_CarAcceptability9,test$Car_Acceptability)
confusionMatrix(predict_CarAcceptability9,test$Car_Acceptability)
#Print the Accuracy of the model and Calculate Missclassfication Rate and
error_classifier_9<-sum(diag(tab9))/sum(tab9)
paste("Accuracy is =",round(error_classifier_9,4)*100,"%")
error<-mean(as.character(predict_CarAcceptability9)!=as.character(test$Car_Acceptability))
paste("Misclassification error",round(error,4)*100,"%")

# Multinomial Logistic Regression with Class_Acceptability and safety+Person+Buying+maint+lug_boot+doors
fit10 <- multinom(Car_Acceptability~safety+persons+buying+maint+lug_boot+doors,data=train)
summary(fit10)
#Confusion Matrix
predict_func10<-predict(fit10,test,types="probs")
predict_CarAcceptability10<-predict(fit10,test)#predict based on class
tab10<-table(predict_CarAcceptability10,test$Car_Acceptability)
confusionMatrix(predict_CarAcceptability10,test$Car_Acceptability)
# Print the Accuracy of the model and Calculate Missclassfication Rate 
error_classifier_10<-sum(diag(tab10))/sum(tab10)
paste("Accuracy is =",round(error_classifier_10,4)*100,"%")
error<-mean(as.character(predict_CarAcceptability10)!=as.character(test$Car_Acceptability))
paste("Misclassification error",round(error,4)*100,"%")

#CROSS Validation for final Build Model icludes all the predictors
control <- trainControl(method = "repeatedcv",number=10,repeats =3)
metric <- "Accuracy"
fit10 <- train(Car_Acceptability~safety+persons+buying+lug_boot+doors+maint,data=train,method="multinom",trControl=control)
print(fit10)
predictions<-predict(fit10,test)
print(fit10$results)













































