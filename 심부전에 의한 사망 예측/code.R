# 데이터 불러오기
df<- read.csv(file = "C:/Users/rstudioid/Downloads/datasets_727551_1263738_heart_failure_clinical_records_dataset.csv",
              header = T, na.strings = c(""))

head(df)

# EDA
df$anaemia<-as.factor(df$anaemia)
df$diabetes<-as.factor(df$diabetes)
df$high_blood_pressure<-as.factor(df$high_blood_pressure)
df$sex<-as.factor(df$sex)
df$smoking<-as.factor(df$smoking)
df$DEATH_EVENT<-as.factor(df$DEATH_EVENT)
str(df)
summary(df)
names(df)
table(is.na(df))
attach(df)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(df, histogram=TRUE, pch=19)
install.packages("corrplot")
library(corrplot)
df.cor<-cor(df)
corrplot(df.cor, method="number")
install.packages("dplyr")
vif(df)
library(dplyr)
a=df %>% 
  select(age,creatinine_phosphokinase,ejection_fraction,platelets ,
         serum_creatinine, serum_sodium , time  )
a=scale(a)
b=df %>% 
  select(-age,-creatinine_phosphokinase,-ejection_fraction,-platelets ,
         -serum_creatinine, -serum_sodium , -time  )
b
x=cbind(a,b)
str(x)
summary(x)


# train / test set으로 나누기
train<-x[1:200, ]
test<-x[201:299, ]


# 로지스틱 회귀분석
glm.fit <- glm( DEATH_EVENT~ ., data =train, family = "binomial")  
summary(glm.fit)

install.packages("GGally")
library(GGally)
ggcorr(df, name="corr",label = T)

glm.fit2<- glm(DEATH_EVENT~age+creatinine_phosphokinase+ejection_fraction+
                 platelets+serum_creatinine+serum_sodium+time+anaemia+diabetes
               +high_blood_pressure+sex,data =train, family = "binomial")

summary(glm.fit2)
glm.fit3<- glm(DEATH_EVENT~age+creatinine_phosphokinase+ejection_fraction+
                 serum_creatinine+serum_sodium+time+anaemia+diabetes
               +high_blood_pressure+sex,data =train, family = "binomial")

summary(glm.fit3)
glm.fit4<- glm(DEATH_EVENT~age+creatinine_phosphokinase+ejection_fraction+
                 serum_creatinine+serum_sodium+time+anaemia+diabetes
               +sex,data =train, family = "binomial")

summary(glm.fit4)
glm.fit5<- glm(DEATH_EVENT~age+creatinine_phosphokinase+ejection_fraction+
                 serum_creatinine+serum_sodium+time
               +sex,data =train, family = "binomial")

summary(glm.fit5)
glm.fit6<- glm(DEATH_EVENT~age+ejection_fraction+
                 serum_creatinine+serum_sodium+time
               +sex,data =train, family = "binomial")

summary(glm.fit6)
glm.fit7<- glm(DEATH_EVENT~age+ejection_fraction+
                 serum_creatinine+serum_sodium+time
               ,data =train, family = "binomial")

summary(glm.fit7)
glm.fit8<- glm(DEATH_EVENT~age+ejection_fraction+
                 serum_creatinine+time
               ,data =train, family = "binomial")

summary(glm.fit8) #최종_statistic=274.83-186.85
pch  # 최종선택모형에 대한 LR Testisq(LR_statistic,df=(199-195),lower.tail = F) #��?�유의미obs=predict(glm.fit8, type="response")
glm.probs
contrasts(DEATH_EVENT)
DEATH_EVENT <-as.factor(DEATH_EVENT)
glm.pred<-rep("0",299)
glm.pred[glm.probs >.5]="1"
table(glm.pred,DEATH_EVENT)
mean(glm.pred==DEATH_EVENT)
summary # 검정 오차율(x)


# svm
a=df %>% 
  select(age,creatinine_phosphokinase,ejection_fraction,platelets ,
         serum_creatinine, serum_sodium , time  )
a=scale(a)
b1=df %>% 
  select(-age,-creatinine_phosphokinase,-ejection_fraction,-platelets ,
         -serum_creatinine, -serum_sodium , -time, -DEATH_EVENT  )
b1
x=cbind(a,b1)
y=df$DEATH_EVENT
y
str(x)
summary(x)
x.train<-x[1:200, ]
x.test<-x[201:299, ]
y.train<-y[1:200]
y.test<-y[201:299]

a=df %>% 
  select(age,creatinine_phosphokinase,ejection_fraction,platelets ,
         serum_creatinine, serum_sodium , time  )
a=scale(a)
b=df %>% 
  select(-age,-creatinine_phosphokinase,-ejection_fraction,-platelets ,
         -serum_creatinine, -serum_sodium , -time )
b
x=cbind(a,b)
train<-x[1:200, ]
test<-x[201:299, ]


# svm
install.packages("e1071")
library(e1071) 
install.packages("caret") 
library(caret) 
install.packages("kernlab")
library(kernlab)  
install.packages("ROCR")
library(ROCR) 
set.seed(1)
tune.out=tune(svm,DEATH_EVENT~., data=train, kernel="linear",
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)  #cost=0.1
svm.fit=svm(DEATH_EVENT~.,data=train, kernel="linear",cost= 0.1,scale=TRUE)
summary(svm.fit)
plot(svm.fit, df)
ypred=predict(svm.fit,test)
table(predict=ypred, truth=test$DEATH_EVENT)
confusionMatrix(data=ypred, reference = test$DEATH_EVENT, positive="1")

set.seed(1)
tune.out=tune(svm,DEATH_EVENT~.,data=train, kernel="radial",
              range=list(cost=c(0.001,0.01,0.1,1,5,10,100,1000),
                         gamma=c(0.5,1,2,3,4,5)))
summary(tune.out)
plot(tune.out, cex.main=0.6, cex.lab=0.8, xaxs="i", yaxs="i")
svm.fit2=svm(DEATH_EVENT~., data=train ,kernal="radical",gamma=0.5, cost=1,scale=TRUE)
summary(svm.fit2)
plot(svm.fit2,train)
ypred2=predict(svm.fit2,test)
table(predict=ypred, truth=test$DEATH_EVENT)
confusionMatrix(data=ypred2, reference = test$DEATH_EVENT, positive="1")

install.packages("ROCR")
library(ROCR)

par(mfrow=c(1,2))
svm.fit.opt=svm(DEATH_EVENT~.,data=train, kernel="linear",cost= 0.1,scale=TRUE ,decision.value=TRUE)
fitted=attributes(predict(svm.fit.opt,test,decision.values = TRUE))$decision.values
predictions <-prediction(fitted, test)
par(mfrow=c(1,2))
rocplot(fitted, test$DEATH_EVENT )
summary(test$DEATH_EVENT)
test
ROC(test=yhat_test, stat=test$y_faulty, plot="ROC", AUC=T, main="SVM")

plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col="black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
  abline(0,1, col="red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend=c(paste0("AUC: ",auc)),cex=0.6,bty = "n",box.col = "white")
}
plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf,col="black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8, xaxs="i", yaxs="i")
}
plot.roc.curve(predictions, title.text = "SVM ROC Curve")
plot.pr.curve(predictions, title.text = "SVM Precision/Recall Curve")

svm.fit2.flex=svm(DEATH_EVENT~., data=train ,kernal="radical",gamma=0.5, cost=1,scale=TRUE,decision.value=TRUE)
fitted=attributes(predict(svm.fit.flex,test,decision.values = TRUE))$decision.values
rocplot(fitted,test$DEATH_EVENT, main="Test Data")


# 랜덤포레스트
install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(DEATH_EVENT ~., data=train, ntree=13, mtry=2)
rf_model
importance(rf_model)
confusionMatrix(test$rf_pred, test$DEATH_EVENT)
