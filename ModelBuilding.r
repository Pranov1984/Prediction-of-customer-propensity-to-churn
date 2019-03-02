#Data Splitting
library(caret)
set.seed(1234)

LGM1=glm(churn~., data = Telecom_Winsor, family = "binomial")
summary(LGM1)
#Remove hnd_wecap since it did not seem to be a significant is introducing NAs in the model
names(Telecom_Winsor)
Telecom_Winsor_Lg=subset(Telecom_Winsor,select = -hnd_webcap)
Telecom_Winsor_Lg=droplevels(Telecom_Winsor_Lg)
set.seed(1234)
Index=createDataPartition(Telecom_Winsor_Lg$churn,times = 1,p=0.75,list = FALSE)
Train=Telecom_Winsor_Lg[Index,]
Test=Telecom_Winsor_Lg[-Index,]

prop.table(table(Train$churn))

LGM1=glm(churn~., data = Train, family = "binomial")
summary(LGM1)
step(LGM1, direction = "both")

library(car)

LGMF=glm(formula = churn ~ mou_Mean + totmrc_Mean + rev_Range + drop_blk_Mean + 
           drop_vce_Range + callwait_Mean + callwait_Range + ccrndmou_Range + 
           adjqty + rev_Mean + ovrmou_Mean + avgqty + age1 + age2 + 
           hnd_price + actvsubs + uniqsubs + datovr_Range + adjmou + 
           adjrev + plcd_dat_Mean + crclscod + asl_flag + mouR_Factor + 
           change_mF + F_months + F_eqpdays + F_iwylis_Vmean, family = "binomial", 
         data = Train)
car::vif(LGMF)
#adjmou, avgqty and adjqty have very high VIF with Df considered GVIF is low enough..

summary(LGMF)

Pred=predict(LGMF, Test, type = "response")
options(scipen = 9999)
L=data.frame(LogOfOdds=round(exp(coef(LGMF)),3))
L$Variable=row.names(L)
row.names(L)=NULL
L=L[,c(2,1)]

#variables with more than 50% probability of changing the decision of the customer for every
#1 unit change in the respective independent variable
L%>%arrange(desc(LogOfOdds))%>%filter(LogOfOdds>=1)%>%mutate(Probability=round(LogOfOdds/(1+LogOfOdds),3))

Pred.class=ifelse(Pred>0.24,1,0)
CM=confusionMatrix(as.factor(Pred.class),Test$churn)
CM$table
fourfoldplot(CM$table)
Acc_Log24=CM$overall[[1]]
Sensitivity_Log24=CM$byClass[[1]]
Specificity_Log24=CM$byClass[[2]]
F1sq_Log24=CM$byClass[[7]]
library(ROCR)
Pred.Storage=prediction(Pred,Test$churn)

AUC=performance(Pred.Storage,"auc")
AUC_Log24=AUC@y.values[[1]]

#plot Specificity
perfspec <- performance(prediction.obj = Pred.Storage,
                        measure="spec",
                        x.measure="cutoff")
plot(perfspec)
abline(v=c(0.20,0.24,0.28,0.32), col=c("red","green","black","pink"))
#plot Sensitivity
par(new=TRUE)
perfsens <- performance(prediction.obj = Pred.Storage,
                        measure="sens",
                        x.measure="cutoff")
plot(perfsens)

perf=performance(Pred.Storage,"tpr","fpr")

plot(perf)

###########################
options(scipen = 999)
cut_offs=data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], tpr=perf@y.values[[1]])
cut_offs=cut_offs[order(cut_offs$tpr, decreasing = TRUE),]
library(dplyr)
cut_offs%>%filter(fpr<=0.42,tpr>0.59)

#cutoff of 0.2356 seems to give the highest tpr and relatively low fpr

Pred.class235=ifelse(Pred>0.235,1,0)
CM=confusionMatrix(as.factor(Pred.class235),Test$churn)
fourfoldplot(CM$table)
Acc_Log23.5=CM$overall[[1]]
Sensitivity_Log23.5=CM$byClass[[1]]
Specificity_Log23.5=CM$byClass[[2]]

#Choice of cutoff at 24,25, 26 with increasing accuracy and sensitivity but decreasing Specificity
AUC=performance(Pred.Storage,"auc")
AUC_Log23.5=AUC@y.values[[1]]
F1sq_Log23.5=CM$byClass[[7]]
############
Pred.class=ifelse(Pred>0.25,1,0)
CM=confusionMatrix(as.factor(Pred.class),Test$churn)
fourfoldplot(CM$table)
Acc_Log25=CM$overall[[1]]
Sensitivity_Log25=CM$byClass[[1]]
Specificity_Log25=CM$byClass[[2]]
F1sq_Log25=CM$byClass[[7]]
AUC=performance(Pred.Storage,"auc")
AUC_Log25=AUC@y.values[[1]]
################################
Pred.class=ifelse(Pred>0.26,1,0)
CM=confusionMatrix(as.factor(Pred.class),Test$churn)
fourfoldplot(CM$table)
Acc_Log26=CM$overall[[1]]
Sensitivity_Log26=CM$byClass[[1]]
Specificity_Log26=CM$byClass[[2]]
F1sq_Log26=CM$byClass[[7]]
#Choice of cutoff at 24,25, 26 with increasing accuracy and sensitivity but decreasing Specificity
AUC=performance(Pred.Storage,"auc")
AUC_Log26=AUC@y.values[[1]]

##################################
names(Train)
library(MASS)
LDA_p56=lda(churn~., data = Train,prior=c(0.56,0.44))
Pred=predict(LDA_p56,Test)
CM=confusionMatrix(Pred$class,Test$churn)
Acc_LDA_56=CM$overall[[1]]
Sensitivity_LDA56=CM$byClass[[1]]
Specificity_LDA56=CM$byClass[[2]]
Pred.Storage=prediction(as.numeric(Pred$class),Test$churn)
AUC=performance(Pred.Storage,"auc")
AUC_LDA56=AUC@y.values[[1]]
F1sq_LDA56=CM$byClass[[7]]
####################
LDA_p58=lda(churn~., data = Train,prior=c(0.58,0.42))
PredLDA58=predict(LDA_p58,Test)
CM=confusionMatrix(PredLDA58$class,Test$churn)
Acc_LDA_58=CM$overall[[1]]
Sensitivity_LDA58=CM$byClass[[1]]
Specificity_LDA58=CM$byClass[[2]]
Pred.Storage=prediction(as.numeric(PredLDA58$class),Test$churn)
AUC=performance(Pred.Storage,"auc")
AUC_LDA58=AUC@y.values[[1]]
F1sq_LDA58=CM$byClass[[7]]
#######################################
LDA_p54=lda(churn~., data = Train,prior=c(0.54,0.46))
Pred=predict(LDA_p54,Test)
CM=confusionMatrix(Pred$class,Test$churn)
Acc_LDA_54=CM$overall[[1]]
Sensitivity_LDA54=CM$byClass[[1]]
Specificity_LDA54=CM$byClass[[2]]
F1sq_LDA54=CM$byClass[[7]]
Pred.Storage=prediction(as.numeric(Pred$class),Test$churn)
AUC=performance(Pred.Storage,"auc")
AUC_LDA54=AUC@y.values[[1]]
#############
LDA_p52=lda(churn~., data = Train,prior=c(0.52,0.48))
Pred=predict(LDA_p52,Test)
CM=confusionMatrix(Pred$class,Test$churn)
Acc_LDA_52=CM$overall[[1]]
Sensitivity_LDA52=CM$byClass[[1]]
Specificity_LDA52=CM$byClass[[2]]
F1sq_LDA52=CM$byClass[[7]]
Pred.Storage=prediction(as.numeric(Pred$class),Test$churn)
AUC=performance(Pred.Storage,"auc")
AUC_LDA52=AUC@y.values[[1]]

########RF
library(randomForest)
#the target variable has only 26% of the levels as 1 and hence randomforest model
#gets negatively affected by the imbalance. SMOTE technique is used to have more
#examples of the less represented level of the target variable
library(ROSE)
set.seed(3465)
d.Rose=ROSE(churn~., data = Telecom_Winsor, seed = 1)$data
prop.table(table(d.Rose$churn))
dim(Telecom_Winsor)
set.seed(123)
Index=createDataPartition(d.Rose$churn, p=0.75, list = FALSE, times = 1)
Train.R=d.Rose[Index,]
Test.R=d.Rose[-Index,]

set.seed(12345)
RF=randomForest(churn~., data = Train.R)
randomForest::varImpPlot(RF)
Pred=predict(RF,Test.R)
CM=confusionMatrix(Pred,as.factor(Test.R$churn))
CM$table
Acc_RF1=CM$overall[[1]]
Sensitivity_RF1=CM$byClass[[1]]
Specificity_RF1=CM$byClass[[2]]
#Remove hnd_webcap, asl_flag, crclscod
F1sq_RF1=CM$byClass[[7]]
Pred.Storage=prediction(as.numeric(Pred),Test$churn)
AUC=performance(Pred.Storage,"auc")
AUC_RF1=AUC@y.values[[1]]
##############
Compare=data.frame(Models=c("LG_23.5","LG_24","LG_25","LG_26","LDA54","LDA56","LDA58","RF1"),
                   Accuracy=round(c(Acc_Log23.5,Acc_Log24,Acc_Log25,Acc_Log26,Acc_LDA_54,Acc_LDA_56,Acc_LDA_58,Acc_RF1),2),
                   Sensitivity=round(c(Sensitivity_Log23.5,Sensitivity_Log24,Sensitivity_Log25,Sensitivity_Log26,Sensitivity_LDA54,Sensitivity_LDA56,Sensitivity_LDA58,Sensitivity_RF1),2),
                   Specificity=round(c(Specificity_Log23.5,Specificity_Log24,Specificity_Log25,Specificity_Log26,Specificity_LDA54,Specificity_LDA56,Specificity_LDA58,Specificity_RF1),2),
                   AUC=round(c(AUC_Log23.5,AUC_Log24,AUC_Log25,AUC_Log26,AUC_LDA54,AUC_LDA56,AUC_LDA58,AUC_RF1),2),
                   F1=round(c(F1sq_Log23.5,F1sq_Log24,F1sq_Log25,F1sq_Log26,F1sq_LDA54,F1sq_LDA56,F1sq_LDA58,F1sq_RF1),2))

Compare
##############
library(reshape)
ggplot(melt(Compare,id.vars = "Models"),aes(Models,value, col=variable, group=variable))+geom_line()+
  geom_point(size=4,shape=21,fill="white")+scale_y_continuous(breaks = seq(0,1,0.25))+
  labs(x="",y="Values", title="Evaluation Metric Comparison", color="Metrics")+
  theme(legend.key = element_rect(colour = "black", fill = "light blue"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#################
library(gains)
Telecom_Winsor_Lg$Cust_ID=mydata$Customer_ID
set.seed(1234)
Index=createDataPartition(Telecom_Winsor_Lg$churn,times = 1,p=0.75,list = FALSE)
Train=Telecom_Winsor_Lg[Index,]
Test=Telecom_Winsor_Lg[-Index,]

LGMF=glm(formula = churn ~ mou_Mean + totmrc_Mean + rev_Range + drop_blk_Mean + 
           drop_vce_Range + callwait_Mean + callwait_Range + ccrndmou_Range + 
           adjqty + rev_Mean + ovrmou_Mean + avgqty + age1 + age2 + 
           hnd_price + actvsubs + uniqsubs + datovr_Range + adjmou + 
           adjrev + plcd_dat_Mean + crclscod + asl_flag + mouR_Factor + 
           change_mF + F_months + F_eqpdays + F_iwylis_Vmean, family = "binomial", 
         data = Telecom_Winsor)

gains(as.numeric(Telecom_Winsor$churn),predict(LGMF,type="response",newdata=Telecom_Winsor[,-42]),groups = 10)
Telecom_Winsor$Cust_ID=mydata$Customer_ID

Telecom_Winsor$prob<-predict(LGMF,type="response",newdata=Telecom_Winsor[,-42])

quantile(Telecom_Winsor$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

targeted=Telecom_Winsor%>%filter(prob>0.3224491 & prob<=0.8470540)%>%dplyr::select(Cust_ID)
dim(targeted)
write.csv(targeted,"targetedCustomers.csv")
##############################
library(caretEnsemble)
#OneHot Encoding
#Converting every categorical variable to numerical using dummy variables
dmy <- dummyVars(" ~ .", data = Telecom_Winsor,fullRank = T)
Telecom_transformed <- data.frame(predict(dmy, newdata = Telecom_Winsor))
#convert the target variable to categorical
str(Telecom_transformed)
Telecom_transformed$churn=as.factor(Telecom_transformed$churn.1)
Telecom_transformed=subset(Telecom_transformed,select=-churn.1)

set.seed(123)
Index=createDataPartition(Telecom_transformed$churn,p=0.7,list = FALSE)
train=Telecom_transformed[Index,]
test=Telecom_transformed[-Index,]
names(train)
train.x=train[,1:47]
train.y=train[,48]
test.x=test[,1:47]
test.y=test[,48]
table(train$churn)
levels(train.y)=c("No","Yes")
levels(test.y)=c("No","Yes")


fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final', # To save out of fold predictions for best parameter combinantions
  classProbs = T # To save the class probabilities of the out of fold predictions
)

model_rf=train(train.x,train.y,method='rf',
               trControl=fitControl,tuneLength=3, n.trees=250)
model_nn=train(train.x, train.y, method="nnet", 
                trControl=fitControl, tuneLength=2)
model_knn=train(train.x,train.y,method='knn',
                trControl=fitControl,tuneLength=3)
model_svm=train(train.x,train.y,method='svmRadial',
                trControl=fitControl, tuneLength=3)
model_glm=train(train.x,train.y,method='glm',
                trControl=fitControl)

#Predicting the out of fold prediction probabilities for training data
train$OOF_pred_rf<-model_rf$pred$Y[order(model_rf$pred$rowIndex)]
train$OOF_pred_knn<-model_nn$pred$Y[order(model_knn$pred$rowIndex)]
train$OOF_pred_nn<-model_nn$pred$Y[order(model_nn$pred$rowIndex)]
train$OOF_pred_svm<-model_svm$pred$Y[order(model_svm$pred$rowIndex)]
train$OOF_pred_glm<-model_glm$pred$Y[order(model_glm$pred$rowIndex)]

test$OOF_pred_rf<-predict(model_rf,test.x,type='prob')$Y
test$OOF_pred_knn<-predict(model_knn,test.x,type='prob')$Y
test$OOF_pred_nn<-predict(model_nn,test.x,type='prob')$Y
test$OOF_pred_svm<-predict(model_svm,test.x,type='prob')$Y
test$OOF_pred_glm<-predict(model_glm,test.x,type='prob')$Y
############

#Taking average of predictions
test$pred_avg<-(test$OOF_pred_rf+test$OOF_pred_knn+
                  test$OOF_pred_svm+test$OOF_pred_nn+test$OOF_pred_glm)/5

#Splitting into binary classes at 0.252
test$pred_avg<-as.factor(ifelse(test$pred_avg>0.252,'Yes','No'))

CM=confusionMatrix(test$pred_avg,test.y)
CM$table
fourfoldplot(CM$table)
Acc_StAv=CM$overall[[1]]
Acc_StAv
Sensitivity_StAv=CM$byClass[[1]]
Sensitivity_StAv
Specificity_StAv=CM$byClass[[2]]
Specificity_StAv
F1sq_StAv=CM$byClass[[7]]
F1sq_StAv
Pred.Storage=prediction(as.numeric(test$pred_avg),test.y)

AUC=performance(Pred.Storage,"auc")
AUC_StAv=AUC@y.values[[1]]
AUC_StAv
##################
library(xgboost)

train.xgb = xgb.DMatrix(as.matrix(train.x))

test.xgb = xgb.DMatrix(as.matrix(test.x))

fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final', # To save out of fold predictions for best parameter combinantions
  classProbs = T # To save the class probabilities of the out of fold predictions
)

tune_grid <- expand.grid(
  nrounds = seq(100, 200, 50),
  eta = c(0.2, 0.3, 0.4,0.5),
  max_depth = c(3, 4, 5),
  gamma = c(5,6),
  colsample_bytree = c(0.5,0.6,0.8),
  min_child_weight = 1,
  subsample = 1
)

xgb_tune = train(
  x = train.xgb,
  y = train.y ,
  trControl = fitControl,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)
xgb_tune$bestTune
Pred=predict(xgb_tune,test.xgb)
CM=confusionMatrix(Pred,test.y)
fourfoldplot(CM$table)
Acc_xgb=CM$overall[[1]]
Acc_xgb
Sensitivity_xgb=CM$byClass[[1]]
Sensitivity_xgb
Specificity_xgb=CM$byClass[[2]]
Specificity_xgb
F1sq_xgb=CM$byClass[[7]]
F1sq_xgb
Pred.Storage=prediction(as.numeric(test$pred_avg),test.y)

AUC=performance(Pred.Storage,"auc")
AUC_xgb=AUC@y.values[[1]]
AUC_xgb
#############
Compare=data.frame(Models=c("LG_23.5","LG_24","LG_25","LG_26","LDA_54","LDA_56","LDA_58","RF1","Stacked-Avg", "Xgboost"),
                   Accuracy=round(c(Acc_Log23.5,Acc_Log24,Acc_Log25,Acc_Log26,Acc_LDA_54,Acc_LDA_56,Acc_LDA_58,Acc_RF1, Acc_StAv,Acc_xgb),2),
                   Sensitivity=round(c(Sensitivity_Log23.5,Sensitivity_Log24,Sensitivity_Log25,Sensitivity_Log26,Sensitivity_LDA54,Sensitivity_LDA56,Sensitivity_LDA58,Sensitivity_RF1,Sensitivity_StAv,Sensitivity_xgb),2),
                   Specificity=round(c(Specificity_Log23.5,Specificity_Log24,Specificity_Log25,Specificity_Log26,Specificity_LDA54,Specificity_LDA56,Specificity_LDA58,Specificity_RF1,Specificity_StAv,Specificity_xgb),2),
                   AUC=round(c(AUC_Log23.5,AUC_Log24,AUC_Log25,AUC_Log26,AUC_LDA54,AUC_LDA56,AUC_LDA58,AUC_RF1,AUC_StAv,AUC_xgb),2),
                   F1=round(c(F1sq_Log23.5,F1sq_Log24,F1sq_Log25,F1sq_Log26,F1sq_LDA54,F1sq_LDA56,F1sq_LDA58,F1sq_RF1,F1sq_StAv,F1sq_xgb),2))

Compare
##############

library(reshape)
ggplot(melt(Compare,id.vars = "Models"),aes(Models,value, col=variable, group=variable))+geom_line()+
  geom_point(size=4,shape=21,fill="white")+scale_y_continuous(breaks = seq(0.4,1,0.05))+
  labs(x="",y="Values", title="Evaluation Metric Comparison", color="Metrics")+
  theme(legend.key = element_rect(colour = "black", fill = "light blue"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 15, hjust = 0.5))

ggplot(melt(Compare,id.vars = "Models"),aes(x=variable,value, fill=Models))+
  geom_bar(stat = "identity", position = "dodge")+coord_flip()+
  labs(x="",y="Values", title="Evaluation Metric Comparison", color="Metrics")+
  theme(legend.key = element_rect(colour = "black", fill = "light blue"),
        axis.text.y = element_text(size = 10, hjust = 1, face = "bold"),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.key.size = unit(0.5,"cm"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey"))
