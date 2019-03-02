setwd("")
mydata=read.csv("Telecom_Sampled.csv")
dim(mydata)
mydata$churn=as.factor(mydata$churn)
anyNA(mydata)
Percentage_missing=round(colSums(is.na(mydata[,colnames(mydata)[colSums(is.na(mydata))>0]]))/nrow(mydata)*100,2)
data.frame(MissingProportion=sort(Percentage_missing, decreasing = TRUE))

#Finding variable names with more than 30% missing values
Variables_with_High_NAs=colnames(mydata)[colSums(is.na(mydata))/nrow(mydata)>0.3]
Variables_with_High_NAs


#Removing the variables with more than 30% missing values from the original dataset
mydata=mydata[,!names(mydata)%in%Variables_with_High_NAs]
names(mydata)

#13 variables removed

library(Boruta)
library(randomForest)

mydata=mydata[,-1]
#using boruta to estimate variable importance to remove insignificant variables
set.seed(1234)
BT=Boruta(churn~., data = na.omit(mydata), doTrace=2)
write.csv(getSelectedAttributes(BT),"SignificantVariables")

getSelectedAttributes(BT)

Significantvariables=c("mou_Mean","totmrc_Mean","rev_Range","mou_Range","change_mou",
                       "drop_blk_Mean","drop_vce_Range","owylis_vce_Range","mou_opkv_Range","months",
                       "totcalls","eqpdays", "custcare_Mean","callwait_Mean","iwylis_vce_Mean",
                       "callwait_Range"   ,"ccrndmou_Range",   "adjqty" ,    "ovrrev_Mean"  ,    "rev_Mean",
                       "ovrmou_Mean"  ,    "comp_vce_Mean"  ,  "plcd_vce_Mean"  ,  "avg3mou",   "avgmou",
                       "avg3qty"  ,  "avgqty" ,   "avg6mou" ,   "avg6qty"   ,       "crclscod"  ,
                       "asl_flag" , "hnd_webcap"    ,   "age1"  ,           "age2"      ,       "models",
                       "hnd_price"  ,      "actvsubs" ,        "uniqsubs"     ,    "roam_Mean",  "da_Range" ,
                       "datovr_Mean" ,     "datovr_Range" ,    "drop_vce_Mean"  ,  "adjmou" , "totrev",
                       "adjrev" ,          "avgrev"  ,         "Customer_ID"  ,    "plcd_dat_Mean" ,"churn")

#Dataset with only the significant variables

mydata_Rev=mydata[,Significantvariables]
str(mydata_Rev)

#The number of variables reduced to 50

Percentage_missing=round(colSums(is.na(mydata_Rev[,colnames(mydata_Rev)[colSums(is.na(mydata_Rev))>0]]))/nrow(mydata_Rev)*100,2)
sort(Percentage_missing, decreasing = TRUE)

#We still have 18 variables with missing values
mydata_Rev=droplevels(mydata_Rev)
options(scipen = 999)
variable = names(mydata_Rev)

lists=1
mins=1
maxs=1
average=1
SD=1
u=1
i=50
for (i in 1:length(variable)) {
  x=variable[i]
  y=mydata_Rev[,x]
  lists[i]=class(y)
  mins[i]=ifelse(class(y)=="numeric"|class(y)=="integer",min(y,na.rm = TRUE),0)
  maxs[i]=ifelse(class(y)=="numeric"|class(y)=="integer",max(y,na.rm = TRUE),0)
  average[i]=ifelse(class(y)=="numeric"|class(y)=="integer",mean(y,na.rm = TRUE),0)
  SD[i]=ifelse(class(y)=="numeric"|class(y)=="integer",mean(y,na.rm = TRUE),0)
  u[i]=ifelse(class(y)=="factor",length(unique(y,na.rm = TRUE)),0)
  print(i)
}


missingvalues=colSums(is.na(mydata_Rev))

Final_Summary=data.frame(Variable=variable, datatype=lists, Max=maxs,Min=mins,SD=SD, Average=average,Unique=u, Missing=missingvalues)
write.csv(Final_Summary,"DataSummary.csv")
str(mydata_Rev)
#A varibale with 49 levels. Need to reduce the number of levels

table(is.na(mydata_Rev$crclscod))
unique(mydata_Rev$crclscod)
library(dplyr)
mydata_Rev%>%count(churn,levels=crclscod)%>%filter(churn==1)->Dat
Dat$N=unclass(mydata_Rev%>%filter(crclscod%in%Dat$levels)%>%count(crclscod))$n
Dat$ChurnPerc=round(Dat$n/Dat$N,2)*100
table(Dat$ChurnPerc)

#Levels with churnpercent differnce of less than 0.1 can be clubbed to one
L_D5=Dat%>%filter(ChurnPerc>0,ChurnPerc<=10)%>%select(levels)
L_C5=Dat%>%filter(ChurnPerc>10,ChurnPerc<=20)%>%select(levels)
L_A=Dat%>%filter(ChurnPerc>20,ChurnPerc<=30)%>%select(levels)
L_A2=Dat%>%filter(ChurnPerc>30,ChurnPerc<=40)%>%select(levels)
L_A3=Dat%>%filter(ChurnPerc>40,ChurnPerc<=100)%>%select(levels)

mydata_Rev$crclscod=as.factor(ifelse(mydata_Rev$crclscod%in%L_D5[[1]],"A1",
                           ifelse(mydata_Rev$crclscod%in%L_C5[[1]],"A2",
                                  ifelse(mydata_Rev$crclscod%in%L_A[[1]],"A3",
                                         ifelse(mydata_Rev$crclscod%in%L_A2[[1]],"A4",
                                                ifelse(mydata_Rev$crclscod%in%L_A3[[1]],"A4","NoChurn"))))))

#remove customer id as it wont predict churn
mydata_Rev=mydata_Rev[,-48]
names(mydata_Rev)
###Finding highly correlated variables
Numeric=Filter(is.numeric,mydata_Rev)
library(corrplot)
M=cor(na.omit(Numeric))
corrplot(M, method = "circle", type = "lower", 
         tl.srt = 45, tl.col = "black", tl.cex = 0.75)

#Mou_mean seems to have high Covariance with a lot of others
names(Numeric)
cor(na.omit(Numeric[,c(1,5,7,17,18,23:30)]))
#High correlation with ```names(Numeric)[22:29]```
#Variables with high correlation are `"comp_vce_Mean","plcd_vce_Mean","avg3mou","avgmou","avg3qty","avgqty","avg6mou","avg6qty"`
names(mydata_Rev)
mydata_Rev=mydata_Rev[,-c(22:26,28,29)]

###Higly correlated variables are mou_Mean with comp_vce_mean,plcd_vce_mean.
#similarly, we have totcalls with adjqty, which in turn with a few more.

str(mydata_Rev[,colnames(mydata_Rev)[colSums(is.na(mydata_Rev))>0]])


###Missing value imputation
library(VIM)

mydata_imp=kNN(mydata_Rev,variable = colnames(mydata_Rev)[colSums(is.na(mydata_Rev)) > 0],k=5)
colnames(mydata_imp)[colSums(is.na(mydata_imp))>0] 
str(mydata_imp)
names(mydata_imp)
mydata_imp=mydata_imp[,1:42]

write.csv(mydata_imp,"Telecom_Imputed.csv")
Telecom=read.csv("Telecom_imputed.csv")

Telecom=Telecom[,-1]
Telecom$churn=as.factor(Telecom$churn)

dim(Telecom)
names(Telecom)
###########Outlier
Numeric=Filter(is.numeric,Telecom)
dev.off()
y=names(Numeric)
for (i in 1:length(y) ){
  x=boxplot(Numeric[,y[i]]~Telecom$churn,
            main=y[i], col = i ,ylab="Churn",horizontal = TRUE)
  index=which(Telecom[,y[i]]%in%x$out)
  rm(x)
}


Telecom_Rev=Telecom

library(DescTools)
Numeric_Winsorized=data.frame(lapply(Numeric,Winsorize))
# For outlier treatment, Winsorize all numeric variables and then log transform
#the variables which still have outliers. Variables with a minimum value of zero
#or less were not transformed.
Numeric_Winsorized$plcd_dat_Mean=as.factor(ifelse(Numeric$plcd_dat_Mean==0,0,1))
Numeric_Winsorized$avgrev=Winsorize(Numeric$avgrev,probs = c(0.10,0.90))
Numeric_Winsorized$adjrev=log(Numeric_Winsorized$adjrev)
Numeric_Winsorized$totrev=log(Numeric_Winsorized$totrev)
Numeric_Winsorized$adjmou=log(Numeric_Winsorized$adjmou)
Numeric_Winsorized$da_Range=Winsorize(Numeric$da_Range,probs = c(0.00,0.89))
Numeric_Winsorized$drop_vce_Mean=log(Numeric_Winsorized$drop_vce_Mean^2+0.01)

#Numeric_Winsorized$avg6qty=log(Numeric_Winsorized$avg6qty)
#Numeric_Winsorized$avg6mou=log(Numeric_Winsorized$avg6mou)
#Numeric_Winsorized$avgqty=log(Numeric_Winsorized$avgqty)
#Numeric_Winsorized$avg3qty=log(Numeric_Winsorized$avg3qty)
#Numeric_Winsorized$avgmou=log(Numeric_Winsorized$avgmou)
#Numeric_Winsorized$avg3mou=log(Numeric_Winsorized$avg3mou)
Numeric_Winsorized$datovr_Range=as.factor(ifelse(Numeric$datovr_Range==0,0,1))
Numeric_Winsorized$datovr_Mean=as.factor(ifelse(Numeric$datovr_Mean==0,0,1))
Numeric_Winsorized$da_Range=as.factor(ifelse(Numeric$da_Range<=1,0,1))
Numeric_Winsorized$roam_Mean=as.factor(ifelse(Numeric$roam_Mean<=0.25,0,1))
Numeric_Winsorized$avgqty=log(Numeric_Winsorized$avgqty)

#Numeric_Winsorized$plcd_vce_Mean=Winsorize(Numeric$plcd_vce_Mean,probs = c(0.0001,0.90))
#Numeric_Winsorized$comp_vce_Mean=Winsorize(Numeric$comp_vce_Mean,probs = c(0.10,0.90))
Numeric_Winsorized$ovrmou_Mean=log(Numeric_Winsorized$ovrmou_Mean^2+0.01)
Numeric_Winsorized$rev_Mean=log(Numeric_Winsorized$rev_Mean)
Numeric_Winsorized$ovrrev_Mean=log(Numeric_Winsorized$ovrrev_Mean^2+0.01)
Numeric_Winsorized$adjqty=log(Numeric_Winsorized$adjqty)
Numeric_Winsorized$ccrndmou_Range=as.factor(ifelse(Numeric_Winsorized$ccrndmou_Range==0,0,1))
Numeric_Winsorized$callwait_Range=as.factor(ifelse(Numeric_Winsorized$callwait_Range==0,0,1))


Numeric_Winsorized$iwylis_vce_Mean=log(Numeric_Winsorized$iwylis_vce_Mean^2+0.01)
Numeric_Winsorized$callwait_Mean=log(Numeric_Winsorized$callwait_Mean^2+0.01)
Numeric_Winsorized$custcare_Mean= as.factor(ifelse(Numeric_Winsorized$custcare_Mean<=1.5,0,1))
Numeric_Winsorized$totcalls=log(Numeric_Winsorized$totcalls)
Numeric_Winsorized$mou_opkv_Range=Winsorize(Numeric$mou_opkv_Range,probs = c(0.10,0.90))
Numeric_Winsorized$owylis_vce_Range=log(Numeric_Winsorized$owylis_vce_Range^2+0.01)

Numeric_Winsorized$drop_vce_Range=log(Numeric_Winsorized$drop_vce_Range^2+0.01)
Numeric_Winsorized$drop_blk_Mean=log(Numeric_Winsorized$drop_blk_Mean^2+0.01)


Numeric_Winsorized$change_mou=Winsorize(Numeric$change_mou,probs = c(0.10,0.90))
Numeric_Winsorized$mou_Range=log(Numeric_Winsorized$mou_Range^2+0.01)
Numeric_Winsorized$rev_Range=log(Numeric_Winsorized$rev_Range^2+0.01)
Numeric_Winsorized$mou_Mean=log(Numeric_Winsorized$mou_Mean)

N=Filter(is.numeric,Numeric_Winsorized)
y=names(N)
for (i in 1:length(y) ){
  x=boxplot(N[,y[i]]~Telecom$churn,
            main=y[i], col = i ,ylab="Churn",horizontal = TRUE)
  index=which(Telecom[,y[i]]%in%x$out)
  rm(x)
}


#Adding the factor variables
Numeric_Winsorized$hnd_webcap=Telecom$hnd_webcap
Numeric_Winsorized$crclscod=Telecom$crclscod
Numeric_Winsorized$asl_flag=Telecom$asl_flag
Numeric_Winsorized$churn=Telecom$churn

Telecom_Winsor=Numeric_Winsorized
###Profiling continuous variables
library(dplyr)

Telecom_Winsor%>%mutate(Dec=ntile(mou_Mean,10))%>%count(Dec,churn)%>%filter(churn==1)->Profiling
Profiling$N=unclass(Telecom_Winsor%>%mutate(Dec=ntile(mou_Mean,10))%>%count(Dec))[[2]]
Profiling$churPerc=round(Profiling$n/Profiling$N,4)*100
Profiling$GreaterThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(mou_Mean,10))%>%group_by(Dec)%>%summarise(min(mou_Mean)))[[2]]
Profiling$LesserThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(mou_Mean,10))%>%group_by(Dec)%>%summarise(max(mou_Mean)))[[2]]
Profiling

#Churn% higher than 25% for the first 2 deciles. Could create dummy variables


Telecom_Winsor%>%mutate(Dec=ntile(totmrc_Mean,10))%>%count(Dec,churn)%>%filter(churn==1)->Profiling
Profiling$N=unclass(Telecom_Winsor%>%mutate(Dec=ntile(totmrc_Mean,10))%>%count(Dec))[[2]]
Profiling$churPerc=round(Profiling$n/Profiling$N,4)*100
Profiling$GreaterThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(totmrc_Mean,10))%>%group_by(Dec)%>%summarise(min(totmrc_Mean)))[[2]]
Profiling$LesserThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(totmrc_Mean,10))%>%group_by(Dec)%>%summarise(max(totmrc_Mean)))[[2]]
Profiling
#churn percent hovering between 20% to 30% and the bins are clearly delineated
#Dummy variable to be created for churn percent greater than 21 and those less than 21


Telecom_Winsor%>%mutate(Dec=ntile(rev_Range,10))%>%count(Dec,churn)%>%filter(churn==1)->Profiling
Profiling$N=unclass(Telecom_Winsor%>%mutate(Dec=ntile(rev_Range,10))%>%count(Dec))[[2]]
Profiling$churPerc=round(Profiling$n/Profiling$N,4)*100
Profiling$GreaterThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(rev_Range,10))%>%group_by(Dec)%>%summarise(min(rev_Range)))[[2]]
Profiling$LesserThan=unclass(Telecom%>%mutate(Dec=ntile(rev_Range,10))%>%group_by(Dec)%>%summarise(max(rev_Range)))[[2]]
Profiling
#rev_Range has a cluster which has very high values i.e. 10th cluster. No major differences in churn%

str(Telecom)
Telecom_Winsor%>%mutate(Dec=ntile(mou_Range,10))%>%count(Dec,churn)%>%filter(churn==1)->Profiling
Profiling$N=unclass(Telecom_Winsor%>%mutate(Dec=ntile(mou_Range,10))%>%count(Dec))[[2]]
Profiling$churPerc=round(Profiling$n/Profiling$N,4)*100
Profiling$GreaterThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(mou_Range,10))%>%group_by(Dec)%>%summarise(min(mou_Range)))[[2]]
Profiling$LesserThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(mou_Range,10))%>%group_by(Dec)%>%summarise(max(mou_Range)))[[2]]
Profiling
#clearly delineated bins. Dummy variable to be created for churn % greater than 24.5

Telecom_Winsor$mouR_Factor=factor(ifelse(Telecom_Winsor$mou_Range<=7.78 |Telecom_Winsor$mou_Range >12.691 ,"High","Low"))
table(Telecom_Winsor$mouR_Factor)
Telecom_Winsor=subset(Telecom_Winsor,select = -mou_Range)


Telecom_Winsor%>%mutate(Dec=ntile(change_mou,9))%>%count(Dec,churn)%>%filter(churn==1)->Profiling
Profiling$N=unclass(Telecom_Winsor%>%mutate(Dec=ntile(change_mou,9))%>%count(Dec))[[2]]
Profiling$churPerc=round(Profiling$n/Profiling$N,4)*100
Profiling$GreaterThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(change_mou,9))%>%group_by(Dec)%>%summarise(min(change_mou)))[[2]]
Profiling$LesserThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(change_mou,9))%>%group_by(Dec)%>%summarise(max(change_mou)))[[2]]
Profiling
Telecom_Winsor$change_mF=factor(ifelse(Telecom_Winsor$change_mou>=1.50,"Low","High"))
Telecom_Winsor=subset(Telecom_Winsor, select = -change_mou)

Telecom_Winsor%>%mutate(Dec=ntile(drop_blk_Mean,10))%>%count(Dec,churn)%>%filter(churn==1)->Profiling
Profiling$N=unclass(Telecom_Winsor%>%mutate(Dec=ntile(drop_blk_Mean,10))%>%count(Dec))[[2]]
Profiling$churPerc=round(Profiling$n/Profiling$N,4)*100
Profiling$GreaterThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(drop_blk_Mean,10))%>%group_by(Dec)%>%summarise(min(drop_blk_Mean)))[[2]]
Profiling$LesserThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(drop_blk_Mean,10))%>%group_by(Dec)%>%summarise(max(drop_blk_Mean)))[[2]]
Profiling
#Telecom_Winsor$change_mF=factor(ifelse(Telecom_Winsor$change_mou>=80.75 & Telecom_Winsor$change_mou<188,"Low","High"))
#Telecom_Winsor=subset(Telecom_Winsor, select = -change_mou)

Telecom_Winsor%>%mutate(Dec=ntile(mou_opkv_Range,10))%>%count(Dec,churn)%>%filter(churn==1)->Profiling
Profiling$N=unclass(Telecom_Winsor%>%mutate(Dec=ntile(mou_opkv_Range,10))%>%count(Dec))[[2]]
Profiling$churPerc=round(Profiling$n/Profiling$N,4)*100
Profiling$GreaterThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(mou_opkv_Range,10))%>%group_by(Dec)%>%summarise(min(mou_opkv_Range)))[[2]]
Profiling$LesserThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(mou_opkv_Range,10))%>%group_by(Dec)%>%summarise(max(mou_opkv_Range)))[[2]]
Profiling
Telecom_Winsor$F_mou_opkv_Range=factor(ifelse(Telecom_Winsor$mou_opkv_Range>=55.73,"Low","High"))
Telecom_Winsor=subset(Telecom_Winsor, select = -mou_opkv_Range)


Telecom_Winsor%>%mutate(Dec=ntile(months,10))%>%count(Dec,churn)%>%filter(churn==1)->Profiling
Profiling$N=unclass(Telecom_Winsor%>%mutate(Dec=ntile(months,10))%>%count(Dec))[[2]]
Profiling$churPerc=round(Profiling$n/Profiling$N,4)*100
Profiling$GreaterThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(months,10))%>%group_by(Dec)%>%summarise(min(months)))[[2]]
Profiling$LesserThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(months,10))%>%group_by(Dec)%>%summarise(max(months)))[[2]]
Profiling
Telecom_Winsor$F_months=factor(ifelse(Telecom_Winsor$months>10,"High","Low"))
Telecom_Winsor=subset(Telecom_Winsor, select = -months)

Telecom_Winsor%>%mutate(Dec=ntile(eqpdays,10))%>%count(Dec,churn)%>%filter(churn==1)->Profiling
Profiling$N=unclass(Telecom_Winsor%>%mutate(Dec=ntile(eqpdays,10))%>%count(Dec))[[2]]
Profiling$churPerc=round(Profiling$n/Profiling$N,4)*100
Profiling$GreaterThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(eqpdays,10))%>%group_by(Dec)%>%summarise(min(eqpdays)))[[2]]
Profiling$LesserThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(eqpdays,10))%>%group_by(Dec)%>%summarise(max(eqpdays)))[[2]]
Profiling

Telecom_Winsor$F_eqpdays=factor(ifelse(Telecom_Winsor$eqpdays>=277,"High","Low"))
Telecom_Winsor=subset(Telecom_Winsor, select = -eqpdays)

Telecom_Winsor%>%mutate(Dec=ntile(iwylis_vce_Mean,5))%>%count(Dec,churn)%>%filter(churn==1)->Profiling
Profiling$N=unclass(Telecom_Winsor%>%mutate(Dec=ntile(iwylis_vce_Mean,5))%>%count(Dec))[[2]]
Profiling$churPerc=round(Profiling$n/Profiling$N,4)*100
Profiling$GreaterThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(iwylis_vce_Mean,5))%>%group_by(Dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
Profiling$LesserThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(iwylis_vce_Mean,5))%>%group_by(Dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
Profiling

Telecom_Winsor$F_iwylis_Vmean=factor(ifelse(Telecom_Winsor$iwylis_vce_Mean>0.0099,"Low","High"))
Telecom_Winsor=subset(Telecom_Winsor, select = -iwylis_vce_Mean)

Telecom_Winsor$models=as.factor(Telecom_Winsor$models)
Telecom_Winsor$actvsubs=as.factor(Telecom_Winsor$actvsubs)
Telecom_Winsor$uniqsubs=as.factor(Telecom_Winsor$uniqsubs)


Telecom_Winsor%>%mutate(Dec=ntile(drop_vce_Mean,5))%>%count(Dec,churn)%>%filter(churn==1)->Profiling
Profiling$N=unclass(Telecom_Winsor%>%mutate(Dec=ntile(drop_vce_Mean,5))%>%count(Dec))[[2]]
Profiling$churPerc=round(Profiling$n/Profiling$N,4)*100
Profiling$GreaterThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(drop_vce_Mean,5))%>%group_by(Dec)%>%summarise(min(drop_vce_Mean)))[[2]]
Profiling$LesserThan=unclass(Telecom_Winsor%>%mutate(Dec=ntile(drop_vce_Mean,5))%>%group_by(Dec)%>%summarise(max(drop_vce_Mean)))[[2]]
Profiling

Telecom_Winsor$F_drop_vM=factor(ifelse(Telecom_Winsor$drop_vce_Mean < -0.7886796,"High","Low"))
table(Telecom_Winsor$F_drop_vM)
Telecom_Winsor=subset(Telecom_Winsor, select = -drop_vce_Mean)

names(Telecom_Winsor)
Telecom_Winsor=Telecom_Winsor[,c(1:34,36:42,35)]
