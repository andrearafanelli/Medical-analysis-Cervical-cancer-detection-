#BASSA PROPORZIONE DI 1
#VARIABILI MOLTO LEGATE TRA LORO 

data<-read.csv('risk.csv',na.strings = "?")
summary(is.na(data))
data<-data[,-c(27,28)]
str(data)
data<-data[,-c(7,15,22,27,28,29)]

#AVOID NA's
data$STDs[is.na(data$STDs)]<-1

data$STDs.HPV[is.na(data$STDs.HPV)]<-1
data$STDs.condylomatosis[is.na(data$STDs.condylomatosis)]<-0
data$STDs.vaginal.condylomatosis[is.na(data$STDs.vaginal.condylomatosis)]<-0
data$STDs.vulvo.perineal.condylomatosis[is.na(data$STDs.vulvo.perineal.condylomatosis)]<-0
data$STDs.syphilis[is.na(data$STDs.syphilis)]<-0
data$STDs.pelvic.inflammatory.disease[is.na(data$STDs.pelvic.inflammatory.disease)]<-0
data$STDs.genital.herpes[is.na(data$STDs.genital.herpes)]<-0
data$STDs.molluscum.contagiosum[is.na(data$STDs.molluscum.contagiosum)]<-0
data$STDs.HIV[is.na(data$STDs.HIV)]<-0
data$STDs.Hepatitis.B[is.na(data$STDs.Hepatitis.B)]<-0
data$Hormonal.Contraceptives[is.na(data$Hormonal.Contraceptives)]<-1
data$Smokes[is.na(data$Smokes)]<-1
data$IUD[is.na(data$IUD)]<-1


data$STDs..Number.of.diagnosis[is.na(data$STDs..Number.of.diagnosis)]<-median(data$STDs..Number.of.diagnosis,na.rm=T)
data$First.sexual.intercourse[is.na(data$First.sexual.intercourse)]<-median(data$First.sexual.intercourse,na.rm = T)

data$Smokes..years.[is.na(data$Smokes..years.)]<-median(data$Smokes..years.,na.rm=T)
data$STDs..number.[is.na(data$STDs..number.)]<-median(data$STDs..number.,na.rm =T)
data$Number.of.sexual.partners[is.na(data$Number.of.sexual.partners)]<-median(data$Number.of.sexual.partners,na.rm=T)
data$Hormonal.Contraceptives..years.[is.na(data$Hormonal.Contraceptives..years.)]<-median(data$Hormonal.Contraceptives..years.,na.rm=T)
data$Num.of.pregnancies[is.na(data$Num.of.pregnancies)]<-median(data$Num.of.pregnancies,na.rm=T)
data$IUD..years.[is.na(data$IUD..years.)]<-median(data$IUD..years.,na.rm=T)

#SET VARIABLES
data$Biopsy<-as.factor(data$Biopsy)
data$Smokes<-as.factor(data$Smokes)
data$Hormonal.Contraceptives<-as.factor(data$Hormonal.Contraceptives)
data$Citology<-as.factor(data$Citology)
data$IUD<-as.factor(data$IUD)
data$Dx<-as.factor(data$Dx)
data$Schiller<-as.factor(data$Schiller)
data$Hinselmann<-as.factor(data$Hinselmann)
data$STDs<-as.factor(data$STDs)
data$STDs.HIV<-as.factor(data$STDs.HIV)
data$STDs.Hepatitis.B<-as.factor(data$STDs.Hepatitis.B)
data$STDs.HPV<-as.factor(data$STDs.HPV)
data$STDs.genital.herpes<-as.factor(data$STDs.genital.herpes)
data$STDs.condylomatosis<-as.factor(data$STDs.condylomatosis)#given by hpv 
data$STDs.syphilis<-as.factor(data$STDs.syphilis)
data$STDs.vaginal.condylomatosis<-as.factor(data$STDs.vaginal.condylomatosis)
data$STDs.pelvic.inflammatory.disease<-as.factor(data$STDs.pelvic.inflammatory.disease)
data$STDs.vulvo.perineal.condylomatosis<-as.factor(data$STDs.vulvo.perineal.condylomatosis)
data$STDs.molluscum.contagiosum<-as.factor(data$STDs.molluscum.contagiosum)

data$Number.of.sexual.partners<-as.numeric(paste(data$Number.of.sexual.partners))
data$Number.of.sexual.partners<-round(data$Number.of.sexual.partners,0)
data$First.sexual.intercourse<-as.numeric(paste(data$First.sexual.intercourse))
data$First.sexual.intercourse<-round(data$First.sexual.intercourse,0)
data$Num.of.pregnancies<-as.numeric(data$Num.of.pregnancies)
data$Num.of.pregnancies<-round(data$Num.of.pregnancies,0)
data$Smokes..years.<-as.numeric(paste(data$Smokes..years.))
data$Smokes..years.<-round(data$Smokes..years.,0)
data$Hormonal.Contraceptives..years.<-as.numeric(paste(data$Hormonal.Contraceptives..years.))
data$Hormonal.Contraceptives..years.<-round(data$Hormonal.Contraceptives..years.,0)
data$IUD..years.<-as.numeric(paste(data$IUD..years.))
data$IUD..years.<-round(data$IUD..years.,0)
data$STDs..number.<-as.numeric(paste(data$STDs..number.))
data$STDs..number.<-round(data$STDs..number,0)
str(data)


#COLUMN NAMES:
data<-setNames(data,c('Age','Number_partners','Years_first_sex','Number_pregnancies','Smoke','Years_smoke','Hormonal_contrac','Years_HC','IUD','Years_IUD','STD','Number_STD','Condylomatosis','Condy_Vaginal','Condy_Vulvo','Syphilis','Pelvic_infiammatory','Genital_herpes','Molluscum_cont','HIV','HepatitisB','HPV','Num_Diagnosis','DX','Hinselmann_test','Schiller','Citology','Biopsy'))

prop.table(table(data$Biopsy))
library(DMwR)
set.seed(123)
data1 <- SMOTE(Biopsy ~ ., data, perc.over = 300,perc.under=400)
data1
###############################
#EDA
library(ggplot2)

ggplot(data1,aes(x=Smoke))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='sienna2')+
  facet_wrap(~Biopsy)+
  theme_minimal()+
  labs(fill='Cancer',y='Frequency',x='Smoke')+
  ggtitle('Distribution of smoke \n by cervical cancer')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))

#HC
ggplot(data1,aes(x=Biopsy,fill=Biopsy,y=Years_HC))+
  geom_boxplot()+
  theme_minimal()+
  labs(fill='Cancer',y='Years of hormonal contraceptive',x='Cancer')+
  ggtitle('Distribution of cervical cancer\n by years of hormonal contraceptive')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))

ggplot(data1,aes(x=HIV))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),fill='plum2',colour='black')+
  facet_wrap(~Biopsy)+
  theme_minimal()+
  labs(fill='Biopsy',y='Frequency',x='HIV')+
  ggtitle('Distribution of HIV \n by cervical cancer')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))

ggplot(data1,aes(x=HPV))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='paleturquoise1')+
  facet_wrap(~Biopsy)+
  theme_minimal()+
  labs(fill='Biopsy',y='Frequency',x='HPV')+
  ggtitle('Distribution of HPV \n by cervical cancer')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))

#Num. partners

#NUMBER DIAGNOSIS: ECCO PERCHE' LA TOGLIAMO:
ggplot(data1,aes(x=Biopsy,fill=Biopsy,y=Num_Diagnosis))+
  geom_boxplot()+
  theme_minimal()+
  labs(fill='Cancer',y='Number of diagnosis',x='Cancer')+
  ggtitle('Distribution of number of diagnosis\n by cancer')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#VARIABILI CHE TOGLIAMO:
ggplot(data1,aes(x=Molluscum_cont))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='turquoise3')+  theme_minimal()+
  labs(fill='Cancer',y='Frequency',x='Molluscum')+
  ggtitle('Frequency of molluscum')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))

ggplot(data1,aes(x =HepatitisB)) +
  facet_wrap(~Biopsy)+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"), fill='tan2',colour='black')+
  theme_minimal()+
  labs(fill='Biopsy',y='Frequency',x='Hepatitis B')+
  ggtitle('Frequency of \nhepatitis B\n by cervical cancer')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))+
  scale_fill_continuous(type = "viridis")

ggplot(data1,aes(x=Genital_herpes))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='violetred')+  theme_minimal()+
  labs(fill='Cancer',y='Frequency',x='Genital herpes')+
  ggtitle('Frequency of Genital herpes')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))

ggplot(data1,aes(x=Pelvic_infiammatory))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='green')+  theme_minimal()+
  labs(fill='Cancer',y='Frequency',x='Pelvic infiammatory')+
  ggtitle('Frequency of Pelvic infiammatory')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))

table(data1$Pelvic_infiammatory)

ggplot(data1,aes(x=STD,fill=STD,y=Number_partners))+
  geom_boxplot()+
  theme_minimal()+
  labs(fill='Sexually\n trasmitted\n disease',y='Number partners',x='Sexually trasmitted disease')+
  ggtitle('Distribution of STD\n by number of partners')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic')) 

ggplot(data1,aes(x =IUD)) +
  facet_wrap(~Biopsy)+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"), fill='springgreen',colour='black')+
  theme_minimal()+
  labs(fill='Biopsy',y='Frequency',x='IUD')+
  ggtitle('Frequency of \nIUD\n by cervical cancer')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))+
  scale_fill_continuous(type = "viridis")

ggplot(data1,aes(x =STD)) +
  facet_wrap(~Biopsy)+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"), fill='steelblue',colour='black')+
  theme_minimal()+
  labs(fill='Biopsy',y='Frequency',x='Sexually trasmitted disease')+
  ggtitle('Frequency of \nsexually trasmitted disease\n by cervical cancer')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))+
  scale_fill_continuous(type = "viridis")

#CORR PLOT
corr<-cor(data1[,c(1,2,3,4,6,8,10,12,23)])


library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)
#VERIFICA INTERAZIONI VARIABILI 
#CREO DATASET SENZA I TEST:SCHILLER,HINSELMANN,CITOLOGY,GENITAL HERPES,CONDY VAGINAL,CONDY,PELVIC INFIAMM
newData<-data1[,-c(14,15,17,18,19,21,23,25,26,27)]

###############################
############################àà
#DOBBIAMO FARLO PER UN PROBLEMA NEL DATASET 
keep <- function (x) {
  if (is.factor(x) || is.character(x)) {
    length(unique(x[!is.na(x)])) > 1
  } else TRUE
}

newData <- newData[sapply(newData, keep)]

library(caTools)
set.seed(123)
split = sample.split(newData, SplitRatio = 0.7)
train = subset(newData, split == TRUE)
test = subset(newData, split == FALSE)


########
#TROPPE VARIABILI PROVIAMO LA STEP:
glm.null=glm(Biopsy~1,data=newData,family='binomial')
glm.full=glm(Biopsy~.,data=newData,family ='binomial')
summary(glm.null)
summary(glm.full)

#Step forward
glm_for=step(glm.null,scope=list(lower=formula(glm.null),upper=formula(glm.full)),
              direction='forward',trace=2)

glm_back=step(glm.null,scope=list(lower=formula(glm.null),upper=formula(glm.full)),
             direction='backward',
             trace=0)
glm_both=step(glm.null,scope=list(lower=formula(glm.null),upper=formula(glm.full)),
                      direction='both',
                      trace=0)
a=summary(glm_for)
a$iter
summary(glm_for)#Deviance is less
summary(glm_both)#Deviance is less
summary(glm_back)#Deviance is high

formula(glm_for)
formula(glm_both)
#SAME RESULTS BOTH AND FOR 
glm1<-glm(Biopsy ~ Years_HC + Number_STD + DX + Syphilis + IUD + Age + 
            HIV + STD + Hormonal_contrac + Number_partners + HPV + Years_smoke,data=train,family=binomial)
glm1_pred<-predict(glm1,test,type=c('response'))
plot(glm1_pred)

#ROC
library(pROC)

g <- roc( test$Biopsy~ glm1_pred,algorithm=1)
plot(g) 
auc(g)
 
#THRESHOLD
matplot(data.frame(g$sensitivities, g$specificities), x = g$thresholds, type='l', xlab = 'threshold', ylab='TPR, TNR')
legend('bottomright', legend=c('TPR', 'TNR'), lty=1:2, col=1:2)

coords(g, "best", "threshold",transpose = T)
a=summary(glm_for)
a$terms
ths = seq(0,0.8,by=0.01) #thresholds 
err.rate = c() # overall error rate
err.yes= c() # err rate among defaults#fpr 
err.no = c() # err rate among no-def FNR =1-specificity 
for(th in ths){
  estimatedResponses=ifelse(glm1_pred>th,"1","0")
  x<-table(estimatedResponses,test$Biopsy)
  err = mean(estimatedResponses!=test$Biopsy)
  err.rate = c(err.rate,err)
  cond = test$Biopsy=="1"
  err = mean(estimatedResponses[cond]=="No") 
  err.yes = c(err.yes,err)
  err = mean(estimatedResponses[!cond]=="Yes") 
  err.no = c(err.no,err)
}
plot(th,err.rate,type="l",lwd=2) 
lines(th,err.yes,lty="dashed",col=4,lwd=2) 
lines(th,err.no,lty="dotted",col=2,lwd=2)

#CONFUSION MATRIX 
estimatedResponses=ifelse(glm1_pred> 0.1832281,"True","False")
library(xtable)
addmargins(table(test$Biopsy,estimatedResponses))
addmargins()
x

#0.29 THRESHOLD: 81% SPEC, 67% SENS
acc<-(161+44)/(264)
#78% ACCURACY

########################ù
train2<-train[,-c(5,7,9,11)]
test2<-test[,-c(5,7,9,11)]
glm2<-glm(Biopsy~.,data=train2,family='binomial')
glm2_pred<-predict(glm2,test2,type=c('response'))

#ROC
library(pROC)

g <- roc( test2$Biopsy~ glm2_pred,algorithm=1)
plot(g) 
auc(g)

#THRESHOLD
matplot(data.frame(g$sensitivities, g$specificities), x = g$thresholds, type='l', xlab = 'threshold', ylab='TPR, TNR')
legend('bottomright', legend=c('TPR', 'TNR'), lty=1:2, col=1:2)

coords(g, "best", "threshold",transpose = T)

#CONFUSION MATRIX 
estimatedResponses=ifelse(glm1_pred>0.1935504,"True","False")
x2<-table(estimatedResponses,test$Biopsy)
x2

#CLASSIFICATION TREE
library(tree)
fit=tree(Biopsy~.,data=newData,method="Gini")
summary(fit)
plot(fit, col='red')
text(fit,pretty=3,col='red',cex=1.2)

cv.fit=cv.tree(fit, FUN =prune.misclass,K=length(newData$Biopsy))#LOOCV 
summary(cv.fit)
plot(cv.fit$size,cv.fit$dev,type="b",
     xlab = 'Size',ylab = 'Deviance')
cv.fit$size
which.min(cv.fit$dev)
#PRUNE
prune.fit=prune.misclass(fit,best=12)
#BEST TREE
plot(prune.fit,col='blue')
text(prune.fit,pretty=3,col='black',cex=1.2)
#YEARS HC , NUMER STD 
####VISUALIZE better 
#######################################################àà

###################################
#KNN
#CV
library(e1071)
library(caret)
trctrl <- trainControl(method = "cv", number = 10)
set.seed(3333)
#AVOID VARIABLES WITH ZERO VARIANCE:
knn_fit <- train(Biopsy ~., data = train, method = "knn",
                 trControl=trctrl,
                 preProcess = 'scale',
                 tuneLength = 25)
knn_fit

#BEST WITH K=5
plot(knn_fit)
#ACCURACY 85%
##########

library(randomForest)
set.seed (123)

########################################

############################################
oob.err=double(17)
test.err=double(17)
set.seed(123)
for(mtry in 1:17){
  rf=randomForest(Biopsy~ . , data =train ,mtry=mtry,ntree=500) 
  pred<-predict(rf,test) #Predictions on Test Set for each Tree
  tt = addmargins(table(pred,test$Biopsy))
  ACC <- (tt[1,1]+tt[2,2])/tt[3,3] 
  test.err[mtry]=with(test,1-ACC)
  oob.err[mtry] = rf$err.rate[,1][500]
  cat(mtry," ") #printing the output to the console
  
}
which.min(test.err)
oob.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

d = ncol(train)-1
#bagging
d## 23# by default in classification cases random Forest use var sqrt(m):m num of var: bagging is the particular case of rf m=d 
set.seed(123)
bag.boston=randomForest(Biopsy~.,data=train, mtry= 15,importance =TRUE)## if m==p allora è bagging
bag.boston
#bag.boston$err.rate
#bag.boston$confusion
# bag.boston$votes

yhat.bag = predict(bag.boston ,newdata=test,type='class')
plot(yhat.bag, test$Biopsy, col= c('blue','turquoise'))
abline (0 ,1, col='red',lwd=4)
t = addmargins(table(yhat.bag,test$Biopsy))
Acc <- (t[1,1]+t[2,2])/t[3,3]  
Acc## 90%
importance(bag.boston)#The former is based upon the mean decrease of accuracy in 
#predictions on the out of bag samples when a given variable is excluded from the model. The latter is a measure of the total decrease in node impurity that results from splits over that variable, averaged over all trees (this was plotted in Figure 8.9). In the case of regression trees, the node impurity 
#is measured by the training RSS, and for classification trees by the deviance.

varImpPlot(bag.boston)
estimatedResponses=ifelse(yhat.bag>0.5,"True","False")
table(test$Biopsy,estimatedResponses)
########

'rf_model <- train(Biopsy ~ .,
                  method = 'rf',
                  data = train)
print(rf_model)
confusionMatrix(rf_model)
############à
###The plot below displays the effect of mtry in the test err rate

#
for(k in 1:500){
  cancer.rf=randomForest(Biopsy~.,data=newData,subset=tr,mtry=5,importance=T,ntree=k)
  tree.pred=predict(cancer.rf,newData[-tr,])
  tt = addmargins(table(tree.pred,boston.test))
  ACC <- (tt[1,1]+tt[2,2])/tt[3,3] 
  test.err=rbind(test.err,1-ACC)
}
plot(1:500,test.err,type='b', xlab = 'ntree')

ntree=1:500
ntree=[which.min(test.err)]#5.79 in order to obtain the lowest test err rate i should choose as vbs for my rf from subsets of 6 predictors 


require(randomForest)

set.seed(42)
cancer1.rf=randomForest(Biopsy~.,data=train,mtry=5,importance=T)

plot(cancer1.rf)## cè qualcosa di sbagliato' 

###################################à
library(randomForestExplainer)
setwd(Downloads)
explain_forest(bag.boston, interactions = TRUE, data = train)
kable(importance(bag.boston))#From the data above you can see that ShelveLoc is now the most important predictor in terms of MSE (whose absence most increase the training MSE). Moreover, while considering only 9 predictors for training each tree achieves
# lower training MSE the test MSE is higher than the bagging approach.




