library('quantmod')
library('e1071')
library('randomForest')
library('ggplot2')
library('misc3d')
library('rgl')
library('MASS')
library('pROC')
library(httr)
library(XML)



tables <- GET("http://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
tables <- readHTMLTable(rawToChar(tables$content))
t=as.data.frame(tables[1])
ticker=as.vector(t$NULL.Ticker.symbol)
play_ticker=ticker[1:10]
#Ten years data
getSymbols(play_ticker,from='2006-01-01',to='2016-01-01')

#MAKE STOCK MMM AS A DATA FRAME
a1=as.data.frame(AET)
#Close-Open
a1$ZD=a1[,4]-a1[,1]

#Record the decrease as 0 and increase as 1
for (i in 1:nrow(a1)){
  if (a1[i,7]<=0){
    a1[i,7]=0
  }else{
    a1[i,7]=1
  }
}

#Approach 1: Treat all variables as factor variables
#b：from 4th day
#c：from 3th day
#d：from 2th day
#e：from 1th day
b=a1[4:nrow(a1),7]
c=a1[3:(nrow(a1)-1),7]
d=a1[2:(nrow(a1)-2),7]
e=a1[1:(nrow(a1)-3),7]
f=as.data.frame(cbind(b,c,d,e))
f[,1]=as.factor(f[,1])
f[,2]=as.factor(f[,2])
f[,3]=as.factor(f[,3])
f[,4]=as.factor(f[,4])
# #Test independence
# chisq.test(f$d,f$c)
# chisq.test(f$d,f$e)
# chisq.test(f$e,f$c)


#write.table(f,'/Users/pingyuanwang/Desktop',se='')
#use first 80%data as training data， remaining 20% as test data
numtrain=round(0.8*nrow(f))
train=f[1:numtrain,]
test=as.data.frame(f[(numtrain+1):nrow(f),])


#Naive bayes,posterior distribution
#MMM, predicted values are all 1s

NB=naiveBayes(b ~.,data=train)
#summary(NB)
T_NB=predict(NB,test[,-1],type='class')
#table(T_NB,test[,1])
summary(T_NB)
summary(T_NB==test[,1])
244/nrow(test)


#Oridinal Logistic Regression
#Dependent variables is categorial
#Using a logistic function
#Failed method
#Alternative, make >0.1 as 1, <0.1 as 0
# LR=glm(b~.,data=train,family=binomial(link='logit'))
# summary(LR)
# LR_P=predict(LR,test[,-1])
# LR_P=as.vector(LR_P)
# summary(LR_P)
# 
# lglgl=seq(1,nrow(test),1)
# for (i in 1:nrow(test)){
#   if (LR_P[i]<=0.1){
#     lglgl[i]=0
#   }else{
#     lglgl[i]=1
#   }
# }
# summary(lglgl>0.1)
# summary(lglgl==test[,1])
# 257/nrow(test)


#SVM
#Predicted values all 1s
Svm_1=svm(b~.,data=train)
summary(Svm_1)
SVM_Pred=predict(Svm_1,test[,-1],decision.values = F)
summary(SVM_Pred)
summary(SVM_Pred==test[,1])
#SVM_table= table(pred = SVM_Pred, true = test[,1])
#classAgreement(SVM_table)
265/nrow(test)

#Random Forest
#Predicted values all 1s
RF=randomForest(b~.,train)
#print(RF)
#importance(RF)
PDRF=predict(RF,test)
summary(PDRF)
summary(PDRF==test[,1])
265/nrow(test)





#Second Approach
#Make 2nd, 3rd, 4th as a continuous variable
a2=as.data.frame(AET)
a2$ZD=a2[,4]-a2[,1]
a2$Y=NA
for (i in 1:nrow(a2)){
  if (a2[i,7]<=0){
    a2[i,8]=0
  }else{
    a2[i,8]=1
  }
}
b1=a2[4:nrow(a2),8]
c1=a2[3:(nrow(a2)-1),7]
d1=a2[2:(nrow(a2)-2),7]
e1=a2[1:(nrow(a2)-3),7]
f1=as.data.frame(cbind(b1,c1,d1,e1))
f1[,1]=as.factor(f1[,1])
f1[,2]=as.numeric(f1[,2])
f1[,3]=as.numeric(f1[,3])
f1[,4]=as.numeric(f1[,4])
#write.table(f,'/Users/pingyuanwang/Desktop',se='')
numtrain=round(0.8*nrow(f1))
train=f1[1:numtrain,]
test=as.data.frame(f1[(numtrain+1):nrow(f1),])

#Naive Bayes
NB_new=naiveBayes(b1 ~.,data=train)
#summary(NB_new)
NBPD=predict(NB_new,test[,-1],type=c('class','raw'))
summary(NBPD)
#table(NBPD,test[,1])
summary(NBPD==test[,1])

251/nrow(test)
##SVM

#using the train set, do a grid search over the supplied parameter ranges(C,Gamma)
# tuned=tune.svm(b1~., data = train, gamma = 10^(-6:-1), cost = 10^(-1:1))
# summary(tuned)

Svm_2=svm(b1~.,data=train)
#summary(Svm_2)
SVM_Pred_2=predict(Svm_2,test[,-1],decision.values = T)
summary(SVM_Pred_2)
summary(SVM_Pred_2==test[,1])
251/nrow(test)
#SVM_table= table(pred = SVM_Pred_2, true = test[,1])
#classAgreement(SVM_table)

# plot3d(f1[,-1],col=c(1,2))
# 
# nnew=50
# newdat.list = lapply(train[,-1], function(x) seq(min(x), max(x), len=nnew))
# newdat      = expand.grid(newdat.list)
# newdat.pred = predict(Svm_2, newdata=newdat, decision.values=T)
# newdat.dv   = attr(newdat.pred, 'decision.values')
# newdat.dv   = array(newdat.dv, dim=rep(nnew, 3))
# contour3d(newdat.dv, level=0, x=newdat.list$c1, y=newdat.list$d1, z=newdat.list$e1, add=T)

#Random Forest
RF=randomForest(b1~.,train)
#print(RF)
#importance(RF)
PDRF=predict(RF,test)
summary(PDRF)
summary(PDRF==test[,1])
248/nrow(test)

#Third Approach
#Use first day's price to predict second day's adjusted close price
data=as.data.frame(AET)
data$ZD=data[,4]-data[,1]
for (i in 1:nrow(data)){
  if (data[i,7]<=0){
    data[i,7]=0
  }else{
    data[i,7]=1
  }
}
O=scale(data[1:(nrow(data)-1),1])
H=scale(data[1:(nrow(data)-1),2])
L=scale(data[1:(nrow(data)-1),3])
V=scale(data[1:(nrow(data)-1),5])
AC=scale(data[1:(nrow(data)-1),6])
Y=data[2:(nrow(data)),7]
data=as.data.frame(cbind(O,H,L,V,AC,Y))
data$Y=as.factor(data$Y)
colnames(data)=c('O','H','L','V','AC','Y')
numtrain=round(0.8*nrow(data))
train_Data=data[1:numtrain,]
test_Data=as.data.frame(data[(numtrain+1):nrow(data),])



#Naive Bayes
NB=naiveBayes(Y ~O+H+AC,data=train_Data)
#summary(NB)
NBPD=predict(NB,test_Data[,(-6)])
#summary(NBPD)
summary(NBPD)
#table(NBPD,test_Data[,6])
summary(NBPD==test_Data[,6])
240/nrow(test)

##SVM

#using the train_Data set, do a grid search over the supplied parameter ranges(C,Gamma)
#MMM, all predictions are 1s
# tuned=tune.svm(Y~O+H+AC, data = train_Data, gamma = 10^(-6:-1), cost = 10^(-1:1))
# summary(tuned)

Svm_new=svm(Y~O+H+AC,data=train_Data)
summary(Svm_new)
SVM_Pred=predict(Svm_new,test_Data[,-6],decision.values =TRUE)
summary(SVM_Pred)
summary(SVM_Pred==test_Data[,6])
240/nrow(test_Data)

#SVM_table= table(pred = SVM_Pred, true = test_Data[,6])
#classAgreement(SVM_table)

#plot3d(f1[,-1],col=c(1,2))

# nnew=20
# newdat.list = lapply(train_Data[,-6], function(x) seq(min(x), max(x), len=nnew))
# newdat      = expand.grid(newdat.list)
# newdat.pred = predict(Svm_2, newdata=c(newdat$O,newdat$H,newdat$AC), decision.values=T)
# newdat.dv   = attr(newdat.pred, 'decision.values')
# newdat.dv   = array(newdat.dv, dim=rep(nnew, 3))
# contour3d(newdat.dv, level=0, x=newdat.list$c1, y=newdat.list$d1, z=newdat.list$e1, add=T)


#RF
RF=randomForest(Y~O+H+AC,data=train_Data)
#print(RF)
#importance(RF)
PDRF=predict(RF,test_Data)
summary(PDRF)
summary(PDRF==test[,1])
242/nrow(test_Data)


















