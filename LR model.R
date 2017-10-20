install.packages("caTools")
install.packages("ROCR")
library(caTools)
library(ROCR)

############### read dataset
setwd("D:/徐文艺/2017 Travelers/")
train = read.csv('Train.csv')
train$year =as.factor(train$year)

################## zip code (assign based on first three digits)
train$area = NA
hist(train$zip.code)
table(train$zip.code %/% 100)
train$area[train$zip.code %/% 100 ==150]="PA"
train$area[train$zip.code %/% 100 ==201]="DC"
train$area[train$zip.code %/% 100 ==500]="IA"
train$area[train$zip.code %/% 100 ==800]="CO"
train$area[train$zip.code %/% 100 ==801]="CO"
train$area[train$zip.code %/% 100 ==850]="AZ"
train$area[train$zip.code %/% 100 ==980]="WA"

train$area =as.factor(train$area)
train$zip.code=as.factor(train$zip.code)
train =train[,c(1:17,19,18)]
summary(train)
str(train)

################## Outliers
par(mfrow=c(2,2))
for (i in (2:18)) {
  if(class(train[,i])!='factor') {
  boxplot(train[,i],main = colnames(train)[i])}
  else {next}
}

#n.adults
hist(train$n.adults)
#nrow(subset(train,train$n.adults>6))
train1 = subset(train,train$n.adults<=6)

#n.children
hist(train$n.children)
#nrow(subset(train,train$n.children<=7))
train2 = subset(train1,train1$n.adults<=7)

#len.at.res
hist(train$len.at.res)
boxplot(train$len.at.res)
#nrow(subset(train,train$len.at.res>27))
train3 = subset(train2,train2$len.at.res<=27)

#ni.age
hist(train$ni.age)
#nrow(subset(train,train$ni.age>80))
train4 = subset(train3,train3$ni.age<=90)

###################### deal with Numeric NAs
for (i in (2:18)) {
  if(class(train4[,i])!='factor') {
    if (sum(is.na(train4[,i]))!=0) {
      train4[which(is.na(train4[,i])),i]=mean(train4[,i],na.rm=T)
    }
  }
  else {next}
}

summary(train4[,-c(6,9,10,11,13,14,16,17,18)])
str(train)

###################### deal with categorical NAs
summary(train[,c(6,9,10,11,13,14,16,17,18)])
 #i=6
for (i in (2:18)) {
  if(class(train4[,i])=='factor') {
    if (sum(is.na(train4[,i]))!=0) {
      train4=subset(train4,!is.na(train4[,i]))
    }
  } 
  else {next}
} 

summary(train4)

######################  dummy variable
colnames(train4)[c(6,9,10,11,13,14,18)]
fmla = as.formula(paste("~",paste(colnames(train4)[c(6,9,10,11,13,14,18)],
                                  collapse = "+"),sep = ""))
dummies = model.matrix(fmla,train4)[,-1]
dummies = as.data.frame(dummies)
train5 = cbind(dummies,train4[,-c(6,9,10,11,13,14,16,17,18)])
summary(train5)

#######################  remove cancel -1 value
table(train$cancel)
train6 = train5[train5$cancel!=-1,]
summary(train6)


#########################  standardization

range01 = function(x){
  (x-min(x))/(max(x)-min(x))
}
train6$tenure = range01(train6$tenure)
train6$n.adults=range01(train6$n.adults)
train6$n.children=range01(train6$n.children)
train6$premium=range01(train6$premium)
train6$len.at.res=range01(train6$len.at.res)
train6$ni.age=range01(train6$ni.age)

######################### split train,validation,test
table(train6$cancel)
model =train6

set.seed(123)
index = sample(nrow(model),nrow(model)*0.1)
test = model[index,]                            ########### test set
tv = model[-index,]

############### balanced cross-validation training and validation
formula = as.formula(paste("cancel~",paste(colnames(model)[-c(1,18,24,27)],
                                           collapse = "+"),sep = ""))

vector_validation = c()
vector_testing = c()
vector_average = c()
vector_aic = c()
table = matrix(NA,nrow=24,ncol=500)

for (i in (1:500)) {                            ############ sampling n times
  cancel1=tv[tv$cancel==1,]
  cancel0=tv[tv$cancel==0,]
  split_train1=sample.split(cancel1$cancel,SplitRatio = .95)
  training1_set =subset(cancel1,split_train1==TRUE)
  validation1_set = subset(cancel1,split_train1==FALSE)
  split_train0=sample(nrow(cancel0),nrow(training1_set))
  training0_set =cancel0[split_train0,]
  validation0_set =cancel0[-split_train0,]
  training=rbind(training0_set,training1_set)
  validation = rbind(validation0_set,validation1_set)

  fit = glm(formula,training,family = binomial())      ################ model part
  
  pred_value = predict(fit,validation,type = "response")
  pred_input = prediction(pred_value,validation$cancel)
  AUC_validation =performance(pred_input,"auc")
  test_value = predict(fit,test,type = "response")
  test_input = prediction(test_value,test$cancel)
  AUC_testing =performance(test_input,"auc")
  
  vector_aic[i] = fit$aic
  table[,i]=as.vector(fit$coefficients)
  vector_validation[i]=unlist(AUC_validation@y.values)
  vector_testing[i]=unlist(AUC_testing@y.values)
  vector_average[i]=mean(c(vector_validation[i],vector_testing[i]))
  print(i)
}
matrix = as.data.frame(cbind(vector_validation,vector_testing,vector_average,vector_aic))    ########## AUC matrix
rownames(table) = c("intercept",colnames(model)[-c(1,18,24,27)])
write.csv(matrix,"LR AUC Matrix.csv",row.names = F)

best_coef =as.data.frame(table[,which.max(x = matrix$vector_average)])                 ############# best model's coefficient

coef = as.data.frame(apply(table,MARGIN = 1,FUN = mean))                  ############## mean-coef
write.csv(coef,"LR mean_coef.csv",row.names = F)

################### predict response part
ptest = as.data.frame(t(test))

check = cbind(coef[match(rownames(ptest),rownames(coef)),],ptest)   ######## test mean-coef:AUC 0.745
intercept = coef[1,1]
check = rbind(intercept,check)
rownames(check)[1]="intercept"
colnames(check)[1]="coef"
check$coef[is.na(check$coef)]=0
check[1,2:ncol(check)]=0

prob = apply(check[,2:ncol(check)],MARGIN = 2,function(x) 1/(1+exp(-(sum(x*check[,1])+check[1,1]))))

pred_input = prediction(prob,test$cancel)
AUC =performance(pred_input,"auc")
print(AUC@y.values)

#####
check2 = cbind(best_coef[match(rownames(ptest),rownames(best_coef)),],ptest)   ######## test best-coef:AUC 0.748

intercept = best_coef[1,1]
check2 = rbind(intercept,check2)
rownames(check2)[1]="intercept"
colnames(check2)[1]="coef"
check2$coef[is.na(check2$coef)]=0
check2[1,2:ncol(check2)]=0

prob = apply(check2[,2:ncol(check2)],MARGIN = 2,function(x) 1/(1+exp(-(sum(x*check2[,1])+check2[1,1]))))

pred_input = prediction(prob,test$cancel)
AUC =performance(pred_input,"auc")
print(AUC@y.values)


# plot coef distribution
par(mfrow=c(2,2))
for (i in (1:nrow(table))) {
  hist(table[i,])
}
