
install.packages("caTools")
install.packages("ROCR")
install.packages("corrgram")
install.packages("rpart")
install.packages("randomForest")
install.packages("caret")
install.packages("e1071")

library(caTools)
library(ROCR)
library(corrgram)
library(rpart)
library(randomForest)
library(caret)
library(e1071)

setwd('/home/senlyu/git/TravelersCompetition/data')

############### read dataset
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

########### correlation plot


corrgram(train6[,-c(18,27)], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Variable Correlation")

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


############### Logistic Regression with balanced cross-validation training and validation
formula = as.formula(paste("cancel~",paste(colnames(model)[-c(1,18,24,27)],
                                           collapse = "+"),sep = ""))

vector_train = c()
vector_validation = c()
vector_diff = c()
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

  fit = glm(formula,training,family = "binomial")      ################ GLM model part
  
  pred_train = predict(fit,training,type = "response")
  pred_input = prediction(pred_train,training$cancel)
  AUC_train = performance(pred_input,"auc")
  pred_validation = predict(fit,validation,type = "response")
  pred_validation_input = prediction(pred_validation,validation$cancel)
  AUC_validation = performance(pred_validation_input,"auc")
  
  vector_aic[i] = fit$aic
  table[,i]=as.vector(fit$coefficients)
  vector_train[i]=unlist(AUC_train@y.values)
  vector_validation[i]=unlist(AUC_validation@y.values)
  vector_diff[i]=vector_train[i]-vector_validation[i]
}
matrix = as.data.frame(cbind(vector_train,vector_validation,vector_diff,vector_aic))    ########## AUC matrix
rownames(table) = c("intercept",colnames(model)[-c(1,18,24,27)])

write.csv(matrix,"LR AUC Matrix.csv",row.names = F)
best_coef =as.data.frame(table[,which.min(abs(x = matrix$vector_diff))])                 ############# best model's coefficient
write.csv(coef,"LR mean_coef.csv",row.names = F)
coef = as.data.frame(apply(table,MARGIN = 1,FUN = mean))                  ############## mean-coef


################### Logistic Regression predict response part
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

colAUC(prob,test$cancel,plotROC = TRUE)                                        ########  plot ROC curve

# plot coef distribution
par(mfrow=c(2,2))
for (i in (1:nrow(table))) {
  hist(table[i,])
}





#################### CART model



model$cancel=as.factor(model$cancel)
test = model[index,]                                                          ########### test set
tv = model[-index,]

fit = rpart(formula,data = tv,method = "class")
summary(fit)

pred = predict(fit,test,type = "prob")
p =prediction(pred[,2],test$cancel)
auc = performance(p,"auc")
auc@y.values                                                                ########################  AUC:0.5  有问题







##############  Random Forest

model =train6
model$cancel=as.factor(model$cancel)
test = model[index,]                           
tv = model[-index,]

fit <- randomForest(formula,data=tv,ntree = 500)

print(fit) # view results 
import = as.data.frame(importance(fit))                                         # importance of each predictor
write.csv(import,"Random Forest Importance.csv",row.names = T)
pred = predict(fit,test,type = "prob")
p =prediction(pred[,2],test$cancel)
auc = performance(p,"auc")
auc@y.values                                                                    ########################  AUC:0.71(ntree = 500)



##################### glmnet model

model$cancel=as.factor(model$cancel)
test = model[index,]                           
tv = model[-index,]

objControl = trainControl(method = "cv", number = 50,returnResamp='none')

# run model
objModel = train(x = tv[,-c(1,18,24,27)], tv$cancel, method='glmnet',trControl=objControl)

#### predict
pred = predict(objModel,test,type = "prob")
p =prediction(pred[,2],test$cancel)
auc = performance(p,"auc")
auc@y.values                                                                   ########################  AUC:0.7448
summary(objModel)

plot(varImp(objModel,scale=F))


library(e1071)
test = model[index,]                           
tv = model[-index,]
formula = as.formula(paste("cancel~",paste(colnames(model)[-c(1,18,24,27)],
                                           collapse = "+"),sep = ""))
model.svm <- svm(formula, tv, probability=TRUE)
pred = predict(model.svm, test, probability=TRUE)
p = prediction(attr(pred,"probabilities")[,2],test$cancel)
auc = performance(p,"auc")
auc@y.values         






