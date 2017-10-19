setwd("D:/徐文艺/2017 Travelers/")
train = read.csv('Train.csv')
train$year =as.factor(train$year)

##################zip code (assign based on first three digits)
train$area = NA
hist(train$zip.code)
table(train$zip.code %/% 100)
train$area[train$zip.code %/% 100 ==150]="PA"
train$area[train$zip.code %/% 100 ==201]="DC"
train$area[train$zip.code %/% 100 ==500]="IA"
train$area[train$zip.code %/% 100 ==800]="CO"
train$area[train$zip.code %/% 100 ==801]="CO"
train$area[train$zip.code %/% 100 ==850]="AZ"
train$area[train$zip.code %/% 100 ==980]="wA"

train$area =as.factor(train$area)
train$zip.code=as.factor(train$zip.code)
train =train[,c(1:17,19,18)]
summary(train)
str(train)

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


#i=2
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

#deal with categorical variables containing NA
summary(train4[,c(6,9,10,11,13,14,16,17,18)])
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

#dummy variable
colnames(train4)[c(6,9,10,11,13,14,18)]
fmla = as.formula(paste("~",paste(colnames(train4)[c(6,9,10,11,13,14,18)],collapse = "+"),sep = ""))
dummies = model.matrix(fmla,train4)[,-1]
dummies = as.data.frame(dummies)
train5 = cbind(dummies,train4[,-c(6,9,10,11,13,14,16,17,18)])
summary(train5)

#remove cancel -1 value
table(train$cancel)
train6 = train5[train5$cancel!=-1,]
summary(train6)


#############standardization
colnames(train6)
range01 = function(x){
  (x-min(x))/(max(x)-min(x))
}
train6$tenure = range01(train6$tenure)
train6$n.adults=range01(train6$n.adults)
train6$n.children=range01(train6$n.children)
train6$premium=range01(train6$premium)
train6$len.at.res=range01(train6$len.at.res)
train6$ni.age=range01(train6$ni.age)

############### balanced cross-validation (train, validation and test)
model = train6
install.packages("caTools")
library(caTools)

formula = as.formula(paste("cancel~",paste(colnames(model)[-c(18,27)],collapse = "+"),sep = ""))

vector_validation = c()
vector_testing = c()
vector_average = c()
table = matrix(NA,nrow=26,ncol=200)

for (i in (1:200)) {                            ############ sampling 200 times
  data_cancel1=model[model$cancel==1,]
  data_cancel0=model[model$cancel==0,]
  split_train1=sample.split(data_cancel1$cancel,SplitRatio = .8)
  training1_set =subset(data_cancel1,split_train1==TRUE)
  validation1_set = subset(data_cancel1,split_train1==FALSE)
  split_train0=sample.split(data_cancel0$cancel,SplitRatio = nrow(training1_set)/(nrow(data_cancel0)))
  training0_set =subset(data_cancel0,split_train0==TRUE)
  validation0_set =subset(data_cancel0,split_train0==FALSE)
  training_set=rbind(training0_set,training1_set)
  validation = rbind(validation0_set,validation1_set)
  split_validation = sample.split(validation,SplitRatio = 0.5)
  validation_set = subset(validation,split_validation==TRUE)
  testing_set =subset(validation,split_validation==FALSE)
  
  fit = glm(formula,training_set,family = binomial())      ################ model part
  
  predicted_value = predict(fit,validation_set,type = "response")         ############ get AUC
  pred_input = prediction(predicted_value,validation_set$cancel)
  AUC_validation =performance(pred_input,"auc")
  testing_value = predict(fit,testing_set,type = "response")
  testing_input = prediction(testing_value,testing_set$cancel)
  AUC_testing =performance(testing_input,"auc")
  
  table[,i]=as.vector(fit$coefficients)                                   ########## restore AUC and coefficient
  vector_validation[i]=unlist(AUC_validation@y.values)
  vector_testing[i]=unlist(AUC_testing@y.values)
  vector_average[i]=mean(c(vector_validation[i],vector_testing[i]))
  
}
matrix = as.data.frame(cbind(vector_validation,vector_testing,vector_average))
table[,which.max(x = matrix$vector_average)]                  ############# best model's coefficient
