library(dplyr)
library(ISLR)
library(e1071)
names(oot)
train_input = read.csv("train.csv")
train =  train_input[,c('Fraud','cardnumZip30', 'cardnumZipNA30',
                        'cardnumZipNA11', 'cardnumMerchantnum30',
                        'cardnum30', 'cardnumMax30',
                        'same_Amount_1', 'same_Amount_Amount_diff_Cardnum_1',
                        'same_Amount_Amount_diff_Cardnum_5', 'same_Amount_Amount_5')]

test = read.csv("test.csv")
oot = read.csv("oot.csv")

#svmfit=svm(Fraud~., data=train, kernel="linear", cost=10, scale=FALSE)

#pred_train = predict(svmfit, train) 
#train_new = cbind(pred_train, train)

#pred_test = predict(svmfit, test)
#test_new = cbind(pred_test, test)

#pred_oot = predict(svmfit, oot)
#oot_new = cbind(pred_oot, oot)

library(randomForest)
train$cardnumMax30 = as.numeric(gsub("\\$", "", train$cardnumMax30))
rf.fit = randomForest(Fraud ~ .,data = train, ntree=150, norm.votes=FALSE, na.action=na.exclude)
rf.fit.prob = predict(rf.fit, type = "response")
#rf.fit.predict = ifelse(rf.fit.prob > 0.1, 1, 0)
#table(rf.fit.predict,train$Fraud)
#mean(rf.fit.predict==train$Fraud)
rf.train.pred.data = cbind(train, rf.fit.prob)
rf.train.pred.data[,c(1,12)] %>%
  arrange(-rf.fit.prob) %>% 
  slice(1:round(.02*nrow(rf.train.pred.data))) %>% 
  count(Fraud)
sum(rf.train.pred.data$Fraud)
328/452

test =  test[,c('Fraud','cardnumZip30', 'cardnumZipNA30',
                        'cardnumZipNA11', 'cardnumMerchantnum30',
                        'cardnum30', 'cardnumMax30',
                        'same_Amount_1', 'same_Amount_Amount_diff_Cardnum_1',
                        'same_Amount_Amount_diff_Cardnum_5', 'same_Amount_Amount_5')]
test$cardnumMax30 = as.numeric(gsub("\\$", "", test$cardnumMax30))
rf.fit.prob.test = predict(rf.fit, test, type = "response")
#rf.fit.predict.test = ifelse(rf.fit.prob.test > 0.5, 1, 0)
#table(rf.fit.predict.test,test$Fraud)
#mean(rf.fit.predict.test==test$Fraud)
rf.test.pred.data = cbind(test, rf.fit.prob.test)
rf.test.pred.data[,c(1,12)] %>%
  arrange(-rf.fit.prob.test) %>% 
  slice(1:round(.02*nrow(rf.test.pred.data))) %>% 
  count(Fraud)
sum(rf.test.pred.data$Fraud)
222/506

oot =  oot[,c('Fraud','cardnumZip30', 'cardnumZipNA30',
                'cardnumZipNA11', 'cardnumMerchantnum30',
                'cardnum30', 'cardnumMax30',
                'same_Amount_1', 'same_Amount_Amount_diff_Cardnum_1',
                'same_Amount_Amount_diff_Cardnum_5', 'same_Amount_Amount_5')]
oot$cardnumMax30 = as.numeric(gsub("\\$", "", oot$cardnumMax30))
rf.fit.prob.oot = predict(rf.fit, oot, type = "response")
rf.oot.pred.data = cbind(oot, rf.fit.prob.oot)

rf.oot.pred.data[,c(1,12)] %>%
  arrange(-rf.fit.prob.oot) %>% 
  slice(1:round(.02*nrow(rf.oot.pred.data))) %>% 
  count(Fraud)

sum(rf.oot.pred.data$Fraud)
91/337


library(MASS)

qda.fit = qda(Fraud ~ .,data=train)
qda.fit.prob = predict(qda.fit, type = "response")
qda.train.pred.data = cbind(train, qda.fit.prob)

qda.train.pred.data[,c(1,13)] %>%
  arrange(-posterior.1) %>% 
  slice(1:round(.02*nrow(qda.train.pred.data))) %>% 
  count(Fraud)
sum(qda.train.pred.data$Fraud)
192/454

qda.fit.prob.test = predict(qda.fit, test, type = "response")
qda.test.pred.data = cbind(test, qda.fit.prob.test)
qda.test.pred.data[,c(12,140)] %>% 
  arrange(-posterior.1) %>% 
  slice(1:round(.02*nrow(qda.test.pred.data))) %>% 
  count(Fraud)
sum(qda.test.pred.data$Fraud)
223/560

qda.fit.prob.oot = predict(qda.fit, oot, type = "response")
qda.oot.pred.data = cbind(oot, qda.fit.prob.oot)
qda.oot.pred.data[,c(12,140)] %>%
  arrange(-posterior.1) %>% 
  slice(1:round(.02*nrow(qda.oot.pred.data))) %>% 
  count(Fraud)

sum(qda.oot.pred.data$Fraud)
85/338

 
#LASSO gives the same variables as best subset
library(glmnet)
'%ni%'= Negate('%in%')
train_lasso = na.omit(train_input)
fctr = lapply(train_lasso[sapply(train_lasso, is.factor)], droplevels)
sapply(fctr, nlevels)
x = model.matrix(Fraud~.,data=train_lasso)
x=x[,-1]
table(train$Fraud)
478/57334

glmnet1=cv.glmnet(x=x,y=train_lasso$Fraud,type.measure='mse',nfolds=5,alpha=.5)

c=coef(glmnet1,s='lambda.min',exact=TRUE)
inds=which(c!=0)
variables=row.names(c)[inds]
variables=variables[variables %ni% '(Intercept)']
variables
#Gives exactly the same variables as best subset

train_l = train_input[,c("Fraud", "cardnumDiffMax1", "cardnumDiffMax30", "cardnumDiffMax10", "cardnumDiffMax3", "cardnumDiffMax7" )]
rf.fit = randomForest(Fraud ~ .,data = train_l, ntree=150, norm.votes=FALSE)
rf.fit.prob = predict(rf.fit, type = "response")
rf.train.pred.data = cbind(train_l, rf.fit.prob)
rf.train.pred.data[,c(1,7)] %>%
  arrange(-rf.fit.prob) %>% 
  slice(1:round(.02*nrow(rf.train.pred.data))) %>% 
  count(Fraud)
sum(rf.train.pred.data$Fraud)
208/294

rf.fit.prob.test = predict(rf.fit, test, type = "response")

rf.test.pred.data = cbind(test, rf.fit.prob.test)
rf.test.pred.data[,c(12,262)] %>%
  arrange(-rf.fit.prob.test) %>% 
  slice(1:round(.02*nrow(rf.test.pred.data))) %>% 
  count(Fraud)
sum(rf.test.pred.data$Fraud)
166/378

rf.fit.prob.oot = predict(rf.fit, oot, type = "response")
rf.oot.pred.data = cbind(oot, rf.fit.prob.oot)
rf.oot.pred.data[,c(12,262)] %>%
  arrange(-rf.fit.prob.oot) %>% 
  slice(1:round(.02*nrow(rf.oot.pred.data))) %>% 
  count(Fraud)

sum(rf.oot.pred.data$Fraud)
47/248

#TAKE 2
train_input = read.csv("train new.csv")
train = train_input[,c('same_Merchantnum_5.x','same_Merchantnum_30.x',
                       'same_Merchantnum_diff_Zip_1.x', 'same_Merchantnum_diff_Zip_3.x',
                       'same_Merchantnum_diff_Zip_30.x', 'same_Merchantnum_diff_amount_1.x',
                       'same_Merchantnum_diff_amount_3.x', 'same_Merchantnum_diff_amount_30.x',
                       'same_Merchantnum_diff_Cardnum_1.x', 'same_Merchantnum_diff_Cardnum_3.x',
                       'same_Merchantnum_diff_Cardnum_14.x', 'same_Merchantnum_1.y',
                       'same_Merchantnum_3.y', 'same_Merchantnum_5.y',
                       'same_Merchantnum_14.y', 'same_Merchantnum_diff_Zip_1.y',
                       'same_Merchantnum_diff_Zip_30.y', 'same_Merchantnum_diff_amount_3.y',
                       'same_Merchantnum_diff_amount_7.y', 'same_Cardnum_diff_Cardnum_5.y',
                       'same_Merchantnum_diff_Cardnum_14.y', 'Fraud')]
test = read.csv('test new.csv')
oot = read.csv('oot new.csv')
rf.fit = randomForest(Fraud ~ .,data = train, ntree=150, norm.votes=FALSE)
rf.fit.prob = predict(rf.fit, type = "response")
rf.train.pred.data = cbind(train, rf.fit.prob)
rf.train.pred.data[,c(22,23)] %>%
  arrange(-rf.fit.prob) %>% 
  slice(1:round(.02*nrow(rf.train.pred.data))) %>% 
  count(Fraud)
sum(rf.train.pred.data$Fraud)
308/478



rf.fit.prob.test = predict(rf.fit, test, type = "response")
rf.test.pred.data = cbind(test, rf.fit.prob.test)
rf.test.pred.data[,c(12,65)] %>%
  arrange(-rf.fit.prob.test) %>% 
  slice(1:round(.02*nrow(rf.test.pred.data))) %>% 
  count(Fraud)
sum(rf.test.pred.data$Fraud)
168/536


rf.fit.prob.oot = predict(rf.fit, oot, type = "response")
rf.oot.pred.data = cbind(oot, rf.fit.prob.oot)

rf.oot.pred.data[,c(12,65)] %>%
  arrange(-rf.fit.prob.oot) %>% 
  slice(1:round(.02*nrow(rf.oot.pred.data))) %>% 
  count(Fraud)

sum(rf.oot.pred.data$Fraud)
42/338
nrow(train)+nrow(test)+nrow(oot)

#Models
library(readxl)
library(dplyr)
data = read_excel('All Models.xlsx')
data %>% filter(label==2) %>% arrange(-`Bootstrap Forest (n = 150)`) %>% slice(1:round(.02*nrow(data[data$label==2,]))) %>% count(Fraud)
data %>% filter(label==2) %>% count(Fraud)
177/338

data %>% filter(label==1) %>% arrange(-`Bootstrap Forest (n = 150)`) %>% slice(1:round(.02*nrow(data[data$label==1,]))) %>% count(Fraud)
data %>% filter(label==1) %>% count(Fraud)
155/263

data %>% filter(label==0) %>% arrange(-`Bootstrap Forest (n = 150)`) %>% slice(1:round(.02*nrow(data[data$label==0,]))) %>% count(Fraud)
data %>% filter(label==0) %>% count(Fraud)
262/1239

data %>% filter(label==2) %>% arrange(-`Boosted Tree`) %>% slice(1:round(.02*nrow(data[,"label"==2]))) %>% count(Fraud)
data %>% filter(label==2) %>% count(Fraud)
238/338

data %>% filter(label==1) %>% arrange(-`Boosted Tree`) %>% slice(1:round(.02*nrow(data[,"label"==1]))) %>% count(Fraud)
data %>% filter(label==1) %>% count(Fraud)
162/263

data %>% filter(label==0) %>% arrange(-`Boosted Tree`) %>% slice(1:round(.02*nrow(data[,"label"==0]))) %>% count(Fraud)
data %>% filter(label==0) %>% count(Fraud)
734/1239

data %>% filter(label==2) %>% arrange(-NN) %>% slice(1:round(.02*nrow(data[,"label"==2]))) %>% count(Fraud)
data %>% filter(label==2) %>% count(Fraud)
197/338

data %>% filter(label==1) %>% arrange(-NN) %>% slice(1:round(.02*nrow(data[,"label"==1]))) %>% count(Fraud)
data %>% filter(label==1) %>% count(Fraud)
174/263

data %>% filter(label==0) %>% arrange(-NN) %>% slice(1:round(.02*nrow(data[,"label"==0]))) %>% count(Fraud)
data %>% filter(label==0) %>% count(Fraud)
965/1239

data %>% filter(label==2) %>% arrange(-`Naive Prob 1`) %>% slice(1:round(.02*nrow(data[,"label"==2]))) %>% count(Fraud)
data %>% filter(label==2) %>% count(Fraud)
223/338

data %>% filter(label==1) %>% arrange(-`Naive Prob 1`) %>% slice(1:round(.02*nrow(data[,"label"==1]))) %>% count(Fraud)
data %>% filter(label==1) %>% count(Fraud)
122/263

data %>% filter(label==0) %>% arrange(-`Naive Prob 1`) %>% slice(1:round(.02*nrow(data[,"label"==0]))) %>% count(Fraud)
data %>% filter(label==0) %>% count(Fraud)
518/1239


data %>% filter(label==2) %>% arrange(-`Logistic 1`) %>% slice(1:round(.02*nrow(data[,"label"==2]))) %>% count(Fraud)
data %>% filter(label==2) %>% count(Fraud)
210/338

data %>% filter(label==1) %>% arrange(-`Logistic 1`) %>% slice(1:round(.02*nrow(data[,"label"==1]))) %>% count(Fraud)
data %>% filter(label==1) %>% count(Fraud)
149/263

data %>% filter(label==0) %>% arrange(-`Logistic 1`) %>% slice(1:round(.02*nrow(data[,"label"==0]))) %>% count(Fraud)
data %>% filter(label==0) %>% count(Fraud)
653/1239
