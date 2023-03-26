# rm(list=ls())
zsnew = read.csv(file='zsnew.csv')
head(zsnew)
summary(zsnew$default)

set.seed(200)
idx = sample(1:dim(zsnew)[1],size=floor(dim(zsnew)[1]*0.8),replace=F)
train = zsnew[+idx,]
test  = zsnew[-idx,]
a=(which(train$default==0))
b=(which(train$default==1))
# Create alternative datasets - that are more balanced
idx.good = which(train$default==0); length(idx.good)
idx.bad  = which(train$default==1); length(idx.bad)

# Undersampling majority
set.seed(300)
tmp = idx.good[sample(1:length(idx.good),size=length(idx.bad),replace=F)]
train.um = rbind(train[tmp,],train[idx.bad,])#combining two data sets
dim(train.um)
summary(train.um$default)
# Just some randomization - we do not want defaults and non-deafults to be stacked one next to each other (just to be sure)
train.um = train.um[sample(1:dim(train.um)[1],size=dim(train.um)[1],replace=F),]

# Undersampling majority and oversampling minority (2 times the number of minority)
set.seed(300)
tmp.good = idx.good[sample(1:length(idx.good),size=length(idx.bad)*2,replace=F)]
# Here we need replacement = T - why?
tmp.bad  = idx.bad[sample(1:length(idx.bad),size=length(idx.bad)*2,replace=T)] # Oversampling
train.uo = rbind(train[tmp.good,],train[tmp.bad,])
# Just some randomization - we do not want defaults and non-deafults to be stacked one next to each other (just to be sure)
train.uo = train.uo[sample(1:dim(train.uo)[1],size=dim(train.uo)[1],replace=F),]
dim(train.uo)
summary(train.uo$default)

# Cost weighted
wgt   = rep(0,dim(train)[1])
wgt[train$default==1] = 0.5/length(idx.bad)
wgt[train$default==0] = 0.5/length(idx.good)
train$wgt = wgt

# Let's estimate couple of models
spec_one = as.formula(default~amount+term+I(amount/term)+rate+time.start+time.start2)

# PLM
p1 = predict(lm(spec_one,data=train),new=test)
p2 = predict(lm(spec_one,data=train.um),new=test)
p3 = predict(lm(spec_one,data=train.uo),new=test)
p4 = predict(lm(spec_one,data=train,weights=wgt),new=test)
hist(p1, breaks = 50)
hist(p2, breaks = 50)
hist(p3, breaks = 50)
hist(p4, breaks = 50)
summary(p4)

# We make some adjustments - why?
p1[p1>=1] = 0.999
p1[p1<=0] = 0.001
p2[p2>=1] = 0.999
p2[p2<=0] = 0.001
p3[p3>=1] = 0.999
p3[p3<=0] = 0.001
p4[p4>=1] = 0.999
p4[p4<=0] = 0.001

# LR
p5 = predict(glm(spec_one,data=train,family='binomial'),new=test)
p5 = 1/(1+exp(-p5))
summary(p5)
hist(p5, breaks = 50)
p6 = predict(glm(spec_one,data=train.um,family='binomial'),new=test)
p6 = 1/(1+exp(-p6))
summary(p6)
hist(p6, breaks = 50)
p7 = predict(glm(spec_one,data=train.uo,family='binomial'),new=test)
p7 = 1/(1+exp(-p7))
hist(p7, breaks = 50)
summary(p7)
p8 = predict(glm(spec_one,data=train,weights=wgt,family='binomial'),new=test)
p8 = 1/(1+exp(-p8))
hist(p8, breaks = 50)
summary(p8)

# We will make some adjustments even here - why? Completely different reason - cross-entropy
p5[p5>0.999] = 0.999
p5[p5<0.001] = 0.001
p6[p6>0.999] = 0.999
p6[p6<0.001] = 0.001
p7[p7>0.999] = 0.999
p7[p7<0.001] = 0.001
p8[p8>0.999] = 0.999
p8[p8<0.001] = 0.001

# Predictions
predicts = cbind(test$default,p1,p2,p3,p4,p5,p6,p7,p8)
head(predicts)

# Data-frame with results: Accuracy, Specificity, Sensitivity, Precision, AUC, BS, CE
#install.packages("caret")
#install.packages("pROC")
#install.packages("Rcpp")
#install.packages("MCS")
library(caret)
library(MCS)
library(pROC)
library(Rcpp)
#update.packages(ask = FALSE)

results = data.frame(metrics = c('Accuracy','Specificity','Sensitivity','Precision','AUC','Brier score','Cross entropy'),
                     p1=NA,p2=NA,p3=NA,p4=NA,p5=NA,p6=NA,p7=NA,p8=NA)
reference = as.factor(predicts[,1])
for (i in 2:9) {
  prediction = as.factor((predicts[,i]>0.5)*1)
  tmp = confusionMatrix(data=prediction,reference=reference,positive='1')
  
  # Brier score
  brier.loss = (predicts[,i] - test$default)^2
  
  # Cross entropy
  # PLM
  cross.loss = -1*(log(predicts[,i])*test$default + log(1-predicts[,i])*(1-test$default))
  
  results[,i] = c(tmp$overall[1], tmp$byClass[c(2,1,5)],
                  as.numeric(pROC::roc(response=predicts[,1],predictor=predicts[,i])$auc),
                  mean(brier.loss),mean(cross.loss))
}
results
confusionMatrix(data=prediction1,reference=reference,positive='1')
# Let's try to improve the zsnew models

# Try the same with the TITANIC
titanic = read.csv(file='titanic.csv')
head(titanic)

titanic$name#############dnt use
table(titanic$Top)
table(titanic$dayAge)
titanic$dayAge = floor(titanic$age*365)
titanic$TopF
titanic$topFf = (titanic$TopF=="TopF")*1
titanic$nTopFf = (titanic$TopF=="noTopF")*1

titanic$midFf = (titanic$MidF=="Mid")*1
titanic$nMidFf = (titanic$MidF=="noMid")*1

titanic$female = (titanic$FemaleF=="Female")*1
titanic$male = (titanic$FemaleF=="Man")*1

titanic$parent = (titanic$ParentF=="Parent")*1
titanic$noParent = (titanic$ParentF=="noParent")*1

titanic$child = (titanic$ChildF=="child")*1
titanic$noChild = (titanic$ChildF=="noChild")*1


set.seed(100)
idx = sample(1:dim(titanic)[1],size=floor(dim(titanic)[1]*0.8),replace=F)
train = titanic[+idx,]
test  = titanic[-idx,]
a=(which(train$survived==0))
b=(which(train$survived==1))
# Create alternative datasets - that are more balanced
idx.good = which(train$survived==0); length(idx.good)
idx.bad  = which(train$survived==1); length(idx.bad)


# Cost weighted
wgt   = rep(0,dim(train)[1])
wgt[train$survived==1] = 0.5/length(idx.bad)
wgt[train$survived==0] = 0.5/length(idx.good)
train$wgt = wgt
head(train)
#There are some limitations if many features are included:
#Estimation uncertainty - too many parameters.
#Over-fitting is likely.
# Let's estimate couple of models
spec_one = as.formula(survived~Top+Mid+female+parent+child+topFf+midFf+nTopFf+nMidFf+male+female+noParent+noChild)
p = predict(glm(spec_one,data=train,weights=wgt,family='binomial'),new=test)
p = 1/(1+exp(-p))
hist(p, breaks = 50)
summary(p)
length(p)


results = data.frame(metrics = c('Accuracy','Specificity','Sensitivity','Precision','AUC','Brier score','Cross entropy'),
                     p1=NA)
reference = as.factor(test$survived)

prediction = as.factor((p>0.5)*1)
tmp = confusionMatrix(data=prediction,reference=reference,positive='1')
  
# Brier score
brier.loss = (p - test$survived)^2
  
# Cross entropy
# PLM
cross.loss = -1*(log(p)*test$survived + log(1-p)*(1-test$survived))
  
results[,2] = c(tmp$overall[1], tmp$byClass[c(2,1,5)],
                  as.numeric(pROC::roc(response=test$survived,predictor=p)$auc),
                  mean(brier.loss),mean(cross.loss))

results

#Ridge regression
#Let???s see how the coefficients will change with Ridge regression. Ridge regression imposes a penalty on the coefficients to shrink them towards zero, but it doesn???t set any coefficients to zero. Thus, it doesn???t automatically do feature selection for us (i.e. all the variables we feed in the algorithm are retained in the final linear formula, see below).
#lasso
#Now, let???s take a look at the lasso regression. This method uses a different penalization approach which allows some coefficients to be exactly zero. Thus, lasso performs feature selection and returns a final model with lower number of parameters.
#In statistics and machine learning, lasso (least absolute shrinkage and selection operator; also Lasso or LASSO) is a regression analysis method that performs both variable selection and regularization in order to enhance the prediction accuracy and interpretability of the resulting statistical model.
#Is lasso always better than Ridge?
#In cases where only a small number of predictor variables are significant, lasso regression tends to perform better because it's able to shrink insignificant variables completely to zero and remove them from the model.
#How do you decide between Lasso and ridge?
#Lasso tends to do well if there are a small number of significant parameters and the others are close to zero (ergo: when only a few predictors actually influence the response). Ridge works well if there are many large parameters of about the same value (ergo: when most predictors impact the response).
#Suppose we have more or less balanced dataset, however we would like to use cost weighted to
#use all the rows in dataset. In this case as well ist it optimal to use 0,5 as appropriate TH?

###Alpha value for ridge is 1 so why cross validation for that?


#Linear regression algorithm works by selecting coefficients for each independent variable that minimizes a loss function. However, if the coefficients are large, they can lead to over-fitting on the training dataset, and such a model will not generalize well on the unseen test data. To overcome this shortcoming, we'll do regularization, which penalizes large coefficients. The following sections of the guide will discuss various regularization algorithms.
##################
# Function creates a 'formula' object
gen.fm = function(dep='survived',x=features) {
  # This will create the 'dep~1' string - which is y = beta_0 model
  # paste() is a useful function... it links strings
  spec = paste(dep,'~1',sep='')
  # If there are no features - the vector 'x' has no elements - return
  if (is.null(x)) return(as.formula(spec))
  # Number of features
  NV = length(x)
  # Loop over all features - we add one feature at a time
  for (v in 1:NV) spec = paste(spec,'+',x[v],sep='')
  return(as.formula(spec))
}
# Classification problem with titanic
titanicDS = read.csv(file='titanic.csv')
head(titanicDS)

#Checking for the empty rows
any(is.na(titanicDS))
apply(titanicDS, 2, function(x) any(is.na(x)))
any(is.na(titanicDS$age))


#Exploring the features in titanic dataset and performing some features transformation
#First lets go through the features which have strings in the value

#################Names of the passenger in titanic######################
titanicDS$name
summary(titanicDS$name)
table(titanicDS$name)###----No significant information ffound that could help in the prediction. Will not be used for prediction

############################TopF#########################
titanicDS$TopF
summary(titanicDS$TopF)
table(titanicDS$TopF)

#transfering the topf variable into dummy to make it usable for our model
titanicDS$topFt = (titanicDS$TopF=="Top")*1
titanicDS$nTopFt = (titanicDS$TopF=="noTop")*1
table(titanicDS$nTopFt)

##################MidF#########################
titanicDS$MidF
summary(titanicDS$MidF)
table(titanicDS$MidF)

#transfering the midf variable into dummy to make it usable for our model
titanicDS$midFt = (titanicDS$MidF=="Mid")*1
titanicDS$nmidFt = (titanicDS$MidF=="noMid")*1
table(titanicDS$midFt)

#################FemaleF#########################
titanicDS$FemaleF
summary(titanicDS$FemaleF)
table(titanicDS$FemaleF)

#transfering the femaleF variable into dummy to make it usable for our model
titanicDS$female = (titanicDS$FemaleF=="Female")*1
titanicDS$male = (titanicDS$FemaleF=="Man")*1
table(titanicDS$female)

########################ParentF######################
titanicDS$ParentF
summary(titanicDS$ParentF)
table(titanicDS$ParentF)

#transfering the parentF variable into dummy to make it usable for our model
titanicDS$parent = (titanicDS$ParentF=="Parent")*1
titanicDS$noParent = (titanicDS$ParentF=="noParent")*1
table(titanicDS$parent)

#########################ChildF####################
titanicDS$ChildF
summary(titanicDS$ChildF)
table(titanicDS$ChildF)

#transfering the childF variable into dummy to make it usable for our model
titanicDS$child = (titanicDS$ChildF=="child")*1
titanicDS$noChild = (titanicDS$ChildF=="noChild")*1
table(titanicDS$child)

#########################age######################
titanicDS$age
summary(titanicDS$age)
table(titanicDS$age)

#transfering the age variable into age in days
titanicDS$dayAge = floor(titanicDS$age*365)
table(titanicDS$dayAge)
summary(titanicDS$dayAge)


################Removing some of rows where value of age in NA##############
any(is.na(titanicDS$age))
titanicDS=titanicDS[!is.na(titanicDS$age),]

#Splitting data sets into train and test dataset
set.seed(100)
idx = sample(1:dim(titanicDS)[1],size=floor(dim(titanicDS)[1]*0.8),replace=F)
train = titanicDS[+idx,]
test  = titanicDS[-idx,]

#Check for data imbalance
idx.good = which(train$survived==0)
idx.bad  = which(train$survived==1)
length(idx.good)
length(idx.bad)

# Dataset is more or less balanced, however, we will will cost weigted approch to make our dataset equal for both survived and not survived and will use threshhold of 0.5 
wgt   = rep(0,dim(train)[1])
wgt[train$survived==1] = 0.5/length(idx.bad)
wgt[train$survived==0] = 0.5/length(idx.good)
train$wgt = wgt
head(train)


#There are some limitations if many features are included:
#Estimation uncertainty - too many parameters.
#Over-fitting is likely.

# Let's estimate couple of models
################################
#LR
spec_one = as.formula(survived~Top+Mid+female+parent+child+topFt+midFt+nTopFt+nmidFt+male+female+noParent+noChild)
p1 = predict(glm(spec_one,data=train,weights=wgt,family='binomial'),new=test)
p1 = 1/(1+exp(-p))
hist(p1, breaks = 50)
summary(p1)
length(p1)

#########################
#Preparing data for ridge, lasso and ET
features = c('Top','Mid','female','parent','child','topFt','midFt','nTopFt','nmidFt','male','female','noParent','noChild')
x = as.matrix(train[,features])
y = as.matrix(train[,'survived'])
newx = as.matrix(test[,features])
thr = 0.5

#Ridge regression
#Let???s see how the coefficients will change with Ridge regression. Ridge regression imposes a penalty on the coefficients to shrink them towards zero, but it doesn???t set any coefficients to zero. Thus, it doesn???t automatically do feature selection for us (i.e. all the variables we feed in the algorithm are retained in the final linear formula, see below).
lasso.Ri = cv.glmnet(x=x,y=y,type.measure='deviance',nfolds=10,family='binomial',alpha=0)
# Check coefficients
coefficients(lasso.Ri,s='lambda.min')
p2 = (1/(1+exp(-predict(lasso.Ri,newx=newx,s='lambda.min'))))
hist(p2, breaks = 50)
summary(p2)
length(p2)



##############################
#Lasso
#lasso
#Now, let???s take a look at the lasso regression. This method uses a different penalization approach which allows some coefficients to be exactly zero. Thus, lasso performs feature selection and returns a final model with lower number of parameters.
#In statistics and machine learning, lasso (least absolute shrinkage and selection operator; also Lasso or LASSO) is a regression analysis method that performs both variable selection and regularization in order to enhance the prediction accuracy and interpretability of the resulting statistical model.
#Is lasso always better than Ridge?
#In cases where only a small number of predictor variables are significant, lasso regression tends to perform better because it's able to shrink insignificant variables completely to zero and remove them from the model.
#How do you decide between Lasso and ridge?
#Lasso tends to do well if there are a small number of significant parameters and the others are close to zero (ergo: when only a few predictors actually influence the response). Ridge works well if there are many large parameters of about the same value (ergo: when most predictors impact the response).

lasso.lr = cv.glmnet(x=x,y=y,type.measure='deviance',nfolds=10,family='binomial',alpha=1)
# Check coefficients
coefficients(lasso.lr,s='lambda.min')
p3 = (1/(1+exp(-predict(lasso.lr,newx=newx,s='lambda.min'))))
hist(p3, breaks = 50)
summary(p3)
length(p3)

##########################
#Elastic net
#Elastic net regression combines the properties of ridge and lasso regression. It works by penalizing the mode
lasso.en = cv.glmnet(x=x,y=y,type.measure='deviance',nfolds=10,family='binomial',alpha=0.5)
coefficients(lasso.en,s='lambda.min')
p4 = (1/(1+exp(-predict(lasso.en,newx=newx,s='lambda.min'))))
hist(p4, breaks = 50)
summary(p4)
length(p4)




results = data.frame(metrics = c('Accuracy','Specificity','Sensitivity','Precision','AUC','Brier score','Cross entropy'),
                     p1=NA)
reference = as.factor(test$survived)

prediction = as.factor((p>0.5)*1)
tmp = confusionMatrix(data=prediction,reference=reference,positive='1')

# Brier score
brier.loss = (p - test$survived)^2

# Cross entropy
# PLM
cross.loss = -1*(log(p)*test$survived + log(1-p)*(1-test$survived))

results[,2] = c(tmp$overall[1], tmp$byClass[c(2,1,5)],
                as.numeric(pROC::roc(response=test$survived,predictor=p)$auc),
                mean(brier.loss),mean(cross.loss))

results

