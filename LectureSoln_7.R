# rm(list=ls())
library(caret)
#install.packages('glmnet')
library(glmnet)
library(tree)
#install.packages("tree")
library(rpart)
#install.packages("rpart.plot")
#install.packages("ranger")
library(rpart.plot)
library(ranger)

# Function creates a 'formula' object
gen.fm = function(dep='default',x=features) {
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


#Data sets
titanic = read.csv(file='Titanic.csv')
zsnew = read.csv(file='zsnew.csv')
corpdef = read.csv(file='corpdef.csv')
byty = read.csv(file='byty_clean.csv')


#####################################
#####################################
# Start with credit-risk models
#####################################
#####################################

summary(zsnew$default)
# 1) Split datasets to Training and Testing
N = dim(zsnew)[1]
set.seed(200)
idx = sample(1:N,size=floor(N*0.8),replace=F)
train = zsnew[+idx,]
test  = zsnew[-idx,]
# The to be predicted outcomes from testing dataset
ref = as.factor(test$default)

# 2) Create balanced dataset - one is enough
idx.good = which(train$default==0); length(idx.good)
idx.bad  = which(train$default==1); length(idx.bad)
# Let's use under/over-sampling approach with ratio of 1.5
rt = 1.5
set.seed(300)
# Good loans
tmp.good = idx.good[sample(1:length(idx.good),size=length(idx.bad)*rt,replace=F)]
# The bad loans
tmp.bad  = idx.bad[sample(1:length(idx.bad),size=length(idx.bad)*rt,replace=T)] # Oversampling
# Combine datasets
train.uo = rbind(train[tmp.good,],train[tmp.bad,])
# Randomize observations in the training dataset
train.uo = train.uo[sample(1:dim(train.uo)[1],size=dim(train.uo)[1],replace=F),]
# Check
dim(train.uo)
summary(train.uo$default)
# Given that the dataset is balanced, the threshold is
thr = 0.5

#####################################
# The benchmark 
#####################################
specs = gen.fm(dep='default',x=names(train.uo)[c(11:18,20:45,47:84,86:100,102:142,144:175)])
m0 = glm(specs, data=train.uo, family='binomial')
# Predicted probability
p0 = (1/(1+exp(-predict(m0, new=test))))
# Confert to factors
f0 = as.factor((p0>thr)*1)
confusionMatrix(f0,reference=ref,positive='1')

#####################################
# The LASSO-can be used for both feature and model selection
#####################################
# Prepare data
x = as.matrix(train.uo[,all.vars(specs)[-1]])
newx = as.matrix(test[,all.vars(specs)[-1]])
y = as.matrix(train.uo[,all.vars(specs)[1]])
# This takes some time....
lasso.lr = cv.glmnet(x=x,y=y,type.measure='deviance',nfolds=10,family='binomial',alpha=1)
# Check coefficients
coefficients(lasso.lr,s='lambda.min')
p1 = (1/(1+exp(-predict(lasso.lr,newx=newx,s='lambda.min'))))
f1 = as.factor((p1>thr)*1)
confusionMatrix(f1,reference=ref,positive='1')

#####################################
# The RIDGE
#####################################
?cv.glmnet
lasso.Ri = cv.glmnet(x=x,y=y,type.measure='deviance',nfolds=10,family='binomial',alpha=0)
# Check coefficients
coefficients(lasso.Ri,s='lambda.min')
p1 = (1/(1+exp(-predict(lasso.Ri,newx=newx,s='lambda.min'))))
f1 = as.factor((p1>thr)*1)
confusionMatrix(f1,reference=ref,positive='1')

#####################################
# The EN
cvm = cv.glmnet(x=x,y=y,type.measure='deviance',nfolds=10,family='binomial',alpha=0.5)
coefficients(cvm,s='lambda.min')
p1 = (1/(1+exp(-predict(cvm,newx=newx,s='lambda.min'))))
f1 = as.factor((p1>thr)*1)
confusionMatrix(f1,reference=ref,positive='1')
#####################################


#####################################
# The Decision tree - deep#Class is based on gini coefficient
#####################################
t0 = rpart(specs,data=train.uo,
           method='class',model=TRUE,
           control=rpart.control(cp=0,xval=10))
rpart.plot(t0)
p4 = predict(t0,new=test)[,2]
f4 = as.factor((p4>thr)*1)
confusionMatrix(f4,reference=ref,positive='1')
?rpart
#####################################
# Bagging Decision tree
#####################################
# We meed to store predictions based on bootstrapped datasets
bagg.pred = array(NA,dim=c(dim(test)[1],100))
# We loop over bootstrapped data
# This takes some time
for (b in 1:100) {
  print(b)
  # Randomly select observations
  idx = sample(1:dim(train.uo)[1],size=dim(train.uo)[1],replace=T)
  # The change here is only trin.uo[idx,] -> randomly selected observations are used to estimate the DC
  tb = rpart(specs,data=train.uo[idx,],
             method='class',model=TRUE,
             control=rpart.control(cp=0,xval=10))
  # Generate and store predictions
  bagg.pred[,b] = predict(tb,new=test)[,2]
}
# Calculate the average prediction (probability)
p5 = apply(bagg.pred,1,mean)
# Convert to factor
f5 = as.factor((p5>thr)*1)
confusionMatrix(f5,reference=ref,positive='1')

#####################################
# Visualize the distribution of predictions for a selected two observations
#####################################
dev.off()
par(mfrow=c(1, 2))
par(cex = 1.1)
par(oma = c(1, 1.5, 0.0, 0.0))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
par(mar = c(2.0, 2.0, 1.5, 0.5))
hist(bagg.pred[1800,], breaks=10,xlim=c(0,1))
p5s=apply(bagg.pred,1,quantile,p=0.05)
f5s = as.factor((p5s>thr)*1)
confusionMatrix(f5s,reference=ref,positive='1')


#####################################
# Random forest
#####################################
rf.tree = ranger(specs,
                 data=train.uo,
                 num.trees=1000,mtry=9,min.node.size=5,max.depth=0)  
p6 = predict(rf.tree,data=test)$predictions
f6 = as.factor((p6>thr)*1);
confusionMatrix(f6,reference=ref,positive='1')

#####################################

#####################################


#####################################
#####################################
# Let's play with firm default dataset
#####################################
#####################################
names(corpdef)
# Stratify the dataset into training and testing
N = dim(corpdef)[1]
set.seed(200)
idx = sample(1:N,size=floor(N*0.8),replace=F)
train = corpdef[+idx,]
test  = corpdef[-idx,]
# Create alternative datasets - that are more balanced
idx.good = which(train$Bankrupt.==0); length(idx.good)
idx.bad  = which(train$Bankrupt.==1); length(idx.bad)
# Undersampling majority and oversampling minority (3 times the number of minority)
rt = 3;
set.seed(300)
tmp.good = idx.good[sample(1:length(idx.good),size=length(idx.bad)*rt,replace=F)]
# Here we need replacement = T - why?
tmp.bad  = idx.bad[sample(1:length(idx.bad),size=length(idx.bad)*rt,replace=T)] # Oversampling
train.uo = rbind(train[tmp.good,],train[tmp.bad,])
# Just some randomization - we do not want defaults and non-deafults to be stacked one next to each other (just to be sure)
train.uo = train.uo[sample(1:dim(train.uo)[1],size=dim(train.uo)[1],replace=F),]
# Check
dim(train.uo)
summary(train.uo$Bankrupt.)


