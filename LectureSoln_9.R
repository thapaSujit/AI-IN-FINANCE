# rm(list=ls())
library(caret)
library(glmnet)
library(tree)
library(rpart)
library(rpart.plot)
library(ranger)
library(factoextra)
library(dummies)
library(cluster)


##################################
# Datasets - corp default again
##################################
corpdef = read.csv(file='corpdef.csv')
# Stratify the dataset into training and testing
N = dim(corpdef)[1]
set.seed(200)
idx = sample(1:N,size=floor(N*0.8),replace=F)
train = corpdef[+idx,]
test  = corpdef[-idx,]
train = train[,-95]
test     = test[,-95]
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
summary(train.uo)
summary(train.uo$Bankrupt.)

#############################
# Names corpdef 
#############################
# Standardized
#############################
names(train)
prof  = train.uo[,c(2:4,27:29,43,44,53,90)]
sprof = scale(prof)
debt  = train.uo[,c(93:95,89,67:68,41,36:38,15)]
sdebt = scale(debt)
#############################
# Standardized
#############################
prof_test  = test[,c(2:4,27:29,43,44,53,90)]
sprof_test = scale(prof_test)
debt_test  = test[,c(93:95,89,67:68,41,36:38,15)]
sdebt = scale(debt_test)
#############################
library(factoextra)
#############################
# Number of variables in sprof
N1 = dim(sprof)[2]
m = prcomp(x=sprof)
str(m)
get_eigenvalue(m)
# Weights in the matrix
dim(m$rotation)
head(m$rotation)
# Rows are variables and Columns are principal components
# Values are weights
# Extract principal components
pcc = sprof %*% m$rotation
# Check - uncorrelated?
round(cor(cbind(pcc,sprof)),2)
# How to select number of components
fviz_eig(m)
# Values above 1?
get_eig(m)
# Add to training dataset
train.uo = data.frame(train.uo,pcc)
# Add to testing dataset
pcc = as.matrix(prof_test) %*% m$rotation
test = data.frame(test,pcc)

#############################
# Estimate models - RF
#############################
specs.old = gen.fm(dep='Bankrupt.',x=names(train.uo)[c(2:95)])
length(2:95)
# Now spec withouth profitability measures but instead using 2 PCs
idx = c(2:4,27:29,43,44,53,90)
all = 2:97
all = all[-which(all %in% idx)]
length(all)
specs.new = gen.fm(dep='Bankrupt.',x=names(train.uo)[all])
# Convert to factors
thr = 0.5
ref = as.factor(test$Bankrupt.)
# Random forest
rf.tree = ranger(specs.old,
                 data=train.uo,
                 num.trees=5000,mtry=9,min.node.size=10,max.depth=0)
p1 = predict(rf.tree,data=test)$predictions
rf.tree = ranger(specs.new,
                 data=train.uo,
                 num.trees=5000,mtry=9,min.node.size=10,max.depth=0)
p2 = predict(rf.tree,data=test)$predictions
# Evaluate and compare
f1 = as.factor((p1>thr)*1);
f2 = as.factor((p2>thr)*1)
confusionMatrix(f1,reference=ref,positive='1')
confusionMatrix(f2,reference=ref,positive='1')
# Prediction accuracy is similar but we have used less observations.





