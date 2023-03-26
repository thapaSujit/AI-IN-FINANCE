# rm(list=ls())
library(caret)
library(glmnet)
library(tree)
library(rpart)
library(rpart.plot)
library(ranger)
install.packages('factoextra')
library(factoextra)
#install.packages('dummy')
library(dummies)
install.packages('cluster')
library(cluster)
#library(KMediansR) remove.packages("KMediansR")

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

##################################
# Datasets - corp default again
##################################
corpdef = read.csv(file='corpdef.csv')
# Stratify the dataset into training and testing
head(corpdef)
N = dim(corpdef)[1]
set.seed(200)
idx = sample(1:N,size=floor(N*0.8),replace=F)
train = corpdef[+idx,]
test  = corpdef[-idx,]
head(train)
colnames(train)

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

##################################
# Distance matrix
##################################
colnames(train.uo)
zscore.train = scale(train.uo[,-1])
# We do the same with training dataset!
zscore.test = scale(test[,-1],)
# Euclidean distance
dist.eucl = dist(zscore.train,method='euclidean')
dist.eucl.mat = as.matrix(dist.eucl)
head(dist.eucl.mat)
# Example
dist.eucl.mat[1:5,1:5]
# Vizualize distance - it will take some time
fviz_dist(dist.eucl)

##################################
# K-MEANS
##################################
# Given the distance matrix - Fin 7 clusters
K = 7
res.lloyd <- kmeans(zscore.train,centers=K,iter.max=50,nstart=20,algorithm='Lloyd')
# Check number of observations in cluster
table(res.lloyd$cluster)
# Are clusters related to Bankruptcy?
table(res.lloyd$cluster,train.uo$Bankrupt.)
# At first sight it look nice
# Perhaps we can create new variables - where the given observation belongs?
# Now let's create dummies corresponding to K-1 clusters, i.e. create new features. 1 if the observation belongs to the given cluster and 0 otherwise
lloyd = dummy(res.lloyd$cluster)
head(lloyd)
lloyd = lloyd[,-5] # We remove one cluster (K-1) (remember colinearity issues) and I will remove 5th because it has only few observations
colnames(lloyd) = paste('ll',c(1:4,6:7),sep='')
# Add to the training dataset as a new set of variables
train.ll = data.frame(train.uo,lloyd)
# Perhaps you want to estimate a model on a training dataset. Well you could but first...
# I also need to create variables for the testing dataset - but how?
# How do we assign an observation from testing dataset to the cluster created in the training dataset?
# Let's use the following rule:
# 1) Observation from the testing dataset belongs to the cluster represented by the closest centroid.
# Other rules are possible: Observation from the testing dataset belongs to the cluster to which it's distance to the observation in the cluster is closest on average
# We will use 1st
##################################
# Start the operation
##################################
# Extract centroid
ll.centroid = res.lloyd$centers
dim(ll.centroid)
# Create a dataset of new variables
NT = dim(zscore.test)[1]
km.test = matrix(0,nrow=NT,ncol=K)
colnames(km.test) = paste('ll',c(1:7),sep='')
head(km.test)
# Now we need to assign each observation from testing dataset to one of the clusters
for (i in 1:NT) {
  # We need to find the distance between given observation from testing dataset to each of the centroids
  tmp = as.matrix(dist(rbind(ll.centroid,zscore.test[i,]),method='euclidean'))
  # Find to which centroid the observation is closest to
  opt.clus = which(tmp[dim(tmp)[1],1:K]==min(tmp[dim(tmp)[1],1:K]))
  # Just add 1 there
  km.test[i,opt.clus] = 1
}
# Check if we have observations in every cluster
apply(km.test,2,mean)
# Remove the fifth cluster - just as before
km.test = km.test[,-5]
# Add to training dataset
test = data.frame(test,km.test)

# Now we can estimate the model
# We will use two specifications. One with and one without clusters
specs.old = gen.fm(dep='Bankrupt.',x=names(train.ll)[c(2:95)])
specs.new = gen.fm(dep='Bankrupt.',x=names(train.ll)[c(2:101)])
# Logistic regression
m0 = glm(specs.old,data=train.ll,family='binomial')
summary(m0)
p0 = (1/(1+exp(-predict(m0, new=test))))
hist(p0); summary(p0)
#
m1 = glm(specs.new,data=train.ll,family='binomial')
summary(m1)
p1 = (1/(1+exp(-predict(m1, new=test))))
hist(p1); summary(p1)
# Convert to factors
thr = 0.5
ref = as.factor(test$Bankrupt.)
#
# Evaluate and compare
f0 = as.factor((p0>thr)*1)
f1 = as.factor((p1>thr)*1)
confusionMatrix(f0,reference=ref,positive='1')
confusionMatrix(f1,reference=ref,positive='1')

# Random forest
rf.tree = ranger(specs.old,
                 data=train.ll,
                 num.trees=2000,mtry=9,min.node.size=20,max.depth=0)
p2 = predict(rf.tree,data=test)$predictions
rf.tree = ranger(specs.new,
                 data=train.ll,
                 num.trees=2000,mtry=9,min.node.size=20,max.depth=0)
p3 = predict(rf.tree,data=test)$predictions
# Evaluate and compare
f2 = as.factor((p2>thr)*1);
f3 = as.factor((p3>thr)*1)
confusionMatrix(f2,reference=ref,positive='1')
confusionMatrix(f3,reference=ref,positive='1')

##################################
# Could we use K-Means directly for prediction purposes?
##################################
# We could
K = 7
hw7    <- kmeans(zscore.train,centers=K,iter.max=50,nstart=20,
                    algorithm='Hartigan-Wong')
class_hw7 = table(hw7$cluster,train.uo$Bankrupt.)
# What is the prediction if observations fall into this cluster?
class_hw7 = class_hw7[,2]/apply(class_hw7,1,sum)
class_hw7 = as.numeric(ifelse(class_hw7>0.5,1,0))
# Now looking at the testing dataset - classify each observation from testing into one of the clusters
hw.centroid = hw7$centers; dim(hw.centroid)
NT = dim(zscore.test)[1]
f4 = c()
for (i in 1:NT) {
  tmp = as.matrix(dist(rbind(hw.centroid,zscore.test[i,]),method='euclidean'))
  opt.clus = which(tmp[dim(tmp)[1],1:K]==min(tmp[dim(tmp)[1],1:K]))
  f4[i] = class_hw7[opt.clus]
}
f4 = as.factor(f4)
confusionMatrix(f4,reference=ref,positive='1')

# Could we use K-Means directly for prediction purposes?
# What if we use more clusters?
K = 15
hw15    <- kmeans(zscore.train,centers=K,iter.max=50,nstart=20,
                 algorithm='Hartigan-Wong')
class_hw15 = table(hw15$cluster,train.uo$Bankrupt.)
class_hw15 = class_hw15[,2]/apply(class_hw15,1,sum)
class_hw15 = as.numeric(ifelse(class_hw15>0.5,1,0))
# Extract centroid
hw.centroid = hw15$centers; dim(hw.centroid)
NT = dim(zscore.test)[1]
f5 = c()
for (i in 1:NT) {
  tmp = as.matrix(dist(rbind(hw.centroid,zscore.test[i,]),method='euclidean'))
  opt.clus = which(tmp[dim(tmp)[1],1:K]==min(tmp[dim(tmp)[1],1:K]))
  f5[i] = class_hw7[opt.clus]
}
f5 = as.factor(f5)
confusionMatrix(f5,reference=ref,positive='1')
# Not very accurate....
# Perhaps we are combining to many variables and introduce too much noise

##################################
# PAM - Partition around medoids
##################################
# Use PAM to make predictions directly
K = 7
pm = pam(zscore.train, k=K, metric = "manhattan", stand = FALSE)
names(pm)
dim(pm$medoids)
pm$clustering
class_pm7 = table(pm$clustering,train.uo$Bankrupt.)
class_pm7
#Average in class
class_pm7 = class_pm7[,2]/apply(class_pm7,1,sum)
class_pm7
class_pm7 = as.numeric(ifelse(class_pm7>0.5,1,0))
class_pm7
#
pm.medioids = pm$medoids; dim(pm.medioids)
NT = dim(zscore.test)[1]
f6 = c()
for (i in 1:NT) {
  tmp = as.matrix(dist(rbind(pm.medioids,zscore.test[i,]),method='manhattan'))
  opt.clus = which(tmp[dim(tmp)[1],1:K]==min(tmp[dim(tmp)[1],1:K]))
  f6[i] = class_pm7[opt.clus]
}
f6 = as.factor(f6)
confusionMatrix(f6,reference=ref,positive='1')
# Still not good

##################################
# Assignment - use clusters created with PAM as variables and create a prediction model (just like we did with k-means early on)
# Use RF and LASSO
##################################
