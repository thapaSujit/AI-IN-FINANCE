#Linear regression algorithm works by selecting coefficients for each independent variable that minimizes a loss function. However, if the coefficients are large, they can lead to over-fitting on the training dataset, and such a model will not generalize well on the unseen test data. To overcome this shortcoming, we'll do regularization, which penalizes large coefficients. The following sections of the guide will discuss various regularization algorithms.
##################
# Reading the dataset
titanicDS = read.csv(file='titanic.csv')
head(titanicDS)

#Checking the whole data set to examine if there are any empty rows (i.e NA values)
any(is.na(titanicDS))
apply(titanicDS, 2, function(x) any(is.na(x)))
any(is.na(titanicDS$age))


#Exploring the features in titanic data set and performing features transformation on some features and handling the empty rows if any
#First lets go through the features which have strings in the value

#################Names of the passenger in titanic######################
titanicDS$name
summary(titanicDS$name)
table(titanicDS$name)###----No significant information found that could help in the prediction. Will not be used for prediction

############################TopF#########################
titanicDS$TopF
summary(titanicDS$TopF)
table(titanicDS$TopF)

#transfering the topf variable into dummy to make it usable for our model
titanicDS$topFt = (titanicDS$TopF=="Top")*1
titanicDS$nTopFt = (titanicDS$TopF=="noTop")*1
table(titanicDS$nTopFt)
hist(titanicDS$topFt)

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

#Splitting data sets into train and test data set
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
#wgt   = rep(0,dim(train)[1])
#wgt[train$survived==1] = 0.5/length(idx.bad)
#wgt[train$survived==0] = 0.5/length(idx.good)
#train$wgt = wgt
#head(train)

# Undersampling majority
#set.seed(300)
#tmp = idx.good[sample(1:length(idx.good),size=length(idx.bad),replace=F)]
#train.um = rbind(train[tmp,],train[idx.bad,])#combining two data sets
#dim(train.um)
#summary(train.um$default)
# Just some randomization - we do not want defaults and non-deafults to be stacked one next to each other (just to be sure)
#train.um = train.um[sample(1:dim(train.um)[1],size=dim(train.um)[1],replace=F),]

# Undersampling majority and oversampling minority (2 times the number of minority)
#set.seed(300)
#tmp.good = idx.good[sample(1:length(idx.good),size=length(idx.bad)*2,replace=F)]
# Here we need replacement = T - why?
#tmp.bad  = idx.bad[sample(1:length(idx.bad),size=length(idx.bad)*2,replace=T)] # Oversampling
#train.uo = rbind(train[tmp.good,],train[tmp.bad,])
# Just some randomization - we do not want defaults and non-deafults to be stacked one next to each other (just to be sure)
#train.uo = train.uo[sample(1:dim(train.uo)[1],size=dim(train.uo)[1],replace=F),]
#dim(train.uo)
# summary(train.uo$default)

#There are some limitations LR if many features are included:
#Estimation uncertainty - too many parameters.
#Over-fitting is likely.
#The reason is that the target label has no linear correlation with the features.
thr = 0.5
#head(test$survived)
ref = as.factor(test$survived)

# Let's estimate couple of models
################################
#LR
#In logistic regression, the algorithm uses input features to predict the likelihood that a given example belongs to one of the two possible classes. 
#Logistic regression can be prone to overfitting, especially when the dataset is small and has a large number of input features. Overfitting occurs when the model fits the noise in the data rather than the underlying pattern, which can lead to poor performance on new, unseen examples.
spec_one = as.formula(survived~Top+Mid+female+parent+child+topFt+midFt+nTopFt+nmidFt+male+female+noParent+noChild+dayAge)
#In case selecting from lot of features suning this one is best
#specs = gen.fm(dep='default',x=names(train.uo)[c(11:18,20:45,47:84,86:100,102:142,144:175)])
p1 = predict(glm(spec_one,data=train,family='binomial'),new=test)
p1 = 1/(1+exp(-p1))
hist(p1, breaks = 50)
summary(p1)
plot(p1)
f1 = as.factor((p1>thr)*1)
confusionMatrix(f1,reference=ref,positive='1')

#########################
#Preparing data for ridge, lasso and ET
features = c('Top','Mid','female','parent','child','topFt','midFt','nTopFt','nmidFt','male','female','noParent','noChild', 'dayAge')
x = as.matrix(train[,features])
head(x)
y = as.matrix(train[,'survived'])
head(y)
newx = as.matrix(test[,features])
head(newx)

#Ridge regression
#ridge classifier adds a regularization term to the cost function to penalize large coefficients, which can help to prevent overfitting and improve the interpretability of the model.
#Let’s see how the coefficients will change with Ridge regression. Ridge regression imposes a penalty on the coefficients to shrink them towards zero, but it doesn’t set any coefficients to zero. Thus, it doesn’t automatically do feature selection for us (i.e. all the variables we feed in the algorithm are retained in the final linear formula).
lasso.Ri = cv.glmnet(x=x,y=y,type.measure='deviance',nfolds=10,family='binomial',alpha=0)
# Check coefficients
coefficients(lasso.Ri,s='lambda.min')
p2 = (1/(1+exp(-predict(lasso.Ri,newx=newx,s='lambda.min'))))
hist(p2, breaks = 50)
summary(p2)
plot(p2)

f2 = as.factor((p2>thr)*1);
confusionMatrix(f2,reference=ref,positive='1')


##############################
#Lasso
#lasso
#Now, let’s take a look at the lasso regression. This method uses a different penalization approach which allows some coefficients to be exactly zero. Thus, lasso performs feature selection and returns a final model with lower number of parameters.
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
plot(p3)

f3 = as.factor((p3>thr)*1);
confusionMatrix(f3,reference=ref,positive='1')

##########################
#Elastic net
#Elastic net regression combines the properties of ridge and lasso regression. It works by penalizing the mode
#the elastic net classifier adds a regularization term to the cost function that combines the L1 regularization term used in lasso regression and the L2 regularization term used in ridge regression. This allows the elastic net classifier to simultaneously shrink some coefficients all the way down to zero and shrink all the coefficients, which can help to automatically select only the most relevant input features and reduce overfitting.
lasso.en = cv.glmnet(x=x,y=y,type.measure='deviance',nfolds=10,family='binomial',alpha=0.5)
coefficients(lasso.en,s='lambda.min')
p4 = (1/(1+exp(-predict(lasso.en,newx=newx,s='lambda.min'))))
hist(p4, breaks = 50)
summary(p4)
length(p4)
plot(p4)

f4 = as.factor((p4>thr)*1);
confusionMatrix(f4,reference=ref,positive='1')

##########################
#DT
#It is a simple and widely used algorithm that is based on the idea of splitting the training dataset into smaller and smaller subsets based on the values of the input features. 
dt = rpart(spec_one,data=train,
           method='class',model=TRUE,
           control=rpart.control(cp=0,xval=10))
rpart.plot(dt)
p5 = predict(dt,new=test)[,2]
summary(p5)
plot(p5)

f5 = as.factor((p5>thr)*1);
confusionMatrix(f5,reference=ref,positive='1')


#####################
#Bagging
#Recall that bagging is based on the idea that averaging unbiased but
#potentially over-fitted model’s predictions will reduce the
#out-of-sample error.
#To use bagging with decision tree classifiers, you would train multiple decision tree models on different subsets of the training data, and then average (for regression tasks) or vote (for classification tasks) the predictions of the individual models to make a final prediction. This can help to reduce the variance of the predictions and improve the overall performance of the model.
# We meed to store predictions based on bootstrapped datasets
bagg.pred = array(NA,dim=c(dim(test)[1],100))
# We loop over bootstrapped data
# This takes some time
for (b in 1:100) {
  print(b)
  # Randomly select observations
  idx = sample(1:dim(train)[1],size=dim(train)[1],replace=T)
  # The change here is only trin.uo[idx,] -> randomly selected observations are used to estimate the DC
  tb = rpart(spec_one,data=train[idx,],
             method='class',model=TRUE,
             control=rpart.control(cp=0,xval=10))
  # Generate and store predictions
  bagg.pred[,b] = predict(tb,new=test)[,2]
}
# Calculate the average prediction (probability)
p6 = apply(bagg.pred,1,mean)

f6 = as.factor((p6>thr)*1);
confusionMatrix(f6,reference=ref,positive='1')

#######################
#RF
#An issue with bagging:
#With trees, bagging leads to (positively) correlated trees, i.e.
#predictions are similar, because trees are similar.
#Same set of important predictors is chosen to split the trees.
#Averaging works best for uncorrelated predictions (recall the
# portfolio theory...).
#Random forest uses bagging as well, but with a twist.
#At each split we are not searching from all features the one that
#gives us the best split, instead random set of m features is used

depth = c(0,6,12)
ND = length(depth)
B = c(500,1000,3000)
NB = length(B)
mtry = c(3,5,7)
NR = length(mtry)
cv = 10
NT = dim(train)[1]
# MSE for CV
rf.cv = array(NA,dim=c(NB,NR,ND,cv))
dimnames(rf.cv)[[1]] = paste('Trees',B)
dimnames(rf.cv)[[2]] = paste('Try',mtry,'features')
dimnames(rf.cv)[[3]] = paste('Depth',depth)
dimnames(rf.cv)[[4]] = paste('CV sample',1:cv)

rf.cv.ave = array(NA,dim=c(NB,NR,ND))
dimnames(rf.cv.ave)[[1]] = paste('Trees',B)
dimnames(rf.cv.ave)[[2]] = paste('Try',mtry,'features')
dimnames(rf.cv.ave)[[3]] = paste('Depth',depth)

# Running the CV excercise
# Number of trees
for (b in 1:NB) {
  num.trees = B[b]
  # Number of mtry
  for (m in 1:NR) {
    num.try = mtry[m]
    # Depth
    for (d in 1:ND) {
      num.depth = depth[d]
      # Now cross-validation      
      for (r in 1:cv) {
        # Select data
        idx = c(((r-1)*(NT/10)+1):(r*(NT/10)))
        zs.train.cvin = train[-idx,]
        zs.train.cout = train[+idx,]
        
        # Estimate the model
        rf.tree = ranger(spec_one,data=zs.train.cvin,
                         num.trees=num.trees,mtry=num.try,min.node.size=5,max.depth=num.depth)  
        pred.rf.cv = predict(rf.tree,data=zs.train.cout)$predictions
        summary(pred.rf.cv)
        #Why is entropy between 0 and 1?
        #The entropy or the impurity measure can only take value from 0 to 1 as the probability ranges from 0 to 1
        pred.rf.cv[pred.rf.cv>0.999] = 0.999
        pred.rf.cv[pred.rf.cv<0.001] = 0.001
        rf.cv[b,m,d,r] = mean(-1*(log(pred.rf.cv)*zs.train.cout$survived + log(1-pred.rf.cv)*(1-zs.train.cout$survived)))
          #mean((pred.rf.cv - zs.train.cout$survived)^2)
          #mean(-1*(log(pred.rf.cv)*zs.train.cout$survived + log(1-pred.rf.cv)*(1-zs.train.cout$survived)))
      }
      # Average
      rf.cv.ave[b,m,d] = mean(rf.cv[b,m,d,])
    }
  }
  print(paste('Number of trees',B[b]))
}
rf.cv.ave
rf.tree = ranger(spec_one,
                 data=train,
                 num.trees=1000,mtry=5,min.node.size=5,max.depth=6)  
p7 = predict(rf.tree,data=test)$predictions

f7 = as.factor((p7>thr)*1);
confusionMatrix(f7,reference=ref,positive='1')

########################
####Evaluation Matrix
# We will make some adjustments even here - why? Completely different reason - cross-entropy
#In order to make the prediction compatible for the cross entropy we transform the predictions
#to be within 0,999 and ,001
#Why is entropy between 0 and 1?
#The entropy or the impurity measure can only take value from 0 to 1 as the probability ranges from 0 to 1
p1[p1>0.999] = 0.999
p1[p1<0.001] = 0.001
p2[p2>0.999] = 0.999
p2[p2<0.001] = 0.001
p3[p3>0.999] = 0.999
p3[p3<0.001] = 0.001
p4[p4>0.999] = 0.999
p4[p4<0.001] = 0.001
p5[p5>0.999] = 0.999
p5[p5<0.001] = 0.001
p6[p6>0.999] = 0.999
p6[p6<0.001] = 0.001
p7[p7>0.999] = 0.999
p7[p7<0.001] = 0.001
predicts = cbind(test$survived,p1,p2,p3,p4,p5,p6,p7)
head(predicts)
results = data.frame(metrics = c('Accuracy','Specificity','Sensitivity','Precision','AUC','Brier score','Cross entropy'),
                     p1=NA,p2=NA,p3=NA,p4=NA,p5=NA,p6=NA,p7=NA)

reference = as.factor(predicts[,1])

for (i in 2:8) {
  prediction = as.factor((predicts[,i]>thr)*1)
  tmp = confusionMatrix(data=prediction,reference=reference,positive='1')
  
  # Brier score
  brier.loss = (predicts[,i] - test$survived)^2
  
  # Cross entropy
  # PLM
  cross.loss = -1*(log(predicts[,i])*test$survived + log(1-predicts[,i])*(1-test$survived))
  
  results[,i] = c(tmp$overall[1], tmp$byClass[c(2,1,5)],
                  as.numeric(pROC::roc(response=predicts[,1],predictor=predicts[,i])$auc),
                  mean(brier.loss),mean(cross.loss))
}
results


######################################################################################################################################################################################
library(caret)
library(glmnet)
library(tree)
library(rpart)
library(rpart.plot)
library(ranger)
#install.packages('factoextra')
library(factoextra)
#install.packages('dummy')
library(dummies)
#install.packages('cluster')
library(cluster)


##################################
# Distance matrix
##################################
head(train)
zscore.train = scale(train[,c(-3,-4,-6,-8,-9,-10,-11,-12,-13,-14)])
head(zscore.train)
# We do the same with testing dataset!
zscore.test = scale(test[,c(-3,-4,-6,-8,-9,-10,-11,-12,-13,-14)],)
head(zscore.test)
# Euclidean distance for the figures
dist.eucl = dist(zscore.train,method='euclidean')
dist.eucl.mat = as.matrix(dist.eucl)
head(dist.eucl.mat)

# Vizualize distance - it will take some time
fviz_dist(dist.eucl)

##################################
# K-MEANS
#K-means clustering is a type of unsupervised machine learning algorithm used to classify data into groups, or clusters. It works by dividing a dataset into a specified number of clusters, and then assigning each data point to one of the clusters based on its similarity to other points in the cluster. This is done by first defining a distance measure (such as Euclidean distance), and then iteratively assigning points to the cluster with the closest mean, or center point.
#Standardize variables - Z-score standardization.
#Consider K = 3 with Euclidean distance.
##################################
# Given the distance matrix - Fin 7 clusters
K = 7
any(is.na(zscore.train))
apply(zscore.train, 2, function(x) any(is.na(x)))
#zscore.train[,5]
#titanicDS=titanicDS[!is.na(titanicDS$age),]

#Using llyods algorithm for K means clustering
#The basic idea behind Lloyd's algorithm is to start with a set of initial cluster centers, and then iteratively improve the solution by reassigning each data point to the cluster with the closest center, and then recalculating the cluster centers based on the new assignments. This process is repeated until the cluster assignments stop changing, at which point the algorithm has converged and the final clusters are returned.
res.lloyd <- kmeans(zscore.train,centers=K,iter.max=50,nstart=20,algorithm='Lloyd')
# Check number of observations in cluster
table(res.lloyd$cluster)
# Are clusters related to survival?
table(res.lloyd$cluster,train$survived)

# At first sight it look nice
# Perhaps we can create new variables - where the given observation belongs?
# Now let's create dummies corresponding to K-1 clusters, i.e. create new features. 1 if the observation belongs to the given cluster and 0 otherwise
lloyd = dummy(res.lloyd$cluster)
head(lloyd)
mean(lloyd[,5])
lloyd = lloyd[,-5] # We remove one cluster (K-1) (remember colinearity issues) and I will remove 5th because it has only few observations
colnames(lloyd) = paste('ll',c(1:4,6:7),sep='')
# Add to the training dataset as a new set of variables
train.ll = data.frame(train,lloyd)
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

###Now using train.ll and test data on different classification algorithms
