library(caret)
library(glmnet)
library(tree)
library(rpart)
library(rpart.plot)
library(ranger)
library(factoextra)
library(dummies)
library(cluster)
# rm(list=ls())
# Function creates a 'formula' object


################################################################
# Kickstarter data - classification problem
ps = read.csv(file='Kickstarter.csv')
head(ps)
#Checking for NA rows in the dataset
any(is.na(ps))
apply(ps, 2, function(x) any(is.na(x)))
################################################################

################################################################
# This is a dataset of a sample of Kickstarter campaigns.
# Projects from the online crowdfunding platform Kickstarter.
################################################################
# The variables are:
# SuccessfulBool    - whether the project was successful (1) or not (0)
# goal              - the amount of money that needs to be raised for a project to be successful 
# pledged           - the amount of money already pledged (promised to be paid) by backers (supporters of the project)
# staff_pick        - a campaign was designated by Kickstarter team members as a "favorite" while it was active (TRUE), or not (FALSE)
# backers_count     - number of backers (supporters) of a project
# name_len          - number of words in the name of the project
# name_len_clean    - number of words in the name of the project, after removing "stop words" - commonly used words
# blurb_len         - number of words in the description of the project
# blurb_len_clean   - number of words in the description of the project, after removing "stop words" - commonly used words
# Plus some time-related variables:
# deadline          - deadline given for successful funding
# state_changed_at  - time when the state of campaign changed to "success" or "failure"
# created_at        - time the project was created
# launched_at       - time the project was launched
# deadline_month          - deadline given for successful funding (months 1-12)
# deadline_day            - deadline given for successful funding (day of the month 1-31)
# deadline_yr             - deadline given for successful funding (years 2009-2017)
# deadline_hr             - deadline given for successful funding (hours 0-23)
# state_changed_at_month  - time when the state of campaign changed to "success" or "failure" (months 1-12)
# state_changed_at_day    - time when the state of campaign changed to "success" or "failure" (day of the month 1-31)
# state_changed_at_yr     - time when the state of campaign changed to "success" or "failure" (years 2009-2017)
# state_changed_at_hr     - time when the state of campaign changed to "success" or "failure" (hours 0-23)
# created_at_month        - time the project was created (months 1-12)
# created_at_day          - time the project was created (day of the month 1-31)
# created_at_yr           - time the project was created (years 2009-2017)
# created_at_hr           - time the project was created (hours 0-23)
# launched_at_month       - time the project was launched (months 1-12)
# launched_at_day         - time the project was launched (day of the month 1-31)
# launched_at_yr          - time the project was launched (years 2009-2017)
# launched_at_hr          - time the project was launched (hours 0-23)
################################################################
# The goal is to design a prediction model that will predict the success rate of Kickstarter campaigns
# Submit a working R (python) code - clear the workspace and test the code before submitting
################################################################

################################################################
# SCORING
# - 10 points with respect to data preparation (outliers, interactions, transformations, unsupervised learning,...)
# - 10 points for valid predictions & evaluation
# - 10 points for interpretation & presentation of results (tables or figures in R) and extra effort
################################################################

#Data Preperation
# Checking goal,pledged, blurblenClean, . I assume that these variables will be critical for the model

ps$goal
summary(ps$goal)
hist(ps$goal)
# goal is not well distrivuted so log is used to avoid any outlier present
ps$goal=log(ps$goal)
hist(ps$goal)
#As you can see, now it has more normal distribution
plot(ps$SuccessfulBool,ps$goal)#'dnt get much info'

summary(ps$pledged)
hist(log(ps$pledged))
# pledge is not well distrivuted so log could be used to avoid any outlier present, Howver, some of values are 0 and leads to NAF result. Hence, log is avoided
hist(log(ps$pledged))

summary(ps$blurb_len_clean)
hist((ps$blurb_len_clean))
#It has well distributed data

#Creating interestional variables
ps$blurb=ps$blurb_len*ps$blurb_len_clean
ps$name=ps$name_len*ps$name_len_clean
ps$month=ps$deadline_month*ps$created_at_month*ps$launched_at_month

#Creating train and test dataset
set.seed(100)
idx = sample(1:dim(ps)[1],size=floor(dim(ps)[1]*0.8),replace=F)
train = ps[+idx,]
test  = ps[-idx,]

#Check for data imbalance
idx.good = which(train$SuccessfulBool==0)
idx.bad  = which(train$SuccessfulBool==1)
length(idx.good)
length(idx.bad)

#Undersampling majority and oversampling minority (2 times the number of minority)
set.seed(300)
tmp.good = idx.good[sample(1:length(idx.good),size=length(idx.bad)*2,replace=F)]
tmp.bad  = idx.bad[sample(1:length(idx.bad),size=length(idx.bad)*2,replace=T)] # Oversampling
train.uo = rbind(train[tmp.good,],train[tmp.bad,])
#Just some randomization - we do not want defaults and non-deafults to be stacked one next to each other (just to be sure)
train.uo = train.uo[sample(1:dim(train.uo)[1],size=dim(train.uo)[1],replace=F),]
dim(train.uo)
summary(train.uo$SuccessfulBool)

#Using different Models

thr = 0.5
ref = as.factor(test$SuccessfulBool)
#LR
spec_one = as.formula(SuccessfulBool~goal+pledged+staff_pick+backers_count+deadline_month+deadline_day+
                        deadline_yr +deadline_hr+created_at_month+created_at_day+created_at_yr+
                        created_at_hr+launched_at_month+launched_at_day +launched_at_yr+launched_at_hr+
                        state_changed_at_month+state_changed_at_day+state_changed_at_yr+state_changed_at_hr+blurb+name+month)

p1 = predict(glm(spec_one,data=train,family='binomial'),new=test)
p1 = 1/(1+exp(-p1))
hist(p1, breaks = 50)
summary(p1)
plot(p1)

f1 = as.factor((p1>thr)*1)
confusionMatrix(f1,reference=ref,positive='1')
#Here we have overall accuracy of 0.77. The problem with this model is however, balanced accuracy(.6632) is low,
#i.e senssity is very low(.44) compared to the specificity(.9250). This means that this model is very bad at prediccting if 
# the venture is succesful, where as it is very good at predicting whether the project will be unsuccessful.


#Preparing data for Lasso
features = c('goal','pledged','staff_pick','backers_count','deadline_month','deadline_day','deadline_yr' ,'deadline_hr','created_at_month','created_at_day','created_at_yr','created_at_hr','launched_at_month','launched_at_day' ,'launched_at_yr','launched_at_hr','state_changed_at_month','state_changed_at_day','state_changed_at_yr','state_changed_at_hr','blurb','name','month')
x = as.matrix(train[,features])
head(x)
y = as.matrix(train[,'SuccessfulBool'])
head(y)
newx = as.matrix(test[,features])
head(newx)
#Lasso
lasso.Ri = cv.glmnet(x=x,y=y,type.measure='deviance',nfolds=10,family='binomial',alpha=1)
# Check coefficients
coefficients(lasso.Ri,s='lambda.min')
p2 = (1/(1+exp(-predict(lasso.Ri,newx=newx,s='lambda.min'))))
hist(p2, breaks = 50)
summary(p2)
plot(p2)

f2 = as.factor((p2>thr)*1);
confusionMatrix(f2,reference=ref,positive='1')

#Here we have overall accuracy of 0.80 which is better than of logistic regression. The problem with this model is however, balanced accuracy(.6961) is low compared to overall accuracy,
#i.e senssity is very low(.44) compared to the specificity(.94). This means that this model is very bad at prediccting if 
# the venture is succesful, where as it is very good at predicting whether the project will be unsuccessful.
# Apart from overall accuracy, the confussion matrix shows similar results as to LR.

#Decision Tree

dt = rpart(spec_one,data=train,
           method='class',model=TRUE,
           control=rpart.control(cp=0,xval=10))
rpart.plot(dt)
p3 = predict(dt,new=test)[,2]
summary(p3)
plot(p3)

f3 = as.factor((p3>thr)*1);
confusionMatrix(f3,reference=ref,positive='1')

# Here we have a great results. Decisopn tree has overall accuracy of 98% and has the similar balanced accuracy as well.
#This has more or less similar sensityvity and specifity score. Now, lets try bagging

bagg.pred = array(NA,dim=c(dim(test)[1],100))
# We loop over bootstrapped data
# This takes some time
for (b in 1:100) {
  print(b)
  # Randomly select observations
  idx = sample(1:dim(train)[1],size=dim(train)[1],replace=T)
  tb = rpart(spec_one,data=train[idx,],
             method='class',model=TRUE,
             control=rpart.control(cp=0,xval=10))
  # Generate and store predictions
  bagg.pred[,b] = predict(tb,new=test)[,2]
}
# Calculate the average prediction (probability)
p4 = apply(bagg.pred,1,mean)

f4 = as.factor((p4>thr)*1);
confusionMatrix(f4,reference=ref,positive='1')
#' Bagging performs slightly better than the decision tree'



#Lets try K means clustering
head(train)
zscore.train = scale(train[,c(-1,-10,-11,-12,-13,-10,-11,-12,-13,-14)])
head(zscore.train)
# We do the same with testing dataset!
zscore.test = scale(test[,c(-1,-10,-11,-12,-13,-10,-11,-12,-13,-14)],)
head(zscore.test)


##################################
# K-MEANS
#K-means clustering is a type of unsupervised machine learning algorithm used to classify data into groups, or clusters. It works by dividing a dataset into a specified number of clusters, and then assigning each data point to one of the clusters based on its similarity to other points in the cluster. This is done by first defining a distance measure (such as Euclidean distance), and then iteratively assigning points to the cluster with the closest mean, or center point.
#Standardize variables - Z-score standardization.
#Consider K = 3 with Euclidean distance.
##################################
# Given the distance matrix - Fin 7 clusters
K = 7


#Using llyods algorithm for K means clustering
#The basic idea behind Lloyd's algorithm is to start with a set of initial cluster centers, and then iteratively improve the solution by reassigning each data point to the cluster with the closest center, and then recalculating the cluster centers based on the new assignments. This process is repeated until the cluster assignments stop changing, at which point the algorithm has converged and the final clusters are returned.
res.lloyd <- kmeans(zscore.train,centers=K,iter.max=50,nstart=20,algorithm='Lloyd')
# Check number of observations in cluster
table(res.lloyd$cluster)
# Are clusters related to survival?
table(res.lloyd$cluster,train$SuccessfulBool)
# Based on table, it seems like cluster 6 has the loweest number of observation and at the same time
#it has all the obsevation as successful'

# At first sight it look nice
# Perhaps we can create new variables - where the given observation belongs?
lloyd = dummy(res.lloyd$cluster)
head(lloyd)
lloyd = lloyd[,-4] # We remove one cluster (K-1) (remember colinearity issues) and I will remove 4th because it has almost half obsevation in both 0 and 1 meaning unsuccessful and successful
colnames(lloyd) = paste('ll',c(1:3,5:7),sep='')
# Add to the training dataset as a new set of variables
train.ll = data.frame(train,lloyd)

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
testN = data.frame(test,km.test)

#Now using new dataset on decision tree that was one of the best performing model

dtn = rpart(spec_one,data=train.ll,
           method='class',model=TRUE,
           control=rpart.control(cp=0,xval=10))
rpart.plot(dtn)
p5 = predict(dt,new=testN)[,2]
summary(p5)
plot(p5)

f5 = as.factor((p5>thr)*1);
confusionMatrix(f5,reference=ref,positive='1')
#Here we have similar results to that of decission tree withouout cluster created variable. We already had a great
#result of DT; so using the cluster created variables does not show lots of effect. Lets try this on LR.

p6 = predict(glm(spec_one,data=train.ll,family='binomial'),new=testN)
p6 = 1/(1+exp(-p1))
hist(p6, breaks = 50)
summary(p6)
plot(p6)

f6 = as.factor((p6>thr)*1);
confusionMatrix(f6,reference=ref,positive='1')
#Using the new variables created by clustering did not improve result for logistic regression. This means
#that creating a cluster using K mean did not provide any information that could be helpful in creating better models.