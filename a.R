zs = read.csv(file='zopa_students (1).csv')[,-1]

# This is a dataset of a sample of consumer loans (P2P loans) from ZOPA. The variables are:
# date.start - the date when the loan originated (started)
# date.end   - the date when the loan ended
# default    - 1 if the borrower defaulted on loan and 0 otherwise
# return     - is the returna achieved on the loan by the lender in [% p.a.]
# aumount    - is the ln of the size of the loan
# term       - it's the time in months for the duration of the loan
# time       - it's the time in days since the first observation in the dataset
# rate       - it's the annualized interest rate on the loans
# ....       - calendar effects with 1 for the loan originated in that given monthand 0 otherwise
# pastRet    - the amount of p.a. return the lender achieved with the same customer.
# ....       - remaining variables are variables related to a specific region - DO NOT USE THOSE!

# The goal is to design a prediction model that will predict the returns of on the loan. Think about
# variables that you will use. You can also create your own variables. Your graded tasks are as follows:
# 1) Prepare a table with descriptive statistics and correlations for key variables (not necessarily all) that enter your model.
# 2) Propose at least 4 models to predict returns.
# 3) Evaluate models and suggest which approach to use in future. Explain your recommendation.
# Report and describe your results in MS WORD.
# Apart from fulfilling the goal, you will be evaluated based on following criteria:
# - if you submit a working code along your explanations.
# - if you present Figures as well.
# - if you select different types of models, but 4 is enough.
# - if you evaluate your models using an MCS test.
# - if you use multiple loss functions.
# - if you interpret your results correctly.
# - if you create your own variables and consider different transformations.

# Descriptive statistics
library(moments) # for skewness and kurtosis
vars = c('return','amount','term')
dscr.nms = c('mean','sd','skewness','kurtosis','min.','25%p.','median','75p.','max.')
dscr = matrix(NA,nrow=length(dscr.nms),ncol=length(vars))
colnames(dscr) = vars
rownames(dscr) = dscr.nms
for (k in 1:length(vars)) {
  x = zs[,vars[k]]
  dscr[,k] = round(c(mean(x),sd(x),skewness(x),kurtosis(x),quantile(x,p=c(0,0.25,0.5,0.75,1))),3)
}
dscr
hist(zs$return,breaks=50)
hist(zs$rate,breaks=50)
hist(zs$term,breaks=10)
hist(zs$amount,breaks=10)
plot(x=zs$rate,y=zs$return,pch=19,cex=0.25)
plot(x=zs$term,y=zs$return,pch=19,cex=0.25)
plot(x=zs$amount,y=zs$return,pch=19,cex=0.25)

# Data transformations
zs$rate2 = zs$rate^2
zs$lrate = log(zs$rate)
zs$time2 = zs$time^2
zs$ltime = log(zs$time)
zs$amt2 = (exp(zs$amount/1000)^2)
zs$amtrate = zs$amount * zs$rate

# Split the sample 80 to 20
set.seed(400)
idx      = sample(1:dim(zs)[1],size=floor(dim(zs)[1]*0.8),replace=F)
zs.train = zs[+idx,]
zs.testi = zs[-idx,]

# OLS, LASSO, RF, XBOOST
fm = as.formula(return~amount+amt2+rate+rate2+lrate+amtrate+time+time2+ltime+pastRet+
                  August+December+February+January+July+June+March+May+November+October+September)
m1 = lm(fm, data = zs.train)
summary(m1)
p1 = predict(m1,newdata=zs.testi)
plot(x=zs.testi$return,y=p1,pch=19,cex=0.5,col='red')

# LASSO
library(glmnet) # for lasso
?cv.glmnet
X  = as.matrix(zs.train[,all.vars(fm)[-1]]) #Except the dependent variable
Y  = as.matrix(zs.train[,all.vars(fm)[+1]]) #Only the dependent variable
XN = as.matrix(zs.testi[,all.vars(fm)[-1]]) #Except the dependent variable
lasso = cv.glmnet(x=X,y=Y)
plot(lasso)
coefficients(lasso,s='lambda.min')
p2 = predict(lasso,s='lambda.min',newx=XN)
plot(x=zs.testi$return,y=p2,pch=19,cex=0.5,col='red')
points(x=zs.testi$return,y=p1,pch=19,cex=0.25,col='blue')

# Random forest
library(ranger)
depth = c(0,2,4,6)
ND = length(depth)
B = c(500,1000,3000)
NB = length(B)
mtry = c(2,4)
NR = length(mtry)
cv = 10
NT = dim(zs.train)[1]
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
        zs.train.cvin = zs.train[-idx,]
        zs.train.cout = zs.train[+idx,]
        
        # Estimate the model
        rf.tree = ranger(fm,data=zs.train.cvin,
                         num.trees=num.trees,mtry=num.try,min.node.size=5,max.depth=num.depth)  
        pred.rf.cv = predict(rf.tree,data=zs.train.cout)$predictions
        rf.cv[b,m,d,r] = mean((pred.rf.cv - zs.train.cout$return)^2)
        
      }
      # Average
      rf.cv.ave[b,m,d] = mean(rf.cv[b,m,d,])
    }
  }
  print(paste('Number of trees',B[b]))
}

# Select optimum parameters, i.e. search for values that have a minimum loss value
which(rf.cv.ave == min(rf.cv.ave), arr.ind=TRUE)

rf.tree = ranger(fm,data=zs.train,num.trees=B[3],mtry=mtry[2],min.node.size=5,max.depth=depth[4])
summary(rf.tree)
p3 = predict(rf.tree,data=zs.testi)$predictions
points(x=zs.testi$return,y=p3,pch=19,cex=0.25,col='black')

##########
# Boosting
library(gbm)
depth = c(4,6)
ND = length(depth)
B = c(500,2000)
NB = length(B)
learning.rate = c(0.10,0.01,0.001)
NLR = length(learning.rate)
cv = 5
NT = dim(zs.train)[1]

# MSE for CV
rf.cv = array(NA,dim=c(NB,NLR,ND,cv))
dimnames(rf.cv)[[1]] = paste('Trees',B)
dimnames(rf.cv)[[2]] = paste('Learning rate',learning.rate)
dimnames(rf.cv)[[3]] = paste('Depth',depth)
dimnames(rf.cv)[[4]] = paste('CV sample',1:cv)

rf.cv.ave = array(NA,dim=c(NB,NLR,ND))
dimnames(rf.cv.ave)[[1]] = paste('Trees',B)
dimnames(rf.cv.ave)[[2]] = paste('Learning rate',learning.rate,'features')
dimnames(rf.cv.ave)[[3]] = paste('Depth',depth)

# Running the CV excercise
# Number of trees
for (b in 1:NB) {
  num.trees = B[b]
  # Number of mtry
  for (m in 1:NLR) {
    shrinkage = learning.rate[m]
    # Depth
    for (d in 1:ND) {
      num.depth = depth[d]
      # Now cross-validation      
      for (r in 1:cv) {
        # Select data
        idx = c(((r-1)*(NT/10)+1):(r*(NT/10)))
        zs.train.cvin = zs.train[-idx,]
        zs.train.cout = zs.train[+idx,]
        
        # Estimate the model
        rf.tree = gbm(fm,data=zs.train,distribution='gaussian',n.trees=num.trees,
                      interaction.depth=num.depth,shrinkage=shrinkage,bag.fraction=1)
        pred.rf.cv = predict(rf.tree,new=zs.train.cout)
        rf.cv[b,m,d,r] = mean((pred.rf.cv - zs.train.cout$return)^2)
        
      }
      # Average
      rf.cv.ave[b,m,d] = mean(rf.cv[b,m,d,])
    }
  }
  print(paste('Number of trees',B[b]))
}

# Select optimum parameters, i.e. search for values that have a minimum loss value
which(rf.cv.ave == min(rf.cv.ave), arr.ind=TRUE)

mod.gbm = gbm(fm,data=zs.train,distribution='gaussian',n.trees=2000,
              interaction.depth=4,shrinkage=0.01,bag.fraction=1)
p4 = predict(mod.gbm,new=zs.testi)
points(x=zs.testi$return,y=p4,pch=19,cex=0.50,col='orange')

###
# Evaluation
library(MCS) # Model confidence set
data = data.frame(true=zs.testi$return,ols=p1,lasso=as.numeric(p2),rf=p3,boost=p4)

# MSE & MAE
mse = matrix(NA,nrow=dim(data)[1],ncol=dim(data)[2]-1)
colnames(mse) = names(data)[-1] #Except true
mae = mse

for (i in 2:dim(data)[2]) {
  mse[,i-1] =    (data[,1]-data[,i])^2
  mae[,i-1] = abs(data[,1]-data[,i])
}

apply(mse,2,mean,na.rm=T)*1000
MCSprocedure(mse,alpha=0.15,B=5000)

apply(mae,2,mean,na.rm=T)
MCSprocedure(mae,alpha=0.15,B=5000)

# OLS works just fine - why? Because rate is such a good predictor.

