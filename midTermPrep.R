
# Not very accurate...
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
# - if you create your own variables and consider different transformations.#Reading the file
###############################################################################
ldata=read.csv(file='zopa_students (1).csv')
head(ldata)
ldata=ldata[,c(4:22)]
head(ldata)
library(moments)
colN=c('return', 'amount','term', 'time','rate')
rowN=c('mean','sd','kurtosis','skewness','min','25p','md','75p','max')
desStat = matrix(NA, nrow=length(rowN), ncol=length(colN))
colnames(desStat)=colN
rownames(desStat)=rowN
for (i in 1:length(colN)) {
  x = ldata[,colN[i]]
  desStat[,i]=round(c(mean(x),sd(x),kurtosis(x),skewness(x),quantile(x,p=c(0,0.25,0.5,0.75,1))),3)
}
desStat
mean(ldata$return)
############am-term, am-rate,ter.time,time-rate
x=ldata$amount
for (i in 2:length(colN)) {
  ?cor.test
  m=cor.test(x,ldata[,colN[i]],method='kendall')
  print(m)
}
x=ldata$term
for (i in 2:length(colN)) {
  ?cor.test
  m=cor.test(x,ldata[,colN[i]],method='kendall')
  print(m)
}
x=ldata$time
for (i in 2:length(colN)) {
  ?cor.test
  m=cor.test(x,ldata[,colN[i]],method='kendall')
  print(m)
}
x=ldata$rate
for (i in 2:length(colN)) {
  ?cor.test
  m=cor.test(x,ldata[,colN[i]],method='kendall')
  print(m)
}

hist(ldata$return)
hist(ldata$amount, breaks=30)
hist(ldata$term, breaks=30)
hist(ldata$time, breaks=30)
hist(ldata$rate, breaks=30)
plot(ldata$amount,ldata$return)
plot(ldata$term,ldata$return)
plot(ldata$time,ldata$return)
plot(ldata$rate,ldata$return)
#Creating new term based on core am-term, am-rate,ter.time,time-rate
ldata$amTerm=ldata$amount*ldata$term
ldata$amRate=ldata$amount*ldata$rate
ldata$tiTerm=ldata$time*ldata$term
ldata$timeRat=ldata$time*ldata$rate
ldata$rateTerm=ldata$rate*ldata$term
ldata$rateTime=ldata$rate*ldata$time
#ldata$rateAm=ldata$rate*ldata$amount
ldata$rateLn=log(ldata$rate)
ldata$timeYear=ldata$time/365

dim(ldata)
sample()
sam=sample(dim(ldata)[1],floor(dim(ldata)[1]*.80),replace=FALSE)
train=ldata[sam,]
test=ldata[-sam,]
head(ldata)
m1=lm(return~amount+term+time+rate+August+December+February+January+July+June+March+May
      +November+October+September+pastDef+pastRet+amTerm+amRate+tiTerm+timeRat
      +rateTerm+rateTime+rateLn+timeYear, data = train)
summary(m1)
pr1 = predict(m1, newdata = test)
plot(pr1,test$return,pch=19,cex=0.5,col='red')

colN=c('true','ols','dt','rf','boost')
predictRes=matrix(NA,nrow=dim(test)[1],ncol = length(colN))
colnames(predictRes)=colN
head(predictRes)
predictRes[,1]=test$return
predictRes[,2]=pr1

colN=c('ols','dt','rf','boost')
mseresult=matrix(NA,nrow=dim(test)[1],ncol = length(colN))
colnames(mseresult)=colN
mseresult[,1]=(predictRes[,1]-predictRes[,2])^2
head(mseresult)
apply(mseresult,2,mean)*1000
colN=c('ols','dt','rf','boost')
maeresult=matrix(NA,nrow=dim(test)[1],ncol = length(colN))
colnames(maeresult)=colN
maeresult[,1]=abs(predictRes[,1]-predictRes[,2])
head(maeresult)
apply(maeresult,2,mean)*1000

library(rpart)
library(rpart.plot)
#Decision trees are nice to interpret, but tend to over-fit the data and do
#not perform very well in an out-of-sample context
?rpart
m2=rpart(return~amount+term+time+rate+August+December+February+January+July+June+March+May
         +November+October+September+pastDef+pastRet+amTerm+amRate+tiTerm+timeRat
         +rateTerm+rateTime+rateLn+timeYear, data = train,method='anova', model=TRUE,
         control=rpart.control(cp = 0.01, maxdepth = 9))
summary(m2)
pr2 = predict(m2, newdata = test)
predictRes[,3]=pr2

mseresult[,2]=(predictRes[,1]-predictRes[,3])^2
head(mseresult)
maeresult[,2]=abs(predictRes[,1]-predictRes[,3])
head(maeresult)
apply(mseresult,2,mean)*1000
apply(maeresult,2,mean)*1000

library(ranger)
#An issue with bagging:
#With trees, bagging leads to (positively) correlated trees, i.e.
#predictions are similar, because trees are similar.
#Same set of important predictors is chosen to split the trees.
#Averaging works best for uncorrelated predictions (recall the
                                                   portfolio theory...).
#Random forest uses bagging as well, but with a twist.
#At each split we are not searching from all features the one that
#gives us the best split, instead random set of m features is used
?ranger
depth = c(0,2,4,6)
ND = length(depth)
B = c(500,1000,3000)
NB = length(B)
mtry = c(2,4)
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
        rf.tree = ranger(return~amount+term+time+rate+August+December+February+January+July+June+March+May
                         +November+October+September+pastDef+pastRet+amTerm+amRate+tiTerm+timeRat
                         +rateTerm+rateTime+rateLn+timeYear,data=zs.train.cvin,
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

# Number of trees
m3=ranger(return~amount+term+time+rate+August+December+February+January+July+June+March+May
          +November+October+September+pastDef+pastRet+amTerm+amRate+tiTerm+timeRat
          +rateTerm+rateTime+rateLn+timeYear, data = train, mtry=4,num.trees = 500, max.depth = 6,
          min.node.size = 5)
pr3 = predict(m3,data = test)$prediction
predictRes[,4]=pr3

mseresult[,3]=(predictRes[,1]-predictRes[,4])^2
head(mseresult)
maeresult[,3]=abs(predictRes[,1]-predictRes[,4])
head(maeresult)
apply(mseresult,2,mean)*1000
apply(maeresult,2,mean)*1000

library(gbm)
#In boosting, trees are created sequentially. We are using
#information from the previous tree to improve our prediction in
#the next one.
??gbm
depth = c(4,6)
ND = length(depth)
B = c(500,2000)
NB = length(B)
learning.rate = c(0.10,0.01,0.001)
NLR = length(learning.rate)
cv = 10
NT = dim(train)[1]

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
        zs.train.cvin = train[-idx,]
        zs.train.cout = train[+idx,]
        
        # Estimate the model
        rf.tree = gbm(return~amount+term+time+rate+August+December+February+January+July+June+March+May
                      +November+October+September+pastDef+pastRet+amTerm+amRate+tiTerm+timeRat
                      +rateTerm+rateTime+rateLn+timeYear,data=zs.train.cvin,distribution='gaussian',n.trees=num.trees,
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

rf.cv.ave
m4=gbm(return~amount+term+time+rate+August+December+February+January+July+June+March+May
       +November+October+September+pastRet+amTerm+amRate+tiTerm+timeRat
       +rateTerm+rateTime+rateLn+timeYear, data = train, distribution='gaussian',
       shrinkage=0.01,interaction.depth = 6, n.trees=500, bag.fraction =1)
pr4 = predict(m4,newdata = test)
head(predictRes)
predictRes[,5]=pr4

mseresult[,4]=(predictRes[,1]-predictRes[,5])^2
head(mseresult)
maeresult[,4]=abs(predictRes[,1]-predictRes[,5])
head(maeresult)
apply(mseresult,2,mean)*1000
apply(maeresult,2,mean)*1000

library(MCS)
MCSprocedure(maeresult)
MCSprocedure(mseresult)

###############################################################################





















lData = read.csv(file='zopa_students (1).csv')
head(lData)

#Considering only important features
lData= lData[,c(4:22)]
head(lData)

#Calculating descriptive stat.
#install.packages('moments')
library(moments)
desStat = matrix(NA, nrow =9, ncol=5)
coName = c('return', 'amount', 'term', 'time', 'rate')
rowName = c('mean', 'sd', 'kurto', 'skew', 'min', '25P', 'md', '75P', 'max')
colnames(desStat) = coName
rownames(desStat) = rowName
desStat

for (i in 1:length(coName)) {
  x = lData[,coName[i]]
  desStat[,i] = round(c(mean(x), sd(x), kurtosis(x), skewness(x), quantile(x, p=c(0,.25,.50,.75,1))),3)
}
desStat

#hist and plot the important features
hist(lData$amount,breaks=30)
hist(lData$term,breaks=30)
hist(lData$time,breaks=30)
hist(lData$rate,breaks = 30)
hist(lData$return,breaks=30)
plot(lData$amount,lData$return)
plot(lData$term,lData$return)
plot(lData$time,lData$return)
plot(lData$rate,lData$return)

#Calculating correlation
#amount
amountCor=matrix(NA, nrow=2, ncol=4)
coName = c( 'amount', 'term', 'time', 'rate')
colnames(amountCor) = coName
x = lData$amount
for (i in 1:4) {
  m = cor.test(lData[,coName[i]],x, method = 'kendall')
  #print(m)
  amountCor[1,i]= round(m$estimate,3)
  amountCor[2,i]=round(m$p.value,3)
}
amountCor

#term
termCor=matrix(NA, nrow=2, ncol=4)
coName = c( 'amount', 'term', 'time', 'rate')
colnames(termCor) = coName
x = lData$term
for (i in 1:4) {
  m = cor.test(lData[,coName[i]],x, method = 'kendall')
  termCor[1,i]= round(m$estimate,3)
  termCor[2,i]=round(m$p.value,3)
}
termCor

#time
timeCor=matrix(NA, nrow=2, ncol=4)
coName = c( 'amount', 'term', 'time', 'rate')
colnames(timeCor) = coName
x = lData$time
for (i in 1:4) {
  m = cor.test(lData[,coName[i]],x, method = 'kendall')
  timeCor[1,i]= round(m$estimate,3)
  timeCor[2,i]=round(m$p.value,3)
}
timeCor

#rate
rateCor=matrix(NA, nrow=2, ncol=4)
coName = c( 'amount', 'term', 'time', 'rate')
colnames(rateCor) = coName
x = lData$rate
for (i in 1:4) {
  m = cor.test(lData[,coName[i]],x, method = 'kendall')
  rateCor[1,i]= round(m$estimate,3)
  rateCor[2,i]=round(m$p.value,3)
}
rateCor

#creating new features and modifying existing features
lData$amountTerm = lData$amount*log(lData$term)
lData$amountRate = lData$amount*lData$rate
lData$rateSq = lData$rate*lData$rate
lData$rateLn = log(lData$rate)
lData$timeTerm = lData$time*lData$term
plot(lData$amountTerm,lData$return)
plot(lData$amountRate,lData$return)
plot(lData$rateSq,lData$return)
plot(lData$rateLn,lData$return)
plot(lData$timeTerm,lData$return)

#Splitting into train and test data
dim(lData)
sample()
sam = sample(dim(lData)[1], floor(dim(lData)[1]*0.80),replace = F)
train = lData[sam,]
test = lData[-sam,]

#OLS MODEL and Prediction
features = c('amount','amountTerm','amountRate','rate','rateLn','rateSq','time','timeTerm','term','pastRet',
             'August','December','February','January','July','June','March','May','November','October','September')
m1=lm(return~amount+ amountTerm+ amountRate+ rate+ rateLn+ rateSq+ time+ timeTerm+ term+ pastRet+ August+
      December+ February+ January+ July+ June+ March+ May+ November+ October+ September, data = train)
pr1 = predict(m1, newdata = test)

predictResult = matrix(NA,nrow = dim(test)[1], ncol = 6)
colnames(predictResult) = c('true', 'ols', 'lasso', 'ridge', 'dt', 'boosting')
head(predictResult)
predictResult[, 1] = test$return
predictResult[, 2] = pr1
plot(test$return,pr1)

#Evaluation
mseResult = matrix(NA,nrow = 1000, ncol = 5)
colnames(mseResult) = c('ols', 'lasso', 'ridge', 'dt', 'boosting')
mseResult[,1] = (predictResult[,1] - predictResult[,2])^2
head(mseResult)
apply(mseResult,2,mean)*10000

maeResult = matrix(NA,nrow = 1000, ncol = 5)
colnames(maeResult) = c('ols', 'lasso', 'ridge', 'dt', 'boosting')
maeResult[,1] = abs(predictResult[,1] - predictResult[,2])
head(maeResult)
apply(maeResult,2,mean)*10000

#DT
library(rpart)
library(rpart.plot)
library(tree)
?rpart
rpart()
m2 = rpart(return~amount+ amountTerm+ amountRate+ rate+ rateLn+ rateSq+ time+ timeTerm+ term+ pastRet+ August+
             December+ February+ January+ July+ June+ March+ May+ November+ October+ September, data = train,
           method = 'anova', model = TRUE, control = rpart.control(cp = 0, maxdepth = 9))
p2 = predict(m2, newdata = test)
head(predictResult)
predictResult[,5] = p2

#Evaluating DT
head(mseResult)
mseResult[,4] = (predictResult[,1]-predictResult[,5])^2
apply(mseResult, 2, mean)*1000

head(maeResult)
maeResult[,4] = abs(predictResult[,1]-predictResult[,5])
apply(maeResult, 2, mean)*1000

#boosting
install.packages("gbm")
library(gbm)
??gbm
m3 = gbm(return~amount+ amountTerm+ amountRate+ rate+ rateLn+ rateSq+ time+ timeTerm+ term+ pastRet+ August+
           December+ February+ January+ July+ June+ March+ May+ November+ October+ September, data = train
         , distribution = "gaussian", n.trees=1000, interaction.depth = 4, shrinkage = 0.01, bag.fraction = 1 )

p3 = predict(m3,newdata = test)
head(predictResult)
predictResult[,6] = p3

#Evaluating Boosting
head(mseResult)
mseResult[,5] = (predictResult[,1]-predictResult[,6])^2
apply(mseResult, 2, mean)*1000

head(maeResult)
maeResult[,5] = abs(predictResult[,1]-predictResult[,6])
apply(maeResult, 2, mean)*1000

#Random Forest
install.packages("ranger")
library(ranger)
install.packages('Rcpp')
library(Rcpp)

m4=ranger(return~amount+ amountTerm+ amountRate+ rate+ rateLn+ rateSq+ time+ timeTerm+ term+ pastRet+ August+
            December+ February+ January+ July+ June+ March+ May+ November+ October+ September, data = train, num.trees = 1000,
          max.depth = 4,  min.node.size = 5)

p4 = predict(m4,data = test)$predictions
head(predictResult)
predictResult[,3] = p4

#Evaluating rf
head(mseResult)
mseResult[,2] = (predictResult[,1]-predictResult[,3])^2
apply(mseResult, 2, mean)*1000

head(maeResult)
maeResult[,2] = abs(predictResult[,1]-predictResult[,3])
apply(maeResult, 2, mean)*1000


library(MCS)
head(maeResult)
MCSprocedure(mseResult[,c(1,2,4,5)])
MCSprocedure(maeResult[,c(1,2,4,5)])





