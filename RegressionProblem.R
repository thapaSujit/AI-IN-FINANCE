

################################################################
# Households data - regression problem
households = read.csv(file='HouseholdsExamI.csv')
head(households)

#Looking for NA rows in dataset
any(is.na(households$dis_income))
apply(households, 2, function(x) any(is.na(x)))
#There are empty rows in dis-income which is our dependent variable, so will remove them
households=households[!is.na(households$dis_income),]
################################################################

################################################################
# This is a dataset from a survey of monthly household incomes and expenditures.
# The survey was conducted in years 2012 and 2015 in Slovakia.
################################################################
# The variables are as follows:
# year	           Year in which the survey was conducted
# dis_income       Discretionary income (Nominal household income minus expenditures on necessities/consumption)
# income	         Nominal household income
# consumption	     Expenditures on necessities
# dis_income_p	   Discretionary income as a % of income
# Jan	             Monthly dummy, 1 - yes for given month
# Feb	             Monthly dummy, 1 - yes for given month
# Mar	             Monthly dummy, 1 - yes for given month
# Apr	             Monthly dummy, 1 - yes for given month
# Maj	             Monthly dummy, 1 - yes for given month
# Jun	             Monthly dummy, 1 - yes for given month
# Jul              Monthly dummy, 1 - yes for given month
# Aug	             Monthly dummy, 1 - yes for given month
# Sep	             Monthly dummy, 1 - yes for given month
# Okt	             Monthly dummy, 1 - yes for given month
# Nov	             Monthly dummy, 1 - yes for given month
# Dec	             Monthly dummy, 1 - yes for given month
# BA	             region 1
# TT	             region 2
# TN	             region 3
# NR	             region 4
# ZA	             region 5
# BB	             region 6
# PO	             region 7
# KE	             region 8
# PC1	             Number of household members, 1
# PC2	             Number of household members, 2
# PC3	             Number of household members, 3
# PC4	             Number of household members, 4
# PC5	             Number of household members, 5 or more
# PP1	             Number of working household members, 1
# PP2	             Number of working household members, 2
# PP3	             Number of working household members, 3
# PP4	             Number of working household members, 4 or more
# D0	             Number of dependent (non-working) children, 0
# D1	             Number of dependent (non-working) children, 1
# D2	             Number of dependent (non-working) children, 2
# D3	             Number of dependent (non-working) children, 3
# D4	             Number of dependent (non-working) children, 4 or more
# N0	             Number of unemployed household members, 0
# N1	             Number of unemployed household members, 1
# N2	             Number of unemployed household members, 2
# N3	             Number of unemployed household members, 3 or more
# jed	             Is there a retiree in the household?
# PPD0	           Number of working retirees in the household, 0
# PPD1	           Number of working retirees in the household, 1
# PPD2	           Number of working retirees in the household, 2 or more
# age	             Age of the household representative
# male	           Gender of the household representative, 1 - male
# Middle	         Highest education status - middle
# University	     Highest education status - university degree
# ver	             Is the household representative working in the public sector, 1 - yes
# full	           Is the household representative working full-time, 1 - yes
################################################################
################################################################
# - Training dataset consists of all observations in 2012
# - Testing dataset consists of all observations in 2015
# - Submit a working R (python) code - clear the workspace and test the code before submitting
################################################################

################################################################
# The goal is to design a prediction model that will be used to predict
# discretionary income of households in 2015.
# As features you can use everything, except 'consumption' and 'dis_income_p' variables.
################################################################

################################################################
# SCORING
# - 10 points with respect to data preparation (outliers, interactions, transformations, unsupervised learning,...)
# - 10 points for valid predictions & evaluation
# - 10 points for interpretation & presentation of results (tables or figures in R) and extra effort
################################################################
#Data Preperation
# Checking if age and income are highly correlated
colN=c('income', 'age')
x=households$income
for (i in 2:length(colN)) {
  m=cor.test(x,households[,colN[i]],method='kendall')
  print(m)
}
# It does not seem to have high correlation.

#Looking at some of the continuous variables
#Income
households$income
summary(households$income)
hist(households$income)
# It seems like the income is skewed. There may be some outliers, so using log on this feature might lead to better result and could reduce outlier to certain extent
households$income=log(households$income)
hist(households$income)
# We have sligly better distribution

#Age
households$age
summary(households$age)
hist(households$age)
#LETS SEE HOWW THE DISTRIBUTION WOULD BE IF USED LOG
hist(log(households$age))
#I would say better so will use log on age as well. Max age is 96 which may be normal. But, using log can ensure that even if its outlier, log can help to reduce to ceertain extent
households$age=log(households$age)
hist(households$age)


plot(households$dis_income,households$income)
plot(households$dis_income,households$age)
#It does not seem there is a very strong correlation.

library(moments)
rowN=c('mean','sd','kurtosis','skewness','min','25p','md','75p','max')
desStat = matrix(NA, nrow=length(rowN), ncol=length(colN))
colnames(desStat)=colN
rownames(desStat)=rowN
for (i in 1:length(colN)) {
  x = households[,colN[i]]
  desStat[,i]=round(c(mean(x),sd(x),kurtosis(x),skewness(x),quantile(x,p=c(0,0.25,0.5,0.75,1))),3)
}
desStat
# After using the log, we have better decriptive sstatistics on continuous variables.
#Lets create some intersectional variables between dummy variablers retiree and number of working retirees
households$jedp0=households$jed*households$PPD0
households$jedp1=households$jed*households$PPD1
households$jedp2=households$jed*households$PPD2

# Lets use some regression model.
  
# creating train dataset that has year 2012
train = households[households$year==2012,]
head(train)
# creating test dataset that has year 2015
test = households[households$year==2015,]
head(test)


##Estimating OLS Model
spec_one = as.formula(dis_income~income+Jan+Feb+Mar+Apr+Maj+Jun+Jul+Aug+Sep+Okt+Nov+Dec+BA+TT+TN
                      +NR+ZA+BB+PO+KE+PC1+PC2+PC3+PC4+PC5+PP1+PP2+PP3+PP4+D0+D1+D2+D3+D4+N0+N1+
                        N2+N3+age+male+Stredne+Vysoke+ver+full+jedp0+jedp1+jedp2)
m1=lm(spec_one, data = train)
head(train)
summary(m1)
pr1 = predict(m1, newdata = test)
plot(pr1,test$dis_income,pch=19,cex=0.5,col='red')

#Creating matrix for storing results
colN=c('true','ols','dt','lasso', 'ridge')
predictRes=matrix(NA,nrow=dim(test)[1],ncol = length(colN))
colnames(predictRes)=colN
head(predictRes)
predictRes[,1]=test$dis_income
predictRes[,2]=pr1

colN=c('ols','dt','lasso', 'ridge')
mseresult=matrix(NA,nrow=dim(test)[1],ncol = length(colN))
colnames(mseresult)=colN
mseresult[,1]=(predictRes[,1]-predictRes[,2])^2
head(mseresult)
apply(mseresult,2,mean)

colN=c('ols','dt','lasso', 'ridge')
maeresult=matrix(NA,nrow=dim(test)[1],ncol = length(colN))
colnames(maeresult)=colN
maeresult[,1]=abs(predictRes[,1]-predictRes[,2])
head(maeresult)
apply(maeresult,2,mean)

#Using Decision Tree
library(rpart)
library(rpart.plot)
m2=rpart(spec_one,data = train,method='anova', model=TRUE,
         control=rpart.control(cp = 0.01, maxdepth = 9))
summary(m2)
pr2 = predict(m2, newdata = test)
plot(pr2,test$dis_income,pch=19,cex=0.5,col='red')
predictRes[,3]=pr2
head(predictRes)

mseresult[,2]=(predictRes[,1]-predictRes[,3])^2
head(mseresult)
maeresult[,2]=abs(predictRes[,1]-predictRes[,3])
head(maeresult)
apply(mseresult,2,mean)
apply(maeresult,2,mean)

library(glmnet)
#Using lasso  as it ivolves feature selection as well and we have alot of features
features = c('income','Jan','Feb','Mar','Apr','Maj','Jun','Jul','Aug','Sep','Okt','Nov','Dec','BA','TT','TN','NR','ZA','BB','PO','KE','PC1','PC2','PC3','PC4','PC5','PP1','PP2','PP3','PP4','D0','D1','D2','D3','D4','N0','N1','N2','N3','age','male','Stredne','Vysoke','ver','full','jedp0','jedp1','jedp2')

X = as.matrix(train[,features])
Y = as.matrix(train[,'dis_income'])
XT = as.matrix(test[,features])
cvm = cv.glmnet(x=X,y=Y,type.measure='mse',nfolds=10,alpha=1)
p3 = predict(cvm,newx = XT,s='lambda.min')
plot(p3,test$dis_income,pch=19,cex=0.5,col='red')
predictRes[,4]=p3
head(predictRes)

mseresult[,3]=(predictRes[,1]-predictRes[,4])^2
head(mseresult)
maeresult[,3]=abs(predictRes[,1]-predictRes[,4])
head(maeresult)
apply(mseresult,2,mean)
apply(maeresult,2,mean)

#Using Ridge
cvm = cv.glmnet(x=X,y=Y,type.measure='mse',nfolds=10,alpha=0)
# Figure
plot(cvm)
p4 = predict(cvm,newx = XT,s='lambda.min')
predictRes[,5]=p4
plot(p4,test$dis_income,pch=19,cex=0.5,col='red')
head(predictRes)

mseresult[,4]=(predictRes[,1]-predictRes[,5])^2
head(mseresult)
maeresult[,4]=abs(predictRes[,1]-predictRes[,5])
head(maeresult)
apply(mseresult,2,mean)
apply(maeresult,2,mean)

library(MCS)
MCSprocedure(maeresult)
MCSprocedure(mseresult)
# Based on the overall evaluation, Interms of MSE result, decision tree performs the best. However, one problem
#with decision tree is overfitting. Here, it would be better if we could try both bagging and random forest.
#I didd try that, however, I had error in code and unfortunately because of time constraint, was not able to correct it.
#'based on the MAE results, here also decision tree outperforms others'
#'
#'Using MCSPROCEDURE; decision tree outperforms other moddel. Here, again recommendattion is to try bagging
#or RF, however as mentioned earlier, due to code errorr could not do that.















