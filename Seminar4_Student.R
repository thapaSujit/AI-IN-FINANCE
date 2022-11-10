###################################################################################
# Seminar No. 4
# 1) Load oct.csv (xlsx) and ocr.csv (xlsx)
# 2) Estimate models
# 2.1) OLS
# 2.2) Backward elim
# 2.3) Forward elim
# 2.4) Forecast evaluation
# 3) Visualize predicted and forecasted values
# 4) Estimate LASSO, RIDGE, EN
# 5) Evaluate models - LASSO, RIDGE, EN
###################################################################################
#1. 
getwd()
oct = read.csv(file='oct1.csv')
ocr = read.csv(file='ocr1.csv')
head(oct)
head(ocr)
#######################
# 2.1) OLS models
#######################
# OLS models
m1 = lm(price ~ km, data=ocr); p1 = predict(m1,new=oct);
m2 = lm(price ~ age, data=ocr); p2 = predict(m2,new=oct)
m3 = lm(price ~ km + age, data=ocr); p3 = predict(m3,new=oct)
# Here use your 'top' model from Seminar 3
m4 = lm(price ~ km + age + I(km*age) + I(eng1*age)+I(eng2*age)+
          I(eng3*age)+I(eng4*age) + I(power_low*age)+
          I(eng6*age)+I(ba*km)+I(bb*km)+I(ke*km) + I(nr*age)+
          I(po*km)+I(tn*km)+I(tt*km),data = ocr) 
p4 = predict(m4,new=oct)
# My 'top' model spec
m5 = lm(price ~ km + I(km^2) + age + I(age^2) +
          diesel + man + eng1 + eng2 + eng3 + eng4 +
          eng6, data = ocr)
p5 = predict(m5,new=oct)
#######################

#######################
# Backward elimination
#######################
install.packages("MASS")
full.model = lm(price ~km+age+km10+power_lowest+power_low+power_mid+power_high+
power_highest+ba+bb+ke+nr+po+tn+tt+petrol+petgas+man+eng1+eng2+eng3+eng4+eng6
, data = ocr)

m6 = step(full.model, direction = "backward", 
                      trace = FALSE)
p6 = predict(m6, new=oct)
m7 = step(full.model, direction = "forward", 
                   trace = FALSE)
p7 = predict(m7, new=oct)
summary(step.modelb)
summary(step.modelf)
# I need to create couple of FUNCTIONS (or you can use a function is a package)

# Generate a specification
# Inputs
# dep - name of the dependent variable
# x   - vector of characters -> names of features to be considered initially (all features)
# Output
# as.formula() object that goes into lm()
# We need the function in order to generate specifications
gen.fm = function(dep='price',x=features) {
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

# Pairs-Bootstrap p-values
# Inputs
# m - the lm() object
# spec - specification
# B - Size of the bootstrap sample
# We need this function in order to calculate significance of coefficients
pair.bt = function(model=m, spec, B=1000) {
  
  # We extract the data from the 'model' object
  dt = model$model
  # How many observations we have?
  N = dim(dt)[1]
  # How many features we have? Minus 1 is because of the dependent variable
  NV = dim(dt)[2]-1
  # We create a matrix where rows correspond to different bootstrap sample
  # Columns are coefficient estimates
  CFB = matrix(NA,nrow=B,ncol=NV+1) # +1 Intercept
  colnames(CFB) = c('Intercept',names(dt)[-1])
  
  for (b in 1:B) {
    
    # Randomly select from 1 to N, N numbers with replacement.
    idx.b = sample(1:N,size=N,replace=TRUE)
    # Now create the b^{th} boostrap sample
    dtb = dt[idx.b,]
    # Estimate model with given specification using the b^{th} bootstrap sample
    mb = lm(spec,data=dtb)
    # Extract coefficients from the model
    cfb = coefficients(mb)
    # Store the Intercept
    CFB[b,1] = cfb[1]
    # Store the remaining variables - beware they should be stored in the correct column
    # which() - Which names from cfb are in (%in%) columns of CFB
    CFB[b,which(names(cfb) %in% colnames(CFB))] = cfb[-1]
    
  }
  
  # Now we calculate the p-values for each variable that we return
  return(apply(CFB,2,function(x) min(c(sum(x>0,na.rm=T),sum(x<0,na.rm=T)))/length(x)))
}


# Backward Elimination
features = c('km','age','km10','power_lowest','power_low','power_mid','power_high',
             'power_highest','ba','bb','ke','nr','po','tn','tt','petrol',
             'petgas','man','eng1','eng2','eng3','eng4','eng6')
# Threshold - critical value
pt = 0.10

# We start by the initial full model
spec = gen.fm(dep='price',x=features)
m = lm(spec,data=ocr)
NV = length(features)
A = Sys.time()
pvals = pair.bt(model=m,spec,B=1000)
Sys.time()-A
# It takes around 7.3s on my laptop

# while(logical condition) is a loop
# It loops until the condition is met
# The condition says that there should be no p-value larger then the threshold pt
while(length(which(pvals[-1] > pt))>0) {
  # We need to find the variable that has the highest p-value which is also above the threshold
  rm.var = names(which(pvals == max(pvals[which(pvals[-1] > pt)+1])))
  # Now we want to remove it from the list of features
  features = features[-which(features==rm.var)]
  # Now we want to re-estimate the regression model without that variable
  # So we create the new specifiction with the updated features
  spec = gen.fm(dep='price',x=features)
  # And estimate the model
  m = lm(spec,data=ocr)
  # And the significances
  pvals = pair.bt(model=m,spec,B=1000)
  # We now check the condition - are there any coefficient that have a pvalue above the threshold pt?
}
# This while() can take a while

# The last model is our Backward Elimination model
m6 = m
# Let's take a look: coefficients + pvals
cbind(coefficients(m6),pvals)
p6 = predict(m6,new=oct)

###############################################
# HOME ASSIGNMENT
# Make a function that returns:
# - Backward elimination selected specification
# - Predictions from backward elimination

###############################################

#######################
# Forward elimination
#######################

# We start with the same initial set of features
features = c('km','age','km10','power_lowest','power_low','power_mid','power_high',
             'power_highest','ba','bb','ke','nr','po','tn','tt','petrol',
             'petgas','man','eng1','eng2','eng3','eng4','eng6')
# Number of features
NV = length(features)
# Threshold - critical value - same as before
pt = 0.10
# We will use a smaller bootstrap sample size (saves time)
B = 250
# We start by the baseline model with only a constant
spec = gen.fm(dep='price',x=NULL)
# Estimate model
m = lm(spec,data=oct)
# Here we will store variables that 'pass' the test
good.features = c()
# In forward elimination we need to start to add the 'best' variable at a time.
# We loop over all variables - initially (v=1) we have NV variables to consider. Finally, only one is left.
for (v in 1:NV) {
  # Remaining number of features
  NF = length(features)
  # Store pvalues
  mat.pvals = matrix(NA,nrow=NF,ncol=1)
  # We need to try to add one-at-a-time all remaining features
  for (f in 1:NF) {
    # Generate specification
    spec = gen.fm(dep='price',x=c(good.features,features[f]))
    # Estimate model
    m = lm(spec,data=ocr)
    # Find p-values
    pvals = pair.bt(model=m,spec,B=B)
    # Store p-values
    mat.pvals[f,1] = pvals[length(pvals)]
  }
  # Check if there is at least one pvalue lower as threshold - if not - we are done - Algo stops
  if (length(which(mat.pvals < pt))==0) break
  # Which variable has lowest p-value?
  add.var = features[(which(mat.pvals==min(mat.pvals)))][1]
  # We will add this variable to the set of good features
  good.features = c(good.features,add.var)
  # We will remove this variable from the set of remaining featurs
  features = features[-which(features==add.var)]
  # Now we repeat the procedure with remaining features until we:
  # - either have only features that have a p-value above threshold
  # - or we are with one left
}
# This takes a while as well! - Try it at home
##############

spec = gen.fm(dep='price',x=good.features)
m7 = lm(spec,data=ocr)
p7 = predict(m7,new=oct)
# Find p-values
pvals = pair.bt(model=m7,spec,B=B)
# Let's take a look: coefficients + pvals
cbind(coefficients(m7),pvals)

###############################################
# HOME ASSIGNMENT
# Make a function that returns:
# - Forward elimination selected specification
# - Predictions from forward elimination
###############################################

#########
# Predict
NT = dim(oct)[1]
# Store predictions
predictions = matrix(NA,nrow=NT,ncol=10+1)
colnames(predictions) = c('True',paste('p',1:5,sep=''),'BE','FE','LASSO','RIDGE','EN')
predictions[,1] = oct$price
# Check if there are negative values and if yes substitute (a function would be nice)
p1[p1 < 0] = min(p1[p1>0])
p2[p2 < 0] = min(p2[p2>0])
p3[p3 < 0] = min(p3[p3>0])
p4[p4 < 0] = min(p4[p4>0])
p5[p5 < 0] = min(p5[p5>0])
p6[p6 < 0] = min(p6[p6>0])
p7[p7 < 0] = min(p7[p7>0])
# Store predictions
predictions[,2:8] = cbind(p1,p2,p3,p4,p5,p6,p7)
head(predictions)
#########
# Evaluate
# Store loss function - MSE
mse = matrix(NA,nrow=NT,ncol=7) # 7 models
colnames(mse) = colnames(predictions)[2:8]
for (i in 1:7) mse[,i] = (predictions[,1] - predictions[,i+1])^2
apply(mse,2,mean)

#########
# MCS
#install.packages("MCS")
library(MCS)
MCSprocedure(mse)
#########

#########
# LASSO
install.packages("glmnet")
library(glmnet)
features = c('km','age','km10','power_lowest','power_low','power_mid','power_high',
                        'power_highest','ba','bb','ke','nr','po','tn','tt','petrol',
                        'petgas','man','eng1','eng2','eng3','eng4','eng6')
# The function we are going to use requires features to be in a matrix object
X = as.matrix(ocr[,features])
# Outcome variable to be a matrix object as well
Y = as.matrix(ocr[,'price'])
# We do the same for the testing dataset
XT = as.matrix(oct[,features])
# Now we need to estimate \lambda using cross-validation
# nfolds - is the k-cross-validation
# type.measure - is the loss function, you could provide your own
# alpha = 1 is LASSO, 0 - RIDGE, in-between EN
cvm = cv.glmnet(x=X,y=Y,type.measure='mse',nfolds=10,alpha=1)
??cv.glmnet
# Figure
plot(cvm)

# You can check out coefficients for lambda-min
coefficients(cvm,s='lambda.min')
# or if lambda is 1se away from lambda-min
coefficients(cvm,s='lambda.1se')

# Now let's predict under lambda.min
p8 = predict(cvm,newx = XT,s='lambda.min')
summary(p8)
p8[p8<0] = min(p8[p8>0])
predictions[,9] = p8

#########
# RIDGE
cvm = cv.glmnet(x=X,y=Y,type.measure='mse',nfolds=10,alpha=0)
# Figure
plot(cvm)
# You can check out coefficients for lambda-min
coefficients(cvm,s='lambda.min')
# or if lambda is 1se away from lambda-min
coefficients(cvm,s='lambda.1se')
# Now let's predict under lambda.min
p9 = predict(cvm,newx = XT,s='lambda.min')
summary(p9)
p9[p9<0] = min(p9[p9>0])
predictions[,10] = p9

#########
# ELASTIC NET
cvm = cv.glmnet(x=X,y=Y,type.measure='mse',nfolds=10,alpha=0.5)
# Figure
plot(cvm)
# You can check out coefficients for lambda-min
coefficients(cvm,s='lambda.min')
# or if lambda is 1se away from lambda-min
coefficients(cvm,s='lambda.1se')
# Now let's predict under lambda.min
p10 = predict(cvm,newx = XT,s='lambda.min')
summary(p10)
p10[p10<0] = min(p10[p10>0])
predictions[,11] = p10

###############################################
# HOME ASSIGNMENT - if you feel up-to-it
# Make a function that cross-validates different alpha values:
# - Selects the 'preferred' Elastic Net model
# - Returns the 'preferred' EN model's specifation
# - Returns predictions from the 'preferred' model
# - Both for lambda.min as well as for lambda.1se
###############################################

#########
# Evaluate
# Store loss function - MSE
mse = matrix(NA,nrow=NT,ncol=10) # 10 models
for (i in 1:10) mse[,i] = (predictions[,1] - predictions[,i+1])^2
apply(mse,2,mean)

#########
# MCS
library(MCS)
MCSprocedure(mse)
#########

plot.ts(x=predictions[,1],y=predictions[,5],pch=19,cex=0.5,col='red',
        xlab = 'true', ylab = 'model 4',ylim=c(0,35000),xlim=c(0,35000))
lines(x=c(0,40000),y=c(0,40000),col='black',lwd=1)

###############################################
# HOME ASSIGNMENT - think about it
# How would you exploit the fact that the model
# systematically underestimates expensive cars?
# Suggest a methodological approach to test your model.
###############################################
