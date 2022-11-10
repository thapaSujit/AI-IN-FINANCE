###################################################################################
# Seminar No. 5
# 1) Import oct.csv (xlsx) and ocr.csv (xlsx) and pred.csv
# 2) Simple decision tree
# 2.1) Estimate
# 2.2) Visualize
# 2.3) Predict
# 2.4) Evaluate evaluation
# 3) More variables - shallow tree
# 4) More variables - deep tree
# 5) More variables - deep tree -> pre-pruning
# 6) More variables - deep tree -> penalization
# 7) Bagging
###################################################################################
ocr = read.csv(file='ocr_kmnxgvdi.csv')
oct = read.csv(file='oct_ophwppqk.csv')
pred = read.csv(file = 'pred.csv')

# 2) Decision tree
install.packages("tree")
install.packages("rpart")
install.packages("rpart.plot")
library(tree)
library(rpart)
library(rpart.plot)
# 2.1) Estimate
t1 = rpart(price~km+man,data=ocr,method='anova',model=TRUE,
           control=rpart.control(cp=0,maxdepth=3))
# 2.2) Visualize 
rpart.plot(t1,type=0)
rpart.plot(t1,type=1)
# 2.3) Predict
pred = cbind(pred, predict(t1,new=oct))
colnames(pred)[dim(pred)[2]] = 'DC1'
head(pred)
# 2.4) Evaluate
apply((pred[,-1] - pred[,1])^2,2,mean)
# Or simply
apply(pred[,-1],2,function(x,y) mean((x-y)^2), y=pred[,1])
apply(pred[,-1],2,function(x,y) mean(abs(x-y)), y=pred[,1])
# Not very accurate...

######################
# HOME ASSIGNMENT
######################
# Create a figure - similar to that on slide 9 of the Lecture 5 (Segmentation of the feature space)
######################

# 3) Let's try a tree with more variables - make it a shallow tree
t2 = rpart(price ~ km + km10 + age + power_lowest + power_low + 
             power_mid + power_high + power_highest + ba + bb + 
             ke + nr + po + tn + tt + diesel + petgas + man + 
             eng1 + eng2 + eng3 + eng4 + eng6, 
           data=ocr,method='anova',model=TRUE,
           control=rpart.control(cp=0,maxdepth=3))
rpart.plot(t2,type=0)
pred = cbind(pred, predict(t2,new=oct))
colnames(pred)[dim(pred)[2]] = 'DC2'
apply(pred[,-1],2,function(x,y) mean((x-y)^2), y=pred[,1])
apply(pred[,-1],2,function(x,y) mean(abs(x-y)), y=pred[,1])
# Better but still not great

# 4) Let's try a tree with more variables - make it a deep tree
t3 = rpart(price ~ km + km10 + age + power_lowest + power_low + 
             power_mid + power_high + power_highest + ba + bb + 
             ke + nr + po + tn + tt + diesel + petgas + man + 
             eng1 + eng2 + eng3 + eng4 + eng6, 
           data=ocr,method='anova',model=TRUE,
           control=rpart.control(cp=0,maxdepth=9))
# Visualization get's a little bit tricky - it's not necessary
rpart.plot(t3,type=0)
pred = cbind(pred, predict(t3,new=oct))
colnames(pred)[dim(pred)[2]] = 'DC3'
apply(pred[,-1],2,function(x,y) mean((x-y)^2), y=pred[,1])
apply(pred[,-1],2,function(x,y) mean(abs(x-y)), y=pred[,1])
# This is a break-through!

# 5) Can we improve via pre-pruning?
?rpart.control
t4 = rpart(price ~ km + km10 + age + power_lowest + power_low + 
             power_mid + power_high + power_highest + ba + bb + 
             ke + nr + po + tn + tt + diesel + petgas + man + 
             eng1 + eng2 + eng3 + eng4 + eng6, 
            data=ocr,method='anova',model=TRUE,
            control=rpart.control(cp=0,minsplit=25,minbucket=12,maxdepth=9))
pred = cbind(pred, predict(t4,new=oct))
colnames(pred)[dim(pred)[2]] = 'DC4'
apply(pred[,-1],2,function(x,y) mean((x-y)^2), y=pred[,1])
apply(pred[,-1],2,function(x,y) mean(abs(x-y)), y=pred[,1])

# 6) Can we improve via penalization?

# Which is the optimal shrinkage parameter
cv.tbl = printcp(t4)
head(cv.tbl)
opt.cp = cv.tbl[which(cv.tbl[,4] == min(cv.tbl[,4])),1]
t5 = rpart(price ~ km + km10 + age + power_lowest + power_low + 
             power_mid + power_high + power_highest + ba + bb + 
             ke + nr + po + tn + tt + diesel + petgas + man + 
             eng1 + eng2 + eng3 + eng4 + eng6, 
           data=ocr,method='anova',model=TRUE,
           control=rpart.control(cp=opt.cp,minsplit=c(25),minbucket=12,maxdepth=9))
rpart.plot(t5, type = 0)
pred = cbind(pred, predict(t5,new=oct))
colnames(pred)[dim(pred)[2]] = 'DC5'
apply(pred[,-1],2,function(x,y) mean((x-y)^2), y=pred[,1])
apply(pred[,-1],2,function(x,y) mean(abs(x-y)), y=pred[,1])

# 7) Bagging
# Recall - it involves bootstrapping!
# Number of bootstrap samples
B = 1000
# Number of observations
NT = dim(ocr)[1]
# I need a place (matrix/dataset) to store prediction
bag.fore = matrix(NA,nrow=dim(oct)[1],ncol=B)
rownames(bag.fore) = rownames(oct)
for (b in 1:B) {
  if (b %in% seq(0,B,100)) print(b)
  # Randomly select (with replacement) some row numbers
  idx = sample(1:NT,NT,replace=T)
  # Create the bootstrap sample
  auta.train.b = ocr[idx ,]
  # Estimate the model
  bt = rpart(price ~ km + km10 + age + power_lowest + power_low + 
               power_mid + power_high + power_highest + ba + bb + 
               ke + nr + po + tn + tt + diesel + petgas + man + 
               eng1 + eng2 + eng3 + eng4 + eng6, 
             data=auta.train.b,method='anova',model=TRUE,
             control=rpart.control(cp=0,xval=10,minsplit=c(25),minbucket=12,maxdepth=9))
  # Prediction on a testing sample
  bag.fore[,b] = predict(bt,new=oct)
}
# For every single observations in the testing dataset you have B predictions
hist(bag.fore[1,],breaks=50) # nice
# This way you could estimate 'confidence in your prediction'
# For example, for the first car, the 95% confidence would be
quantile(bag.fore[1,],p=c(0.025,0.975))
# Now evaluate
pred = cbind(pred, apply(bag.fore,1,mean,na.rm=T))
colnames(pred)[dim(pred)[2]] = 'DC6'
apply(pred[,-1],2,function(x,y) mean((x-y)^2), y=pred[,1])
apply(pred[,-1],2,function(x,y) mean(abs(x-y)), y=pred[,1])
# Impressive improvements!

# 8) Random forest - can it get any better?
# Let's try to decorrelate trees.
install.packages("ranger")
library(ranger)
# Number of trees
B = 5000
# Depth of the trees
depth = c(3,6,9,12)
# Number of depth parameters
ND = length(depth)
# Number of random 'picks' of features to consider in each split
mtry = c(3,6,9)
# Number of mtry parameters
NR = length(mtry)
# Number of cross-validations
cv = 10
# For each cross-validation sample I need to store predictions
# MSE for CV
rf.cv = array(NA,dim=c(NR,ND,cv))
dimnames(rf.cv)[[1]] = paste('Try',mtry,'features')
dimnames(rf.cv)[[2]] = paste('Depth',depth)
dimnames(rf.cv)[[3]] = paste('CV sample',1:cv)
# I need to find the average values for each cross-validation
rf.cv.ave = array(NA,dim=c(NR,ND))
dimnames(rf.cv.ave)[[1]] = paste('Try',mtry,'features')
dimnames(rf.cv.ave)[[2]] = paste('Depth',depth)
# Now we loop over different parameters & cross-validation samples
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
      auta.train.cvin = ocr[-idx,]
      auta.train.cout = ocr[+idx,]
        
      # Estimate the model
      rf.tree = ranger(price ~ km + km10 + age + power_lowest + power_low + 
                         power_mid + power_high + power_highest + ba + bb + 
                         ke + nr + po + tn + tt + diesel + petgas + man + 
                         eng1 + eng2 + eng3 + eng4 + eng6,
                       data=auta.train.cvin,
                       num.trees=B,mtry=num.try,min.node.size=5,max.depth=num.depth)  
      pred.rf.cv = predict(rf.tree,data=auta.train.cout)$predictions
      rf.cv[m,d,r] = mean((pred.rf.cv - auta.train.cout$price)^2)
    }
    # Average
    rf.cv.ave[m,d] = mean(rf.cv[m,d,])
  }
}
# We estimated the random forest under different hyper-parameters
# Now which has the lowest forecast error via cross-validation?
which(rf.cv.ave == min(rf.cv.ave), arr.ind=TRUE)
mtry.opt  = mtry[3]
depth.opt = depth[4]
rf = ranger(price ~ km + km10 + age + power_lowest + power_low + 
              power_mid + power_high + power_highest + ba + bb + 
              ke + nr + po + tn + tt + diesel + petgas + man + 
              eng1 + eng2 + eng3 + eng4 + eng6,
            data=ocr,num.trees=B,mtry=mtry.opt,min.node.size=5,max.depth=depth.opt)
# Now evaluate
pred = cbind(pred, predict(rf,data=oct)$predictions)
colnames(pred)[dim(pred)[2]] = 'RF'
apply(pred[,-1],2,function(x,y) mean((x-y)^2), y=pred[,1])
apply(pred[,-1],2,function(x,y) mean(abs(x-y)), y=pred[,1])
# We got something

# 9) What about boosting?
# We will use a package
install.packages("gbm")
library(gbm)
# library(xgboost)
mod.gbm = gbm(price ~ km + km10 + age + power_lowest + power_low + 
                power_mid + power_high + power_highest + ba + bb + 
                ke + nr + po + tn + tt + diesel + petgas + man + 
                eng1 + eng2 + eng3 + eng4 + eng6,data=ocr,distribution='gaussian',n.trees=B,
                interaction.depth=depth.opt,shrinkage=0.001,bag.fraction=1)
# Now evaluate
pred = cbind(pred, predict(mod.gbm,new=oct))
colnames(pred)[dim(pred)[2]] = 'GB'
apply(pred[,-1],2,function(x,y) mean((x-y)^2), y=pred[,1])
apply(pred[,-1],2,function(x,y) mean(abs(x-y)), y=pred[,1])
# We got something

##################
# Home assignment
##################
# Design a procedure that will cross-validate shrinkage (learning parameters) and depth of gbm
##################

