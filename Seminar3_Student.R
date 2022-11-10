###################################################################################
# Seminar No. 3
# 1) Work with raw data and convert to a workable dataset (training, testing)
# 2) Prepare summary statistics (to-do write a function that will return a table with: mean, sd, quantiles, skewness, kurtosis, normality test, correlation table)
# 3) Visualize data: Overlapping histograms, Ordered box-plots, x-y plots.
# 4) Estimate lm() models
# 5) compare model accuracy
# 6) consider other loss functions - create your own!
###################################################################################

# The raw dataset
getwd()
oc = read.csv(file='octavia.csv')

# Let's take a look at first 10 observations
head(oc,n=10)
# Let's take a look at last 10 observations
tail(oc,n=10)

###################################################################################
# 1) Work with raw data and convert to a workable dataset (training, testing)

###################################################################################

# I do not like the first column - uninformative
##################################
## TASK 1 - Load data without the first column!
oc =oc[,-(1:2)]
##################################

# Now check
head(oc,n=10)

# Lets translate the names of variables
names(oc)[c(4,5,7)] = c('power','region','transmission')
names(oc)

#################
# Check variables
#################


# YEAR
# Some older cars are not very well represented.
table(oc$year)
# First, all cars older than 2004 incl. are going to be set to 2004
# Second, we will change year to age - more intuitive (better scale)
oc$year[oc$year <= 2004] = 2004
table(oc$year)
#mean(oc$year)
##################################
# TASK 2 - Create age variable
oc$age = 2022-oc$year
table(oc$age)
summary(oc$age)
  
##################################  
  
# KM
summary(oc$km)
# I am suspicious -> outliers?
# I also do not like the unnecessary large number - convert into 1000s of km
oc$km = oc$km/1000
summary(oc$km)
# An ugly histogram
hist(oc$km,prob=T)
hist(oc$km)
# Slightly better looking one
dev.off()
hist(oc$km,breaks=15,prob=T,yaxt='n',xaxt='n',ylim=c(-0.0004,0.008),xlim=c(0,500),density=30,col=rgb(0.5,0.5,0.5,alpha=0.8),xlab=c(),ylab=c(),main='')
axis(1,at=seq(from=0,to=max(oc$km),by=100),label=seq(from=0,to=max(oc$km),by=100),cex.axis=0.85)
axis(2,at=seq(from=0,to=0.01,by=0.001),label=seq(from=0,to=0.01,by=0.001),cex.axis=0.85,las=2)
legend('bottomright',bty='n',legend='Kilometers')
legend('topleft',bty='n',legend='Density')

# We should categorize relatively new cars: km < 10000.
oc$km10 = (oc$km<10)*1

##################################
# TASK 3 - Use log transform on km
oc$lkm = log(oc$km)
table(oc$km)
hist(oc$lkm,prob=T)
    
##################################

# POWER
summary(oc$power)
table(oc$power)
# It seems like there might be errors but also different power levels. Let's create dummies
oc$power_lowest  = (oc$power<77)*1
oc$power_low     = (oc$power>=77 & oc$power<85)*1
oc$power_mid     = (oc$power>=85 & oc$power<103)*1
oc$power_high    = (oc$power>=103 & oc$power<118)*1
oc$power_highest = (oc$power>118)*1
# Domain knowledge will guide us later, as power and fuel should be addressed together

# REGION
table(oc$region)
typeof(oc$region)
oc$region = as.character(oc$region)
# Simplify into dummies
oc$ba = (oc$region == 'BA kraj')*1
oc$bb = (oc$region == 'BB kraj')*1
oc$ke = (oc$region == 'KE kraj')*1
oc$nr = (oc$region == 'NR kraj')*1
oc$po = (oc$region == 'PO kraj')*1
oc$tn = (oc$region == 'TN kraj')*1
oc$tt = (oc$region == 'TT kraj')*1
oc$za = (oc$region == 'ZA kraj')*1

# FUEL
table(oc$fuel)
# Translate and convert: 
typeof(oc$fuel)
oc$fuel = as.character(oc$fuel)
oc$diesel = (oc$fuel == 'Diesel')*1
oc$petrol = (oc$fuel == 'Benzmn')*1 # you might have issues with the special character here
oc$petgas = (oc$fuel == 'Benzmn+Plyn')*1

# TRANSMISSION
table(oc$transmission)
# Simplify and dummify
oc$man = (oc$transmission != 'Automat')*1

# POWER AND FUEL - some important combination?
oc$eng1 = c(oc$power == 77 & oc$diesel == 1)*1
oc$eng2 = c(oc$power == 81 & oc$diesel == 1)*1
oc$eng3 = c(oc$power == 85 & oc$diesel == 1)*1
oc$eng4 = c(oc$power == 103 & oc$diesel == 1)*1
oc$eng5 = c(oc$power == 103 & oc$diesel == 1)*1
oc$eng6 = c(oc$power == 110 & oc$diesel == 1)*1

# PRICE
summary(oc$price)
hist(oc$price,prob=F,breaks=15)
dev.off()
hist(oc$price,breaks=15,prob=F,yaxt='n',xaxt='n',ylim=c(0,180),xlim=c(0,40000),density=30,col=rgb(0.5,0.5,0.5,alpha=0.8),xlab=c(),ylab=c(),main='')
axis(1,at=seq(from=0,to=40000,by=5000),label=seq(from=0,to=40000,by=5000),cex.axis=0.85)
axis(2,at=seq(from=0,to=180,by=30),label=seq(from=0,to=180,by=30),cex.axis=0.85)
legend('bottomright',bty='n',legend='Price')
legend('topleft',bty='n',legend='Density')

# REMOVE WHAT WE DO NOT NEED
names(oc)
oc[,c('year','km','power','region','fuel','transmission')] = NULL
names(oc)

# Divide the sample into training and testing
set.seed(50)
# Overall number of observations
N = dim(oc)[1]

idx = sample(1:N,size=floor(0.8*N),replace=F)
#a =   sample(1:N,size=floor(0.8*N),replace=F)
# Training
ocr = oc[ idx,]
# Number of observations in the Training
NR = dim(ocr)[1]
# Testing
oct = oc[-idx,]
# Number of observations in the Testing
NT = dim(oct)[1]

###################################################################################
#  2) Prepare summary statistics - work in training dataset
###################################################################################

##################################
## TASK 4 - Calculate and print correlation & stat. significance between price and all remaining variables:
table(y)
nv = dim(ocr[2])
y = ocr$price

#for (i in 2:nv) {
 # m = cor.test(y,ocr[,i], method = 'kendall')
  #print(m)
#}
for(i in 2:nv) {
  m = cor.test(y,ocr[,i], method ='kendall')
  print(paste('corr. between price and', names(ocr)[i], 'is' , 
              round(m$estimate,3), 'and the sig. is', round(m$p.value,3)))
}
estimaate
##################################


##################################
## Home assignment: Write a function() that will return 2 tables:
# 1) Descriptive statistics for given variables
# 2) Correlation table with statistical significance
statSigSumm <- function(x) {
  summary(x)
  for(i in 2:nv) {
    m = cor.test(x,ocr[,i], method ='kendall')
    print(paste('corr. between price and', names(ocr)[i], 'is' , 
                round(m$estimate,3), 'and the sig. is', round(m$p.value,3)))
  }
}
summary(ocr$price)  
statSigSumm(ocr$price)
# Are there features that are excessively correlated? (say above 0.95?)


###################################################################################
# 3) Visualize data: Overlapping histograms, Ordered box-plots, x-y plots.
###################################################################################
dev.off()
par(mfrow=c(1, 1)) # layout of figures - (rows,columns)
par(cex = 1.1)
par(oma = c(2, 2.0, 1.0, 1.0))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
par(mar = c(2.0, 3.0, 1.5, 0.5))
hist(oc$price[oc$man==1],breaks=15,prob=T,xaxt='n',
     xlim=c(0,40000),density=10,
     col=rgb(0.85,0.5,0.05,alpha=0.9),
     xlab=c(),ylab=c(),main='',cex.axis=0.55,
     ylim=c(0,9.5^(-4)))
hist(ocr$price[oc$man==0],breaks=15,prob=T,add=T,
     col=rgb(0.15,0.85,0.85,alpha=0.9),density=10)
axis(1,at=seq(from=0,to=40000,by=5000),label=seq(from=0,to=40000,by=5000),cex.axis=0.65)
lines(density(ocr$price[ocr$man==1]),col=rgb(0.85,0.5,0.05),lwd=1.25,lty=2)
lines(density(ocr$price[ocr$man==0]),col=rgb(0.15,0.85,0.85),lwd=1.25,lty=2)
legend('topright',bty='n',legend=c('Distribution of price\nmanual shift\n',
                                   'Distribution of price\nautomatic shift'),
       col=c(rgb(0.85,0.5,0.05),rgb(0.15,0.85,0.85)),cex=0.75,lty=1)


boxplot(price~man,data=ocr,pch=19,cex=0.35,yaxt='n',xlab='',
        ylab = 'Price of the car',xaxt='n',cex.lab=0.75)
axis(2,at=seq(from=0,to=40000,by=5000),label=seq(from=0,to=40000,by=5000),cex.axis=0.65,las=2)
axis(1,at=c(1,2),label=c('Automatic shift', 'Manual shift'),
     cex.axis=0.65)

dev.off()
par(mfrow=c(1, 1)) # layout of figures - (rows,columns)
par(cex = 1.1)
par(oma = c(2, 2.0, 1.0, 1.0))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
par(mar = c(3.0, 3.0, 1.5, 0.5))
plot(x=exp(ocr$lkm[ocr$age > 9]),y=ocr$price[ocr$age > 9], 
     pch=19, cex=0.5, col='black',ylab='Price',xlab='Kilometers',
     ylim=c(0,max(ocr$price)),xlim=c(0,max(exp(ocr$lkm))),
     cex.axis=0.85,cex.lab=0.95)
abline(lm(price~I(exp(lkm)),data=ocr[ocr$age > 9,]),lwd=1.5)
points(x=exp(ocr$lkm[ocr$age <= 9]),y=ocr$price[ocr$age <= 9],
      col='red',pch=19,cex=0.5)
abline(lm(price~I(exp(lkm)),data=ocr[ocr$age <= 9,]),lwd=1.5,
       col='red')
legend('topright',bty='n',legend=c('Dependence for older cars\n(9 +)\n',
                                   'Dependence for younger cars\n(9 -)'),
       col=c('black','red'),cex=0.75,lty=1)

?boxplot
boxplot(price~man,data=ocr)
###################################################################################
# 4) Estimate OLS models
###################################################################################
m1 = lm(price ~ lkm, data=ocr)
summary(m1)

m2 = lm(price ~ age, data=ocr)
summary(m2)

m3 = lm(price ~ lkm + age, data=ocr)
summary(m3)

m4 = lm(price ~ lkm + age + I(lkm*age), data=ocr)
summary(m4)

##################################
# TASK 5 - Estimate your own specification - try to beat the others! You can also create new variables
m5 = lm(price ~ lkm + age + I(lkm*age), data = ocr )
m6 = lm(price ~ lkm + age + I(lkm*age) + I(eng1*age)+I(eng2*age)+
          I(eng3*age)+I(eng4*age)+I(eng5*age) + I(power_low*age)+
          I(eng6*age)+I(ba*lkm)+I(bb*lkm)+I(ke*lkm) + I(nr*age)+
          I(po*lkm)+I(tn*lkm)+I(tt*lkm),data = ocr)
  
##################################

#########
# Predict
# Store

predictions = matrix(NA,nrow=NT,ncol=7)
colnames(predictions) = c('True',paste('p',1:6,sep=''))
predictions[,1] = oct$price
# Predictions itself
?predict

p1 = predict(m1,new=oct); summary(p1)
predictions[,2] = p1
p2 = predict(m2,new=oct); summary(p2) # Negative price does not make sense here... so some adjustment is in order
# We substitute the lowest positive predicted value
p2[p2 < 0] = min(p2[p2>0]); summary(p2)
predictions[,3] = p2
p3 = predict(m3,new=oct); summary(p3) 
# Again
p3[p3 < 0] = min(p3[p3>0]); summary(p3)
predictions[,4] = p3
p4 = predict(m4,new=oct); summary(p4)
predictions[,5] = p4
p5 = predict(m5,new=oct); summary(p5) # Again
p5[p5 < 0] = min(p5[p5>0]); summary(p5)
predictions[,6] = p5
p6 = predict(m6,new=oct);
summary(p6)
p6[p6<0] = min(p6[p6>0])
predictions[,7] = p6
#########

#########
# Evaluate
# Store
mse = matrix(NA,nrow=NT,ncol=6) # 5 models
for (i in 1:6) mse[,i] = (predictions[,1] - predictions[,i+1])^2
apply(mse,2,mean)

#########
# MCS
library(MCS)
MCSprocedure(mse)
#########
