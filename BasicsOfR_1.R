# Title: Intro to R and RStudio 1
# Goal: Learn basic data types, functions and data structures
# Course: AI in Finance
# Date: 30.9.2022
# Author: Martina HalouskovC/Sujit Thapa
# License: GPL>=3


##############################################################
#####  Mathematical operations (using R as a calculator) #####
##############################################################

# Using R as a calculator
2+2
6/3
2^3     # this is a comment
(5-2)*2
sqrt(16)
log(10) # log base e

# Numeric variables
a=3
b=8
a+b
a*b
(a+b)^2

c=a+b+10
c

d <- a+b    +    10    # spaces are not important, we can use = or <- 
d

e=3
E   # R is case sensitive (E is not e)     


# Notice: a, b, c, d and e are now in our environment (top right window)


##############################################################
#################### Data types, vectors #####################  
##############################################################

# There are 4 students, their heights are: 165 151 182 and 177 centimeters
# Store these numbers in a data vector with the c() function
heights=c(165,151,182,177)

# Need some help?
?c()

# Look at the data
heights

# [1] refers to the first observation
heights[1]

# We can combine vectors
x=c(1.5, 3.2, -4.7)
y=1:10
z=c(x,y)
z

# Vectors have types! 
class(x)
class(y)
# Note: If the data consists of only whole numbers, the type is "integer", otherwise we use "numeric"

is.numeric(x)
is.character(x)

# Character vectors (plain text)
# Lets give these 4 students names
students=c("John", "Sarah", "Tim", "Jane")
class(students)
students[2]
# We can convert data types
x.char=as.character(x)
x.char
x.num=as.numeric(x.char)
x.num


# Logical vectors
# Do these four students speak English?
english=c(TRUE,FALSE,FALSE,TRUE)
class(english)

# How many of them speak English?
sum(english)


# Logical operators 
x == 0
students == "John"
students != "John"
heights > 160
?Logic

# Factors
group = c("Group A", "Group B")
group = as.factor(group)
group
class(group)


##############################################################
################### Working with vectors ##################### 
##############################################################

# Lets explore the vector with heights some more and apply some basic functions
heights

# Length of vector
length(heights)

# Sum
sum(heights)

# What is the average height?
sum(heights)/length(heights)
mean(heights)

# Who is the smallest?
min(heights)
which(heights == min(heights))

# Which student is the tallest?
max(heights)
which(heights == max(heights))

# Min and max
range(heights)

# Standard deviation
sd(heights)

# The summary function produces basic descriptive statistics
summary(heights)

# Boxplot - our first graph!
p=boxplot(heights)
p
# We can sort the heights
sort(heights)
sort(heights, decreasing = T)
sort(heights, decreasing = F)

# Cumulative sum
cumsum(heights)


# Heights of these students the next year
heights.next = c(170,151,183,179)
heights.next[2]-heights.next[1]

# How much did they grow?
heights.next - heights

# How much did they grow on average?
mean(heights.next - heights)

# Notice, functions are applied to each entry in the vector


##############################################################
############## Conditional Statements and Loops ##############
##############################################################

# Conditional Statements (Similar to IF in excel)

# Test, whether a variable is equal to 1
variable=2
if (variable == 1){
  print("Yes")
}


# Great! Now lets add "else"
variable=1
if (variable > 0){
  print("Yes")
} else {
  print("No")
}


# Try setting variable equal to any value you want
variable= c("SUJIT", "SHYAM", "GHANSHYAM")
  
# If "variable" is greater than 4, divide it by 2 and print it
# If not, print a message that tells us, the variable is smaller then 4
if(variable == "Shyam"){
  print("gotcha2")
}else{
  print("sorry")
}

# FOR LOOPS
for (i in 1:10){
  print(i)
}

for (i in 1:10){
  j=i*2
  print(j)
}

for (i in seq(5,15)){
  print(i)
}


# Let's combine loops with if statements
for (i in 1:30){
  if((i %% 2) == 0) {
    print(paste(i,"is even"))
  } else {
    print(paste(i,"is odd"))
  }
}


# WHILE LOOPS
i=1
while (i < 10) {
  print(i)
  i=i+1
}


# FINAL CHALANGE - create a function
my_function <- function(x){
  x^2/2
}

my_function(3)


# Define a function, that returns:
# the sum of all values in a vector, if the vector has at least 5 values
# or the mean of all values if there are less then 5 values

my_func <- function(x){
  if (length(x) >= 5){
    sum(x)
  } else {
    mean(x)
  }
}

x=c(seq(1:14))
my_func(x)


##############################################################
##############  Data structures - DATAFRAMES #################  
##############################################################

# Constructing data frames (something like an excel spreadsheet)
vec1 = rep(c("A","B","C","D","E","F"), 10)
vec2 = rnorm(n=60, mean=10, sd=5)
vec3 = rep(c("TRUE","FALSE"),30)

DT=data.frame("grades"=vec1,
              "points"=vec2,
              "logicvec"=vec3)

# Explore
View(DT)
head(DT)
tail(DT)

# What are the dimensions?
dim(DT)

dim(DT)[2]
# Number of rows
nrow(DT)

#Number of columns
ncol(DT)

# Column names and row names
names(DT)
colnames(DT)
rownames(DT)

# Add new row names
rownames(DT)=paste("row", seq(60))

####
DT["row 2", ]
# Accessing elements 
# 1. By names
DT$grades
DT$points
DT$logicvec

# 2. By indices
# DT[row,column]
# Look at a single row
DT[2,]
# Look at a single column
DT[,2]
# Look the value in first row and first column
DT[1,1] 
DT["row 1","grades"]
# Look at multiple rows and multiple columns
DT[c(3,5),1:3]
DT[c(3,5),c(1,2,3)]


# Setting new values
# 1. By names
DT$empty=NA
DT$points2=DT$points*2
DT$diff=DT$points2 - DT$points
View(DT)

# 2. By indices
DT[,7]=rnorm(n=60, mean=0, sd=1)
DT[61,]=c(1:7)

# 3. With logical operators
nerds = DT[DT$grades == "A",]
nerdsss = DT[DT["grades"] == "A",]
top = DT[DT$points >= 20,]

# 4. Subsetting
topdiff = subset(DT, points>= 20, select = c(points,diff))

# Combine dataframes
rbind(nerds,top)
cbind(top,topdiff)


##############################################################
########### Working directory, Save and load files  ##########  
##############################################################

# CSV

# Save this dataset as a csv to a folder called "data"
getwd()
my_wd=getwd()
setwd("./data")
write.csv(x = DT, file = "DT.csv")

# Load this dataset back to a new object
newDT=read.csv("DT.csv")
rownames(newDT)=newDT$X
newDT=newDT[,2:8]

# RData
save(DT, file="DT.RData")
load("DT.RData")

# RDS
saveRDS(DT, file="DT")
DT=readRDS("DT")
setwd(my_wd)


##############################################################
################  Data structures - LISTS ####################  
##############################################################

# Vectors or any datatypes and datastructures can be combined into lists
# Lists can be named and nested
mylist = list("grades"=vec1,
              "points"=vec2,
              "logicvec"=vec3,
              "nerds"=nerds,
              "gradelist"=list("A"=1,"B"=2,"C"=3, "D"=4, E="5", "F"=6))

class(mylist)
str(mylist)

# Accessing lists
mylist[[1]]       # by index
mylist$logicvec   # by a name
mylist[[4]][1,1]  # first column & first row in fourth list element
mylist$nerds[1,1]
mylist[[5]]       # fifth element is a list within a list
mylist[[5]][[1]]  

length(mylist[[2]])
mean(mylist[[2]])


##############################################################
##############  Data structures - MATRICES ###################  
##############################################################

A=matrix(1:10,nrow=2)
B=matrix(11:20,ncol=5)
A
B
is.matrix(A)
is.matrix(B)
A+B
A-B
A*B
3*A
A%*%B
A %*% t(B)
C = t(A) %*% B

topdiff.m=as.matrix(topdiff)
topdiff.m

k=c(1,8,6,3,11)
l=c(9,7,6,2,2)
m=rbind(k,l)
n=cbind(k,l)
is.matrix(n)
