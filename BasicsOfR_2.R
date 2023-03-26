# Title: Intro to R and RStudio 2
# Goal: Learn more about data frames, plots, models and statistical testing
# Course: AI in Finance
# Date: 30.9.2022
# Author: Martina HalouskovC!
# License: GPL>=3


# First, create a project


##############################################################
###########  SOME MORE FUN STUFF WITH DATAFRAMES #############  
##############################################################

# Installing and loading packages
install.packages("datasets")
library("datasets")

# Load a built-in dataset from library datasets
data(iris)
data()
DT=iris

# Lets explore this dataset
DT
head(DT)
tail(DT)
dim(DT)
names(DT)
class(DT)

# Any missing values?
is.na(DT)
sum(is.na(DT))
which(is.na(DT))
DT[complete.cases(DT),]

# What are the sums of rows and columns?
colSums(DT) # returns an error
colSums(DT[,c(14)])
rowSums(DT[c(5,6), c(1:2)])

# Summarize this dataset
summary(DT) 

# What are the 3 main species?
unique(DT$Species)
# How many flowers are in each species?

# Summary in a graph = a boxplot!
boxplot(DT[,-c(5)])

# Plot
plot(DT)  # or pairs(DT)
cor(DT)   # returns error, why?
cor(DT[,-c(5)])
DT["Sepal.Length"]
# Scatterplot of one pair
plot(DT$Sepal.Length, DT$Petal.Length)

# Can we test the correlation of this pair?
cor.test(DT$Sepal.Length, DT$Petal.Length)

# How about a linear model?
model=lm(Petal.Length~Sepal.Length, data=DT)
summary(model)

plot(DT$Sepal.Length, DT$Petal.Length)
abline(m)



# Lets look at Petal.Length. Notice anything suspicious?
plot(DT$Petal.Length)
barplot(DT$Petal.Length)
hist(DT$Petal.Length)
table(DT$Petal.Length)
table(DT$Species,DT$Petal.Length)
plot(DT$Petal.Length, col=DT$Species)

# Use boxplot to compare Petal.Length for individual species
boxplot(Petal.Length~Species,data=DT)

boxplot(Petal.Length~Species,
        data=DT,
        main = "Petal Length in 3 species of iris",
        xlab = "Species of flowers",
        ylab = "Petal Length",
        col=c("red","green","blue"))

# Lets subset Petal.Length into 3 different vectors
setosa = DT[DT$Species == "setosa",3]
versicolor = DT[DT$Species == "versicolor",3]
virginica = DT[DT$Species == "virginica",3]

# Compare their histograms
par(mfrow=c(1,3))
hist(setosa)
hist(versicolor)
hist(virginica)
par(mfrow=c(1,1))

# Are these differences significant?
t.test(setosa,versicolor)


##############################################################
####################  SOME Nicer graphs ######################  
##############################################################

# Lattice graphics
install.packages("lattice")
library("lattice")
histogram(setosa)
xyplot(DT$Sepal.Length~DT$Petal.Length)
xyplot(DT$Petal.Length~DT$Species)
xyplot(DT$Sepal.Length~DT$Petal.Length|DT$Species)

# ggplot2
install.packages("ggplot2")
library(ggplot2)

# Scatter plots
qplot(Petal.Length, Petal.Width, data = iris)

# ggplot
g1 = ggplot(data=DT, aes(x=Sepal.Length, y=Petal.Length)) +
  geom_line()
g1

g1 = ggplot(data=DT, aes(x=Sepal.Length, y=Petal.Length, color=Species)) +
  geom_point(shape=17, size=1.2)
g1

g2 = g1 + geom_smooth(method="lm")
g2 + theme_minimal()

g3 = g2 + theme(axis.line = element_line(colour = "blue", size = .1, linetype = "dashed"),
                axis.text = element_text(size=12), axis.title=element_text(size=12,face="bold")) 
g4 = g3 +
  ggtitle("Our first ggplot graph with a title") +
  labs(y = "Petal Length (cm)") + labs(x = "Sepal Length (cm)")+
  theme(legend.title = element_blank())

g4  

g5 = ggplot(data=DT, aes(x=Sepal.Length, y=Petal.Length, color=Species)) +
  geom_point(shape=17, size=1.2) + 
  geom_smooth(method="lm") +
  theme(axis.line = element_line(colour = "blue", size = .1, linetype = "dashed"),
        axis.text = element_text(size=12), axis.title=element_text(size=12,face="bold"))+
  ggtitle("Our first ggplot graph with a title") +
  labs(y = "Petal Length (cm)") + labs(x = "Sepal Length (cm)")+
  theme(legend.title = element_blank())
g5

##############################################################
####################  SOME MORE problems #####################  
##############################################################


# Try going through these on your own
# 1. Change the rownames
rownames(DT) = paste("row", seq(150))


# 2. How many flowers of the species "setosa" have the Sepal wider then 3.5?
setosa.dt= DT[DT["Species"] == "setosa",]
setosa.wi=setosa.dt[setosa.dt["Sepal.Width"] < "3.5",]
nrow(setosa.wi)


# 3. Calculate the median and standard deviation of all variables 4 numeric variables in the dataset. 
#    Assign these results into vectors. Store these two vectors in a list.
median(DT[,1])
median(DT[,2])
median(DT[,3])
median(DT[,4])
sd(DT[,1])
sd(DT[,2])
sd(DT[,3])
sd(DT[,4])

vec.med=c(median(DT[,1]), median(DT[,2]), median(DT[,3]), median(DT[,4]))
vec.sd=c(sd(DT[,1]), sd(DT[,2]), sd(DT[,3]), sd(DT[,4]))
veclist = list("median"=vec.med, "sd"=vec.sd)


# 4. Is there a relationship between the Petal length and Petal width? 
#    Fit a linear model of Petal length modeled by Petal width 
#    Create a scatter plot and add a regression line

plot(DT$Petal.Length, DT$Petal.Width)
plot(DT[,"Petal.Length"], DT[,"Petal.Width"])
cor.test(DT[,"Petal.Length"], DT[,"Petal.Width"])
cor.test(DT$Petal.Length, DT$Petal.Width)
model=lm(Petal.Length~Petal.Width, data=DT)
summary(model)

plot(DT$Petal.Width, DT$Petal.Length)
abline(model)



# 5. Add a new column with log transformation of variable Sepal length
#    Make a plot of this new variable. Connect the dots using a type="l" argument
#    Change the axis labels and plot title to explain your graph
#    Change the color of the line to green
#    Assign this plot to an object and then save this object as an RData file 
#    Save this plot as an image
DT$log = log(DT$Sepal.Length)
plot(DT$log, type = "b", xlab = "Rows", ylab = "Log Value")



# 7. Find out, which rows of DT correspond to Sepal width equal to 2.2.
#    Replace all values in these rows with empty values (NAs)
#    use function na.omit() function complete.cases() to remove these rows 




