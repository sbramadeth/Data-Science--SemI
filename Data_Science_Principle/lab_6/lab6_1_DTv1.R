# Lab 6 : Decision Trees and x-validation


# Fitting Classification Trees


install.packages("tree")
library(tree)
# If there is an issue with the R version on your PC then you will need to run the following two commands to
# load a previous version of the rpart package with is compatible with the older version of R on your PC.
#require(devtools)
#install_version("tree", version = "1.0-39", repos = "http://cran.us.r-project.org")
install.packages("ISLR") # To get the dataset Carseats.
library(ISLR)
#attach(Carseats)
Carseats <- Carseats
?Carseats
help("Carseats")
# Carry out some EDA

str(Carseats)
View(Carseats)
summary(Carseats)
hist(Carseats$Age)
hist(Carseats$Sales)
plot(Carseats$Age, Carseats$Sales)

# Create a categorical response variable for classification using ifelse command
?ifelse
help(ifelse)
High <- ifelse(Carseats$Sales<=8,"No","Yes")
Carseats <- data.frame(Carseats,High)
str(Carseats)
# Run the following if you have a variable called Carseats$High.1
#Carseats$High.1 <- NULL
# Carseats should now be 400X12 dataset

?tree
help(tree)
tree.carseats <- tree(High~.-Sales,data = Carseats)
summary(tree.carseats)
# if you are getting a Warning message:
#In tree(High ~ . - Sales, data = Carseats) : NAs introduced by coercion
# then you need to change the column High to factor
Carseats$High <- as.factor(Carseats$High)
str(Carseats)
# rerun
tree.carseats <- tree(High~.-Sales,data = Carseats)
summary(tree.carseats)

table(Carseats$High)/nrow(Carseats)
# ..... is the baseline rate, your model need to be better than this rate.
# i.e. the majority class proportion of the total.

plot(tree.carseats)
text(tree.carseats,pretty=1)
tree.carseats
# What appears to be the most important variable in this classification problem?
# It is ....... because...

# Now split into training and testing dataset 50:50
set.seed(2) # set a random seed
?sample
x <- c(1,2,3,4,5,6)
sample(x,2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
#[] are used for subsetting the dataset with a structure [rows,columns]
High.test <- High[-train]
tree.carseats <- tree(High~.-Sales,Carseats,subset=train)
tree.pred <- predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
# This table is called a ...... matrix
# The overall accuracy rate here is .......
(98+51)/200


# Since this is ... than baseline,  this is .... model.

# Now use cross fold validation to tune the parameter, size, which equals the number of leaves
# this is done with by pruning the tree using cross fold validation


set.seed(3)
?cv.tree
cv.carseats3 <- cv.tree(tree.carseats,K=10, FUN=prune.misclass)

#.... folds of cross validation are being used.

names(cv.carseats3)
cv.carseats3
# plot the results

par(mfrow=c(1,2))
plot(cv.carseats3$size,cv.carseats3$dev,type="b")
plot(cv.carseats3$k,cv.carseats3$dev,type="b")
# .... is the best cross validation error rate, i.e.
# Size of tree = ..... looks best here.
#Look up this cost-complexity parameter k and pruning in ISLR page 3331-335 formula 8.1 where it is called alpha.

?prune.misclass
prune.carseats<-prune.misclass(tree.carseats,best=13)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred<-predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

# The test set error then is ......

# We have tuned the parameter size to make a better model as the accuracy has increased from ... to ....
# 

# Exercise
#1. Redo for Churn dataset, remember Y variable is Churn,
# Import this dataset as before using Import Dataset>From Text(base)
# I have Copied the code from above for you to adapt it.
# create a copy of the dataset and call it c2 (as before)
# With Churn as the Y variable find the baseline rate.
# 

c2 <- Churn_Data.raw.data

# Build an initial tree, call this tree.churn removing the columns Phone and State.

tree.carseats <- tree(High~.-Sales,Carseats)
summary(tree.carseats)
table(Carseats$High)/nrow(Carseats)
# This is the baseline rate, your model needs to be better than this rate.
# i.e. the majority class proportion of the total.

# Now plot the tree

plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

#... appears to be the most important variable

# Now split into training and testing dataset 50:50
set.seed(2) # set a random seed
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
#[] are used for subsetting the dataset with a structure [rows,columns]
#High.test <- High[-train] not needed here
tree.carseats <- tree(High~.-Sales,Carseats,subset=train)
tree.pred <- predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

# ...... is the accuracy rate in the test set.
# Is better than baseline model?

# Now use x-validation to prune the tree

set.seed(3)
?cv.tree
cv.carseats3 <- cv.tree(tree.carseats,K=10, FUN=prune.misclass)
names(cv.carseats3)
cv.carseats3

# .... is the best cross validation error rate, i.e.

par(mfrow=c(1,2))
plot(cv.carseats3$size,cv.carseats3$dev,type="b")
plot(cv.carseats3$k,cv.carseats3$dev,type="b")

# Now prune the tree and check is it better on the test dataset.

# Size of tree = ..... looks best here.

prune.carseats<-prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred<-predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)


# ...... is the accuracy rate in the test set.
# We have tuned the parameter size to make a better model.

