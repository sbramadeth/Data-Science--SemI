# Lab week 5 - Info Gain algorithm - a splitting algorithm for Decision Trees
# We will use the Churn dataset from week 2
# Create a new sub folder for Lab Week 5 - Info Gain
# Download the files from Canvas and put them in the new sub folder
# Best to create an R project for this lab linked to this new sub folder.

# Import the dataset using Import Dataset drop-down, choose From Text(base) as before

Churn_Data.raw.data <- read.csv("C:/Users/aengus.daly/OneDrive/1. Data Science and Analytics/Labs/Week 5 Info Gain/Churn_Data raw data.csv")
View(Churn_Data.raw.data)

c2 <- Churn_Data.raw.data # create a clean copy of the dataset

# We will use the Kelleher et al. pdf from the lectures last week -Machine Learning for  Predictive Analytics
# for details of the algorithm
# Also we will reproduce the Excel calcs given in Churn_Data Lab 5 Info Gain v3.xlsx

# Aim - create a function to calculate entropy for split of the dataset
# using on given column x in Churn dataset.

#Similar to Naive Bayes Classifier create a table and a proportional table - 1-D for Churn
# This calculates the overall entropy for the dataset by response variable.
# This is the starting point

table(c2$Churn)
(Churnpt <- prop.table(table(c2$Churn)))

# Now as before create a function so that we can create any 2-D prop.table
# Fill in z and run it.
z <- 1
tabfun <- function(x) {prop.table(table(c2[,x],c2[,21]), margin = z)}  
print(tabfun(5))  # checking it agrees with Excel

# Now calculate the entropy - i.e. -1 *probability * log2(probability)

-tabfun(5)*log2(tabfun(5))

# or 

-tabfun(5)*log(tabfun(5),2)

# Issue if any probabilities are zero as log2(0) = -Inf

# Fix this with Laplace smoothing 
tabfun <- function(x) {prop.table(table(c2[,x],c2[,21]) + 1e-6, margin = 1)}  
print(tabfun(5))  # checking this agrees with Excel.


# Let's have a look at entropy graphically.

# We can graph this for different probability values against entropy

x <-  seq(0,1, by = 0.005)
plot(x, -x*log(x,2))  # one class - but normally 2 or more classes
plot(x, -(x*log(x,2)+(1-x)*log(1-x,2))) # this is a 2-class problem

# Comment

# Back to entropy
tabfun <- function(x) {prop.table(table(c2[,x],c2[,21]) + 1e-6, margin = 1)}  
print(tabfun(5))

# Now we need rowSums of this, i.e the row sums

rowSums(-tabfun(5)*log2(tabfun(5)))

# Next we need to multiply these by the proportion in each of these rows

prop.table(table(c2$Int.l.Plan))
prop.table(table(c2[,5]))

sum(prop.table(table(c2[,5]))*rowSums(-tabfun(5)*log2(tabfun(5))))

# Check that this is the same as the Excel calcs.
# Slight difference due to Laplace smoothing

# Now bring it altogether with one formula

entropy_tab <- function(x) { tabfun <- prop.table(table(c2[,x],c2[,21]) + 1e-6, margin = 1)
  sum(prop.table(table(c2[,x]))*rowSums(-tabfun(x)*log2(tabfun(x))))}

entropy_tab(5)

#this is tidier

entropy_tab <- function(x) { tabfun2 <- prop.table(table(c2[,x],c2[,21]) + 1e-6, margin = 1)
sum(prop.table(table(c2[,x]))*rowSums(-tabfun2*log2(tabfun2)))}

entropy_tab(5)

# Check for other 2 columns/variables in c from Excel calcs
# Vmail plan is col 6
entropy_tab(6) # agrees with Excel

# Customer Care is col 20
entropy_tab(20) # agrees with Excel

# Now create a for loop for the 3 categorical variables of interest

r <- c(5,6,20)

r3 <- NULL # Initialise variable
for (x in r) { r2 <- entropy_tab(x)
                r3 <- c(r3,r2)
               if (x == 20) print(r3)
                }


# Exercises
# 1 In the for loop above print out the max of the array r3, then print out the column/variable name

r <- c(5,6,20)

r3 <- NULL
for (x in r) { r2 <- entropy_tab(x)
r3 <- c(r3,r2)
if (x == 20) print(c(max(r3),colnames(c2[r[which.min(r3)]])))
}

# 2. Note the values are close together - comment on this.

# This shows that DTs can have high variance/be unstable, a small change in
# the training set can have a large change in the DT, especially at root node
# and high level splits.

# 3. We did not find the info gain - use the excel calcs as reference incorporate this in here.

Churnpt <- prop.table(table(c2$Churn))
(entropy_total <-sum(-Churnpt*log2(Churnpt)))
r <- c(5,6,20)

r3 <- NULL
info_gain <- NULL
for (x in r) { r2 <- entropy_tab(x)

r3 <- c(r3,r2)
if (x == 20) 
{print(c("Entropy minimum",colnames(c2[r[which.min(r3)]]), round(min(r3),5)))
  print(c("Info Gain maximum", round((entropy_total-min(r3)),5)))}  
}

# 3. Variables of type numeric/continuous, check if the function above works for these ( recall we need to find the
# where to split also in the variable.)

# Check column 2 - account length

entropy_tab <- function(x) { tabfun2 <- prop.table(table(c2[,x],c2[,21]) + 1e-6, margin = 1)
sum(prop.table(table(c2[,x]))*rowSums(-tabfun2*log2(tabfun2)))}

entropy_tab(2)



# Yes it gives answer - but what is it doing?
(tabfun <- prop.table(table(c2[,2],c2[,21]), margin = 1))
# or
(table(c2[,2],c2[,21]))
# This is not a reasonable split for a decision tree!
# We need to solve this by using cut function to 'cut' Account.length in a number of factors - say 3 or 4.


entropy_tab_cut <- function(x,y) { tabfun2 <- prop.table(table(cut(c2[,x],y),c2[,21])  + 1e-6, margin = 1)
sum(prop.table(table(cut(c2[,x],y)))*rowSums(-tabfun2*log2(tabfun2)))}

entropy_tab_cut(2,10)

