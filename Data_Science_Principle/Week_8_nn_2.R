# Lab Week 8 Neural Nets packages

# Code from RJournal_2010-1_Guenther+Fritsch.pdf

install.packages("neuralnet")
library(neuralnet)
?infert
View(infert)
str(infert)
?neuralnet
# One hidden layers of two neurons
set.seed(123)
nn <- neuralnet(case~age+parity+induced+spontaneous, data = infert, hidden =c(2) , err.fct = "ce", linear.output = FALSE)
plot(nn)
summary(nn)
nn$result.matrix

# Two hidden layers of two neurons, one neuron respectively
set.seed(123)
x <- c(2,1)
nn <- neuralnet(case~age+parity+induced+spontaneous, data = infert, hidden =x , err.fct = "ce", linear.output = FALSE)
plot(nn)

# Use a factor variable
set.seed(123)
nn <- neuralnet(case~education+age+parity+induced+spontaneous, data = infert, hidden = 2, err.fct = "ce", linear.output = FALSE)
infert$education <- as.numeric(infert$education)
str(infert)
nn <- neuralnet(case~education+age+parity+induced+spontaneous, data = infert, hidden = 2, err.fct = "ce", linear.output = FALSE)
plot(nn)
# Question: What does linear.output = FALSE do in the function above?
?neuralnet
#nn

# Return to original as in paper
# One hidden layers of two neurons
set.seed(123)
nn <- neuralnet(case~age+parity+induced+spontaneous, data = infert, hidden =c(2) , err.fct = "ce", linear.output = FALSE)
plot(nn)

summary(nn)
nn$result.matrix

out <- cbind(nn$covariate, nn$net.result[[1]])
dimnames(out) <- list(NULL, c("age","parity","induced","spontaneous","nn-output"))
head(out)
plot(nn)

## Comparing to nnet package
library(nnet)
set.seed(123)
?neuralnet
?nnet
nn.bp <- neuralnet(case~age+parity+induced+spontaneous, data = infert, hidden = 2, err.fct = "ce", 
                   linear.output = FALSE, algorithm = "backprop", learningrate = 0.01)

#nn.bp
summary(nn.bp)

plot(nn.bp)

plot(nn)

par(mfrow=c(2,2))
gwplot(nn,selected.covariate="age",
         min=-2.5, max=5)
gwplot(nn,selected.covariate="parity",
         min=-2.5, max=5)
gwplot(nn,selected.covariate="induced",
       min=-2.5, max=5)
gwplot(nn,selected.covariate="spontaneous",
         min=-2.5, max=5)
?gwplot
