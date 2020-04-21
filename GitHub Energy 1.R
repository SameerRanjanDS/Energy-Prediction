library(tidyverse)
library(MASS)
library(readr)
library(modelr)
library(class)
library (dplyr)


#new assignment
energy <- read.csv("energydata_complete.csv")
getwd()


sample_ind <- sample(nrow(energy),nrow(energy)*0.70)
train <- energy[sample_ind,]
test <- energy[-sample_ind,]
install.packages('e1')
library(e1071)
High = ifelse(energy$Appliances<=100,"No","Yes")
energy = data.frame(energy,High)
energy<- energy[,-c(1,2)]

#Test Energy 
x<- test[,-28]
y<- test[,28]

m <- svm(x,y, scale = TRUE, type = NULL, kernel =
           "radial", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
         coef0 = 0, cost = 1, nu = 0.5,
         class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
         shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
m
pred<- predict(m,x)
table(pred,y)

#Train Energy
x1<- train[,-28]
y1<- train[,28]

m <- svm(x1,y1, scale = TRUE, type = NULL, kernel =
           "radial", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
         coef0 = 0, cost = 1, nu = 0.5,
         class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
         shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
m
pred<- predict(m,x1)
table(pred,y1)

#method 2
#test 
n<- svm(x, y = NULL, scale = TRUE, type = NULL, kernel =
          "linear", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
pred<- predict(n,x)
table(pred,y)

#train
n<- svm(x1, y1 = NULL, scale = TRUE, type = NULL, kernel =
          "linear", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
pred<- predict(n,x1)
table(pred,y1)



#method 3
#Test 
p<- svm(x, y = NULL, scale = TRUE, type = NULL, kernel =
          "sigmoid", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)

pred<- predict(p,x)
table(pred,y)
#train 
p<- svm(x1, y1 = NULL, scale = TRUE, type = NULL, kernel =
          "sigmoid", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)

pred<- predict(p,x1)
table(pred,y1)


#Decision Tree Test 

library(rpart)

fit <- rpart(High ~.,method="class", data=train)
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for energy")
text(fit, use.n=TRUE, all=TRUE, cex=0.6)

# prune the tree
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
summary(pfit)
# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for energy")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

# Decision Tree Test
fit1 <- rpart(High ~.,method="class", data=test)
printcp(fit1) # display the results
plotcp(fit1) # visualize cross-validation results
summary(fit1) # detailed summary of splits

# plot tree
plot(fit1, uniform=TRUE,
     main="Classification Tree for energy")
text(fit1, use.n=TRUE, all=TRUE, cex=0.6)

# prune the tree
pfit1<- prune(fit1, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
summary(pfit1)
# plot the pruned tree
plot(pfit1, uniform=TRUE,
     main="Pruned Classification Tree for energy")
text(pfit1, use.n=TRUE, all=TRUE, cex=.8)

#boosting 
library(adabag)
install.packages('adabag')
model = boosting(High~., data=train, boos=TRUE, mfinal=50)
print(names(model))
print(model$trees[1])

pred = predict(model, test)
print(pred$confusion)
print(pred$error)

result = data.frame(test$High, pred$prob, pred$class)
print(result)

# cross-validataion method
cvmodel = boosting.cv(High~., data=train, boos=TRUE, mfinal=10, v=5)

print(cvmodel[-1])                         
print(data.frame(train$High, cvmodel$class))
data_check <- data.frame(train$High, cvmodel$class)
table(train$High,cvmodel$class)

cvmodel = boosting.cv(High~., data=test, boos=TRUE, mfinal=10, v=5)

print(cvmodel[-1])                         
print(data.frame(test$High, cvmodel$class))
data_check <- data.frame(test$High, cvmodel$class)
table(test$High,cvmodel$class)

#new partition of data

sample_ind <- sample(nrow(energy),nrow(energy)*0.60)
train <- energy[sample_ind,]
test <- energy[-sample_ind,]


m <- svm(x,y, scale = TRUE, type = NULL, kernel =
           "radial", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
         coef0 = 0, cost = 1, nu = 0.5,
         class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
         shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
m
pred<- predict(m,x)
table(pred,y)

#Train Energy
x1<- train[,-28]
y1<- train[,28]

m <- svm(x1,y1, scale = TRUE, type = NULL, kernel =
           "radial", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
         coef0 = 0, cost = 1, nu = 0.5,
         class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
         shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
m
pred<- predict(m,x1)
table(pred,y1)

#method 2
#test 
n<- svm(x, y = NULL, scale = TRUE, type = NULL, kernel =
          "linear", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
pred<- predict(n,x)
table(pred,y)

#train
n<- svm(x1, y1 = NULL, scale = TRUE, type = NULL, kernel =
          "linear", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
pred<- predict(n,x1)
table(pred,y1)



#method 3
#Test 
p<- svm(x, y = NULL, scale = TRUE, type = NULL, kernel =
          "sigmoid", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)

pred<- predict(p,x)
table(pred,y)
#train 
p<- svm(x1, y1 = NULL, scale = TRUE, type = NULL, kernel =
          "sigmoid", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)

pred<- predict(p,x1)
table(pred,y1)


#Decision Tree Test 

library(rpart)

fit <- rpart(High ~.,method="class", data=train)
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for energy")
text(fit, use.n=TRUE, all=TRUE, cex=0.6)

# prune the tree
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for energy")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

# Decision Tree Test
fit1 <- rpart(High ~.,method="class", data=test)
printcp(fit1) # display the results
plotcp(fit1) # visualize cross-validation results
summary(fit1) # detailed summary of splits

# plot tree
plot(fit1, uniform=TRUE,
     main="Classification Tree for energy")
text(fit1, use.n=TRUE, all=TRUE, cex=0.6)

# prune the tree
pfit1<- prune(fit1, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
summary(pfit1)
# plot the pruned tree
plot(pfit1, uniform=TRUE,
     main="Pruned Classification Tree for energy")
text(pfit1, use.n=TRUE, all=TRUE, cex=.8)

#boosting 
library(adabag)
install.packages('adabag')
model = boosting(High~., data=train, boos=TRUE, mfinal=50)
print(names(model))
print(model$trees[1])

pred = predict(model, test)
print(pred$confusion)
print(pred$error)

result = data.frame(test$High, pred$prob, pred$class)
print(result)

# cross-validataion method
cvmodel = boosting.cv(High~., data=train, boos=TRUE, mfinal=10, v=5)

print(cvmodel[-1])                         
print(data.frame(train$High, cvmodel$class))
data_check <- data.frame(train$High, cvmodel$class)
table(train$High,cvmodel$class)

# new partition 

sample_ind <- sample(nrow(energy),nrow(energy)*0.50)
train <- energy[sample_ind,]
test <- energy[-sample_ind,]
x<- test[,-28]
y<- test[,28]

m <- svm(x,y, scale = TRUE, type = NULL, kernel =
           "radial", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
         coef0 = 0, cost = 1, nu = 0.5,
         class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
         shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
m
pred<- predict(m,x)
table(pred,y)

#Train Energy
x1<- train[,-28]
y1<- train[,28]

m <- svm(x1,y1, scale = TRUE, type = NULL, kernel =
           "radial", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
         coef0 = 0, cost = 1, nu = 0.5,
         class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
         shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
m
pred<- predict(m,x1)
table(pred,y1)

#method 2
#test 
n<- svm(x, y = NULL, scale = TRUE, type = NULL, kernel =
          "linear", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
pred<- predict(n,x)
table(pred,y)

#train
n<- svm(x1, y1 = NULL, scale = TRUE, type = NULL, kernel =
          "linear", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)
pred<- predict(n,x1)
table(pred,y1)



#method 3
#Test 
p<- svm(x, y = NULL, scale = TRUE, type = NULL, kernel =
          "sigmoid", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)

pred<- predict(p,x)
table(pred,y)
#train 
p<- svm(x1, y1 = NULL, scale = TRUE, type = NULL, kernel =
          "sigmoid", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
        coef0 = 0, cost = 1, nu = 0.5,
        class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
        shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, subset, na.action = na.omit)

pred<- predict(p,x1)
table(pred,y1)


#Decision Tree Test 

library(rpart)

fit <- rpart(High ~.,method="class", data=train)
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for energy")
text(fit, use.n=TRUE, all=TRUE, cex=0.6)

# prune the tree
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for energy")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

# Decision Tree Test
fit1 <- rpart(High ~.,method="class", data=test)
printcp(fit1) # display the results
plotcp(fit1) # visualize cross-validation results
summary(fit1) # detailed summary of splits

# plot tree
plot(fit1, uniform=TRUE,
     main="Classification Tree for energy")
text(fit1, use.n=TRUE, all=TRUE, cex=0.6)

# prune the tree
pfit1<- prune(fit1, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
summary(pfit1)
# plot the pruned tree
plot(pfit1, uniform=TRUE,
     main="Pruned Classification Tree for energy")
text(pfit1, use.n=TRUE, all=TRUE, cex=.8)

#boosting 
library(adabag)
install.packages('adabag')
model = boosting(High~., data=train, boos=TRUE, mfinal=50)
print(names(model))
print(model$trees[1])

pred = predict(model, test)
print(pred$confusion)
print(pred$error)

result = data.frame(test$High, pred$prob, pred$class)
print(result)

# cross-validataion method
cvmodel = boosting.cv(High~., data=train, boos=TRUE, mfinal=10, v=5)

print(cvmodel[-1])                         
print(data.frame(train$High, cvmodel$class))
data_check <- data.frame(train$High, cvmodel$class)
table(train$High,cvmodel$class)

