#Installing all of the packages required for the model
install.packages(c('rpart','rpart.plot','caret'))
install.packages('VIF')
install.packages('randomForest')
install.packages('gbm')

#Loading all of the packages
library(dplyr)
library(ggplot2)
library(VIF)
library(ROCR)
library(caTools)
library(caret)
library(randomForest)
library(gbm)
library(GGally)
library(rpart)
library(rpart.plot)

########################################################

#Reading the datafile 
letters242 <- read.csv("Letters242.csv")
str(letters242)

##PART A
#Exploratory Data Analysis
#Creating BoxPlots for the given features 
par(mfrow=c(1,1))
boxplot(letters242$onpix~letters242$letter, ylab = "onpix")
boxplot(letters242$x2ybar~letters242$letter, ylab = "x2y")
boxplot(letters242$xy2bar~letters242$letter, ylab = "xy2")
boxplot(letters242$xedge~letters242$letter, ylab = "xedge")
boxplot(letters242$yedge~letters242$letter, ylab = "yedge")
boxplot(letters242$xedgeycor~letters242$letter, ylab = "xedgeycor")
boxplot(letters242$yedgexcor~letters242$letter, ylab = "yedgexcor")

ggscatmat(letters242, columns=2:3, alpha = 0.8, color="letter")
ggscatmat(letters242, columns=7:8, alpha = 0.8, color="letter")
ggscatmat(letters242, columns=9:10, alpha = 0.8, color="letter")
ggscatmat(letters242, columns=c(14,16), alpha = 0.8, color="letter")
ggscatmat(letters242, columns=c(15,17), alpha = 0.8, color="letter")


##PART B
#Adding another feature to check if the letter is B
letters242$isB<-as.factor(letters242$letter=="B")
set.seed(456)


#Forming the logistic regression model to find out if the letter is B or not

#Baseline Model
base_conf <- table(letters242.test$isB,pred>1)
base_conf
acc_base <- sum(diag(base_conf))/sum(base_conf)
acc_base

#Splitting into testing and training data
train.ids = sample(nrow(letters242), 0.65*nrow(letters242))
letters242.train = letters242[train.ids,]
letters242.test = letters242[-train.ids,]


#Model 1
letters_glm <- glm(isB ~ xbox+ybox+width+height+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge+xedgeycor+yedge+yedgexcor,
                   data = letters242.train, family = 'binomial')
summary(letters_glm)

#Model Final
letters_glm2 <- glm(isB ~ width+height+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge+yedge+yedgexcor,
                    data = letters242.train, family = 'binomial')
summary(letters_glm2)

#Prediction using the final model
pred <- predict (letters_glm2, newdata = letters242.test, type = "response")
summary(pred)

#Confusion Matrix
conf_matrix <- table(letters242.test$isB, pred>0.5)
conf_matrix

#Calculating the accuracy of the model
acc <- sum(diag(conf_matrix))/sum(conf_matrix)
acc

#ROC curve and AUC value
roc <- prediction(pred,letters242.test$isB)
log_perf <- performance(roc,"tpr","fpr")
plot(log_perf,colorize=TRUE)
as.numeric(performance(roc,"auc")@y.values)

######Creating the CART model for the data

cpval <- data.frame (cp=seq(0,0.04,0.002))

cart_train <- train(isB ~ xbox+ybox+width+height+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge+xedgeycor+yedge+yedgexcor,
                    data = letters242.train,
                    method = "rpart",
                    tuneGrid = cpval,
                    trControl = trainControl(method = "cv",number = 10),
                    metric = "Accuracy")
cart_train$results
plot(cart_train$results, aes(x=cpval, y=Accuracy))

#Best Cart Model
cart_train$bestTune
cart_best <- cart_train$finalModel
prp(cart_best)

#Using final CART model for predicting
cart_pred <- predict (cart_best, newdata = letters242.test, type = "class")
summary(cart_pred)

#Confusion matrix for CART
cart_conf <- table(letters242.test$isB,cart_pred)
cart_conf

#Accuracy of CART model - 
cart_acc <- (sum(diag(cart_conf)))/sum(cart_conf)


########Creating a Random Forest for the model for B/not B
rf <- randomForest(isB ~ xbox+ybox+width+height+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge+xedgeycor+yedge+yedgexcor,
                   data = letters242.train)
summary(rf)

#Predicting using random forests
rf_pred <- predict(rf,newdata = letters242.test)
summary(rf_pred)

#Confusion matrix
rf_conf <- table(letters242.test$isB,rf_pred)
rf_conf

#Accuracy
rf_acc <- (sum(diag(rf_conf)))/sum(rf_conf)

#################################################################
##PART C

#Baseline Model
table(letters242.test$letter=="A")
table(letters242.test$letter=="B")
table(letters242.test$letter=="P")
table(letters242.test$letter=="R")

#Baseline Accuracy
acc_A <- nrow(letters242.test[letters242.test$letter == "A",])/nrow(letters242.test)
acc_B <- nrow(letters242.test[letters242.test$letter == "B",])/nrow(letters242.test)
acc_P <- nrow(letters242.test[letters242.test$letter == "P",])/nrow(letters242.test)
acc_R <- nrow(letters242.test[letters242.test$letter == "R",])/nrow(letters242.test)


#CART model for predicting all the letters
cart_t <- train(letter ~xbox+ybox+width+height+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge+xedgeycor+yedge+yedgexcor,
                data = letters242.train,
                method = "rpart",
                tuneGrid = cpval,
                trControl = trainControl(method = "cv",number = 10),
                metric = "Accuracy")
cart_t$results
plot(cart_t$results, aes(x=cpval, y=Accuracy))

#Best Cart Model
cart_t$bestTune
cart_best2 <- cart_t$finalModel
prp(cart_best2)

#Using final CART model for predicting
cart_pred2 <- predict (cart_best2, newdata = letters242.test, type = "class")
summary(cart_pred2)

#Confusion matrix for CART
cart_conf2 <- table(letters242.test$letter,cart_pred2)
cart_conf2

#Accuracy of CART model - 
cart_acc2 <- (sum(diag(cart_conf2)))/sum(cart_conf2)

########Creating a Random Forest for the model for predicting all of the letters
rf2 <- randomForest(letter ~ xbox+ybox+width+height+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge+xedgeycor+yedge+yedgexcor,
                   data = letters242.train)
summary(rf2)

#Predicting using random forests
rf_pred2 <- predict(rf2,newdata = letters242.test)
summary(rf_pred2)

#Confusion matrix
rf_conf2 <- table(letters242.test$letter,rf_pred2)
rf_conf2

#Accuracy
rf_acc2 <- (sum(diag(rf_conf2)))/sum(rf_conf2)

### Using the mtry method for random forests with the train command
rf_new <- train(letter ~xbox+ybox+width+height+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge+xedgeycor+yedge+yedgexcor,
                data = letters242.train,
                method="rf",
                tuneGrid = data.frame(mtry = 1:16),
                trControl = trainControl(method="cv",number = 5, verboseIter = TRUE),
                metric="Accuracy")

ggplot(rf_new,aes(x=mtry,y=Accuracy))
rf_new$results

#Predicting using the new final model
best_rf_new <- rf_new$finalModel
rf_new_pred <- predict(best_rf_new, newdata = letters242.test)

#Confusion Matrix for the random forest model
conf_rf_new <- table(letters242.test$letter, rf_new_pred)
conf_rf_new

#Accuracy for the new model 
acc_rf_new <- sum(diag(conf_rf_new))/sum(conf_rf_new)
acc_rf_new

## Applying Boosting to the model using gbm
model_gbm <- gbm(letter ~ xbox+ybox+width+height+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge+xedgeycor+yedge+yedgexcor,
                 data = letters242.train,
                 distribution = "multinomial",
                 n.trees = 3300,
                 interaction.depth = 10)
model_gbm_pred <- predict(model_gbm, newdata= letters242.test, n.trees = 3300, type = "response")

#Converting values from a probability class to a vector class
pred = apply(model_gbm_pred, 1, which.max)
pred = factor(pred, levels = c(1,2,3,4), labels = c("A", "B", "P", "R"))

#Confusion Matrix for Boosting
gbm_conf <- table(letters242.test$letter,pred)
gbm_conf

#Accuracy of the model
acc_gbm <- sum(diag(gbm_conf))/sum(gbm_conf)
acc_gbm























































