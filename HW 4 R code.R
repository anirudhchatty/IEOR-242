#Installing all of the packages
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("caTools")
install.packages("dplyr")
install.packages("caret")
install.packages("stringr")
install.packages("NLP")
install.packages("XML")
install.packages("tm.plugin.webmining")
install.packages("boot")

#Loading all of the files
library(tm)
library(SnowballC)
library(wordcloud)
library(MASS)
library(caTools)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(XML)
library(tm.plugin.webmining)
library(boot)

##################################################
## PART A
#Loading the data
data <- read.csv("ggplot2questions2016_17.csv",stringsAsFactors = FALSE)

#Creating a new variable to classify query as useful or not useful
data$useful <- as.factor(as.numeric(data$Score>=1))

#Nullifying the score column
data$Score <- NULL

#Exploratory data analysis - checking the number of useful and non-useful queries
table(data$useful)

##Starting to process the information
corpus_t <- Corpus(VectorSource((data$Title)))
strwrap(corpus_t[[1]])

corpus_t <- tm_map(corpus_t,tolower)
strwrap(corpus_t[[1]])

corpus_t <- tm_map(corpus_t, removePunctuation)
strwrap(corpus_t[[1]])

corpus_t <- tm_map(corpus_t,removeWords,c("ggplot2",stopwords("english")))
strwrap(corpus_t[[1]])

#corpus <- tm_map(corpus, removeWords, c("ggpl"))
#strwrap(corpus[[1]])

corpus_t <- tm_map(corpus_t, stemDocument)
strwrap(corpus_t[[1]])

freq <- DocumentTermMatrix(corpus_t)
freq

findFreqTerms(freq, lowfreq = 50)
findFreqTerms(freq, lowfreq = 20)

sparse <- removeSparseTerms(freq,0.95)
sparse$dimnames$Terms

new_df.title <- as.data.frame(as.matrix(sparse))
new_df.title

## Analyzing the body of the questions 

doc <- rep("abcd",7468)

for(i in 1:7468){
  text_b <- htmlParse(data$Body[i], asText = TRUE)
  text_b <- unlist(xpathApply(text_b,"//p",xmlValue))
  text_b <- gsub("//n"," ", text_b)
  for(j in 2:length(text_b)){
    text_b[1] = paste(text_b[1], " ", text_b[j])
  }
  doc[i] <- as.vector(text_b[1])
}
doc[1]
doc[2]

corpus_b <- Corpus(VectorSource((doc)))
strwrap(corpus_b[1])

corpus_b <- tm_map(corpus_b,tolower)
strwrap(corpus_b[1])

corpus_b <- tm_map(corpus_b, removePunctuation)
strwrap(corpus_b[1])

corpus_b <- tm_map(corpus_b,removeWords,c(stopwords("english")))
strwrap(corpus_b[1])

#corpus <- tm_map(corpus, removeWords, c("ggpl"))
#strwrap(corpus[[1]])

corpus_b <- tm_map(corpus_b, stemDocument)
strwrap(corpus_b[1])

freq_b <- DocumentTermMatrix(corpus_b)
freq_b

findFreqTerms(freq_b, lowfreq = 50)
findFreqTerms(freq_b, lowfreq = 20)
findFreqTerms(freq_b, lowfreq = 1000)

sparse_b <- removeSparseTerms(freq_b,0.95)
sparse_b$dimnames$Terms

#Creating a column for the analysis of the body of the questions
new_df.body <- as.data.frame(as.matrix(sparse_b))

#Combining the title and the body into a new dataframe and adding columns
new_df <- cbind(new_df.title,new_df.body)
colnames(new_df) <- make.names(colnames(new_df))

#Adding the useful column from the previous data into the new dataframe
new_df$useful <- data$useful

#############################################################
##PART B
##Splitting the dataset (The new one)
set.seed(100)
colnames(new_df) <- make.names(names(new_df))
splitting <- sample.split(new_df$useful, SplitRatio = 0.7)
df_train <- filter(new_df, splitting==TRUE)
df_test <- filter(new_df, splitting==FALSE)

#Checking if the split worked
base_train <- table(df_train$useful)
base_test <- table(df_test$useful)

## FIRST MODEL - LOGISTIC REGRESSION
logreg <- glm(useful ~ .,
              data = df_train, family = 'binomial')

summary(logreg)

logreg_final <- glm(useful ~ ggplot + chart + code + datafram + result + solut + exampl + error + axi.1 + order + help 
                    + ive + question + variabl + much + right,
              data = df_train, family = 'binomial')

summary(logreg_final)

predict_logreg <- predict(logreg_final, newdata = df_test, type = 'response')
logreg_conf <- table(df_test$useful, predict_logreg >0.75)
logreg_conf

logreg_acc <- sum(diag(logreg_conf))/sum(logreg_conf)
logreg_tpr <- 13/(13+1090)
logreg_fpr <- 7/(7+1130)
logreg_tnr <- 1130/(7+1130)


## SECOND MODEL - CART
cart1 <- train(useful ~ ., data = df_train,
               method = 'rpart',
               tuneGrid = data.frame(cp = seq(0,0.4, 0.002)),
               trControl = trainControl(method = 'cv',number = 10)
               )

cart1$results
cart_final <- cart1$finalModel
cart_pred <- predict(cart_final, newdata = df_test, type = 'class')
cart_conf <- table(df_test$useful, cart_pred)
cart_conf

cart_acc <- sum(diag(cart_conf))/sum(cart_conf)
cart_tpr <- 298/(298+805)
cart_fpr <- 238/(238+899)
cart_tnr <- 899/(899+238)

#ggplot(cart1$results, aes(x= cp, y= Accuracy))

## THIRD MODEL - RANDOM FORESTS

rf_train <- randomForest(useful ~ .,
                   data = df_train)
rf_pred <- predict(rf, newdata = df_test)
summary(rf_pred)

rf_conf <- table(df_test$useful, rf_pred)
rf_conf
rf_acc <- sum(diag(rf_conf))/sum(rf_conf)
rf_tpr <- 565/(565+538)
rf_fpr <- 448/(448+689)
rf_tnr <- 689/(689+448)

# Cross Validating the random forest model

rf_train_cv <- train(useful ~ .,
                     data = df_train,
                     method = 'rf',
                     tunegrid = data.frame(mtry = 1:148),
                     trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
                     )
rf_cv_final <- rf_train_cv$finalModel
rf_cv_pred <- predict(rf_cv_final, newdata = df_test)
rf_cv_conf <- table(df_test$useful, rf_cv_pred)
rf_cv_conf

rf_cv_acc <- sum(diag(rf_cv_conf))/sum(rf_cv_conf)
rf_cv_tpr <- 464/(464+639)
rf_cv_fpr <- 384/(384+753)

##Step Regression
log_step <- step(logreg, direction = 'backward')
summary(log_step)
length(log_step$coefficients)

log_step_predict <- predict(log_step, newdata = df_test, type='response')
log_step_predict_conf <- table(df_test$useful, log_step_predict > 0.75)
log_step_acc <- sum(diag(log_step_predict_conf))/sum(log_step_predict_conf)

## Bootstrapping

big_B = 10000
set.seed(342)
boot_all_metrics <- function(data, index) {
  acc = boot_accuracy(data, index)
  tpr = boot_tpr(data, index)
  fpr = boot_fpr(data, index)
  return(c(acc, tpr, fpr))
}
boot_accuracy <- function(data, index) {
  labels <- data$label[index]
  predictions <- data$prediction[index]
  return(tableAccuracy(labels, predictions))
}

boot_tpr <- function(data, index) {
  labels <- data$label[index]
  predictions <- data$prediction[index]
  return(tableTPR(labels, predictions))
}

boot_fpr <- function(data, index) {
  labels <- data$label[index]
  predictions <- data$prediction[index]
  return(tableFPR(labels, predictions))
}

tableAccuracy <- function(label, pred) {
  t = table(label, pred)
  a = sum(diag(t))/length(label)
  return(a)
}

tableTPR <- function(label, pred) {
  t = table(label, pred)
  return(t[2,2]/(t[2,1] + t[2,2]))
}

tableFPR <- function(label, pred) {
  t = table(label, pred)
  return(t[1,2]/(t[1,1] + t[1,2]))
}

# Log Regression
log_df = data.frame(labels = df_test$useful, predictions =  log_step_predict > 0.5)
Log_boot = boot(log_df, boot_all_metrics, R = big_B)
Log_boot
boot.ci(Log_boot, index = 1, type = "basic")
boot.ci(Log_boot, index = 2, type = "basic")
boot.ci(Log_boot, index = 3, type = "basic")

#CART
cart_df = data.frame(labels = df_test$useful, predictions = cart_pred)
set.seed(3526)
CART_boot = boot(cart_df, boot_all_metrics, R = big_B)
CART_boot
boot.ci(CART_boot, index = 1, type = "basic")
boot.ci(CART_boot, index = 2, type = "basic")
boot.ci(CART_boot, index = 3, type = "basic")

#RF
rf_df = data.frame(labels = df_test$useful, predictions = rf_pred)
set.seed(6722)
RF_boot = boot(rf_df, boot_all_metrics, R = big_B)
RF_boot
boot.ci(RF_boot, index = 1, type = "basic")
boot.ci(RF_boot, index = 2, type = "basic")
boot.ci(RF_boot, index = 3, type = "basic")


