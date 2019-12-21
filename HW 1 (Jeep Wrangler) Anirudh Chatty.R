
install.packages(c("dplyr", "ggplot2", "GGally","car"))

library(dplyr)
library(ggplot2)
library(GGally)
library(car) # for VIF

# Load dataset
jeep <- read.csv("Wrangler242-Spring2019.csv") 

str(jeep)
head(jeep)


# split into training and test sets 

jeep.train <- filter(jeep,Year <= 2015) 
head(jeep.train)

jeep.test <- filter(jeep, Year > 2015)
head(jeep.test)


###################################################################
# QUESTION 3 
## PART A

# train the model usiung the 4 variables mentioned in part A
#lm(y~x1+x2+...,data)
mod1 <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.Energy + CPI.All, 
           data = jeep.train)
summary(mod1)

vif(mod1)

# Eliminating variables that have high collinearity
# Eliminating Unemployement

mod2 <- lm(WranglerSales ~ WranglerQueries + CPI.Energy + CPI.All, 
           data = jeep.train)
summary(mod2)
vif(mod2)

# Eliminating CPI.All

mod3 <- lm(WranglerSales ~ WranglerQueries + CPI.Energy, 
            data = jeep.train)
summary(mod3)
vif(mod3)


##############################################
# PART B
## Redoing part a by including seasonality ( i.e. MonthFactor feature) into the regression model

modb <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.Energy + CPI.All + MonthFactor, 
           data = jeep.train)
summary(modb)

vif(modb)



#############################################
# PART C
##  BUILDING A FINAL REGRESSION MODEL USING PARTS A AND B TO FIND SIGNIFICANT VARIABLES 

# Eliminating CPI.All from part B

mod_final <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.Energy + MonthFactor, 
           data = jeep.train)
summary(mod_final)
vif(mod_final)

# Eliminating WranglerQueries

mod_final <- lm(WranglerSales ~ Unemployment + CPI.Energy + MonthFactor, 
           data = jeep.train)
summary(mod_final)
vif(mod_final)

# Calculating the SSE, SST and OSR2 using the final regression equation

jeep_Predictions <- predict(mod_final, newdata=jeep.test)
SSE = sum((jeep.test$WranglerSales - jeep_Predictions)^2)
SST = sum((jeep.test$WranglerSales - mean(jeep.train$WranglerSales))^2)
OSR2 = 1 - SSE/SST


#################################################
# PART D

#Reading the New Data
NewData <- read.csv("Toyota Data.csv") 

#Splitting the new data into test and training sets

NewData.train <- filter(NewData,Year <= 2015) 
head(NewData.train)

NewData.test <- filter(NewData, Year > 2015,!is.na(ToyotaSales))
head(NewData.test)

#Running the new Data using the models created in parts A and B

modT <- lm(ToyotaSales ~ Unemployment + ToyotaQueries + CPI.Energy + CPI.All + MonthFactor, 
           data = NewData.train)
summary(modT)

vif(modT)

# Eliminating features in order to find the best models


modT2 <- lm(ToyotaSales ~ ToyotaQueries + CPI.Energy + CPI.All + MonthFactor, 
           data = NewData.train)
summary(modT2)

vif(modT2)


#Calculating the SSE,SST and the OSR2 for the new data using the final model

Toyota_Predictions <- predict(modT2, newdata=NewData.test)
SSE = sum((NewData.test$ToyotaSales - Toyota_Predictions)^2)
SST = sum((NewData.test$ToyotaSales - mean(NewData.train$ToyotaSales))^2)
OSR2 = 1 - SSE/SST

###################################################
# PART E
#Calcualting the sales for both the Wrangler as well as the Prius fpr January using the model and with data collected online

j_p <- predict(mod_final, data.frame(Unemployment = 4,  CPI.Energy = 209.5, MonthFactor = 'January'))
print(j_p)

t_p <- predict(modT2, data.frame(Unemployment = 4,  CPI.Energy = 209.5, CPI.All = 252.73, ToyotaQueries = 63, MonthFactor = 'January'))
print(t_p)


