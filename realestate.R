
library(tidyverse) # Pipe operator (%>%) and other commands
library(leaps)     # Aid in calculating Cp, adjusted R-squared or R-squared
library(car)       # Muticolinearity detection (vif)
library(caret)     # Random split of data/cross validation
library(broom)     # Diagnostic Metric Table (augment)
library(olsrr)     # Heteroscedasticity Testing (ols_test_score)
library(ggplot2)   # Creates complex plots from data in a data frame. It provides a more programmatic interface
library(GGally)    # A plotting system based on the grammar of graphics it extends 'ggplot2' 
library(ggmap)     # Enables such visualization by combining the spatial information of static maps from Google Maps
library(glmnet)    # Lasso, Ridge and Elastic net
library(Hmisc)

 #reading the file 
realestate <- read.csv(file.choose(), stringsAsFactors = 2)

# Inspection of top 6-rows of realestate dataset
head(realestate)

# Inspection of bottom 6-rows of realestate dataset
tail(realestate)

#dropping the first column
realestate <- realestate[,-c(1)]

#renaming the columns 
colnames(realestate) <- c("TD", "HSEAGE", "DIST_TO_MRT", "NO_OF_STR", "LAT", "LONG", "HSEPRICE")

# Getting Structure of whole data set
str(realestate)

summary(realestate)

 #??as.date
 # data$X1transactiondate = lubridate::as_date(data$X1transactiondate)

#Checking for outliers 
boxplot(realestate)

#Removing outliers
#Outliers were completely eliminated after the 6th iteration in this variable
realestate <- realestate[-which(realestate$DIST_TO_MRT %in% boxplot.stats(realestate$DIST_TO_MRT) $out),]

realestate <- realestate[-which(realestate$HSEPRICE %in% boxplot.stats(realestate$HSEPRICE) $out),]
realestate <- realestate[-which(realestate$LONG %in% boxplot.stats(realestate$LONG) $out),]

## Again Checking Outliers
boxplot(realestate)

#Normal Distribution For Most Of the Variables
hist(realestate$DIST_TO_MRT)
hist(realestate$HSEPRICE)
hist(realestate)

# Creating scatter plot matrix 
pairs(realestate, upper.panel = NULL)

plot(realestate$TD, realestate$HSEPRICE)


# Plots to see the relationships of location based on different variables 
# There are some cluster (neighborhoods), with houses with similar price range
qmplot(LONG, LAT, data = realestate, colour = HSEPRICE, main = "Price By Location")

#It's seen that the transaction date has nothing to do with the location.
qmplot(LONG, LAT, data = realestate, colour = TD, main = "Transaction date By Location")

# There are some spots with houses with similar ages and in the same location.
qmplot(LONG, LAT, data = realestate, colour = HSEAGE, main = "House Age By Location")

# The distance to the nearest MRT station is highly related with the location.
qmplot(LONG, LAT, data = realestate, colour = DIST_TO_MRT, main = "Distance To MRT Station By Location")

# The number of convenience stores is highly related with the location.
qmplot(LONG, LAT, data = realestate, colour = NO_OF_STR, main = "Number Of Convinience Stores By Location")

#Split the data into training and test set
set.seed(42)
training.samples <- realestate$HSEPRICE
train.data  <- realestate[training.samples, ]
test.data <- read.csv(file.choose(), stringsAsFactors = 2)

#Inspection of bottom 6-rows of test dataset
head(test.data)

#Inspection of bottom 6-rows of test dataset
tail(test.data)

#Dropping the first column of the test dataset
test.data <- test.data[,-c(1)]

#Renaming the columns names of the test data set so that they are similiar to the training dataset(realestate)
colnames(test.data) <- c("TD", "HSEAGE", "DIST_TO_MRT", "NO_OF_STR", "LAT", "LONG")

#Getting the structure of the test dataset
str(test.data)

#Fitting a base model to check for significant and insignificant variables 
basemodel <- lm(HSEPRICE~., data = train.data)
summary(basemodel)

#Backward Elimination
full <- lm(HSEPRICE~., data = train.data)
drop1(full, test = "F")
drop1(update(full, ~. -LONG), test = "F")
drop1(update(full, ~. -LONG-NO_OF_STR), test = "F")

#This type of feature of selection suggests that no of stores is not significant but i feel it is so i added other models
#the model after backward elimination
model1 <- lm(HSEPRICE ~ HSEAGE+DIST_TO_MRT+LAT+TD, data = train.data)
summary(model1)

  #Created model is statistically significant since p-value < 0.05
  #Residual standard error for the model is 4.621
  #The coefficients (slope and intercept) are statistically significant since p-value < 0.05
  #This model explains approximately 69.57% variability of target (housprice)

model2 <- lm(HSEPRICE ~ HSEAGE+DIST_TO_MRT+LAT+TD+NO_OF_STR, data = train.data)
summary(model2)

  #Created model is statistically significant since p-value < 0.05
  #Residual standard error for the model is 4.629
  #The coefficients (slope and intercept) are statistically significant since p-value < 0.05 except for NO_OF_STR
  #This model explains approximately 69.46% variability of target (houseprice)

model3 <- lm(HSEPRICE ~ HSEAGE+DIST_TO_MRT+LAT+NO_OF_STR, data = train.data)
summary(model3)

  #Created model is statistically significant since p-value < 0.05
  #Residual standard error for the model is 5.216
  #The coefficients (slope and intercept) are statistically significant since p-value < 0.05 except for NO_OF_STR
  #This model explains approximately 61.22% variability of target (houseprice)

#Performing ANOVA to check for the significance in change of adjusted R-Squared
anova(model1, model2)
  #p-value for testing null hypothesis. Since this value is extremely higher than 0.05,
  #That's why the Adjusted R-squared is not statistically significant

anova(model1, model3)
  #no p-value 

# Score Test for Heteroskedasticity
ols_test_score(model1)
ols_test_score(model2)
   
  #p-value is less than the significance level 0.05. for both models.
  #Hence, we may reject the null hypothesis and conclude that the variance is not homogeneous. 

ols_test_score(model3)
 
  #p-value is greater than the significance level 0.05. 
  #Hence, we may accept the null hypothesis and conclude that the variance is homogeneous. i.e., Homoscedasticity

# Checking effect of Auto-correlation
durbinWatsonTest(model1)
durbinWatsonTest(model2)
durbinWatsonTest(model3)

 #p-value > 0.05 for all the models, Hence, we may accept the null hypothesis and 
 #conclude that there is no autocorrelation between errors. i.e., Errors are uncorrelated.

#Detecting Multicolinearity
vif(model1)
vif(model2)
vif(model3)

  #Variance inflation factor for all predictor variables are less than 5 (as a rule of thumb), In all the models 
  #Hence there is no multicolinearity between predictors.

#Checking Normality of Errors
shapiro.test(model1$residuals)
shapiro.test(model2$residuals)
shapiro.test(model3$residuals)

#Normality does not hold since p-value < 0.05 for all the models

# Plotting Histogram for Residuals
hist(model1$residuals)
hist(model2$residuals)
hist(model3$residuals)

   #There is some problem with right tail

#manual forward selection
null <- lm(HSEPRICE~1, data = train.data)
add1(null, scope = full, test = "F")
add1(update(null, ~. +TD), scope = full, test = "F")
add1(update(null, ~. +TD+LONG), scope = full, test = "F")
add1(update(null, ~. +TD+LONG+HSEAGE), scope = full, test = "F")
add1(update(null, ~. +TD+LONG+HSEAGE+DIST_TO_MRT), scope = full, test = "F")
add1(update(null, ~. +TD+LONG+HSEAGE+DIST_TO_MRT+LAT), scope = full, test = "F")

#the model after forward selection
model4 <- lm(HSEPRICE ~ TD+LONG+HSEAGE+DIST_TO_MRT+LAT, data = train.data)
summary(model4)

  #Created model is statistically significant since p-value < 0.05
  #Residual standard error for the model is 4.608
  #The coefficients (slope and intercept) are statistically significant since p-value < 0.05 except for LONG
  #This model explains approximately 69.74% variability of target (housprice)

  #Removing the insignificant variable(LONG)  we get back to model 1

#Performing ANOVA to check for the significance in change of adjusted R-Squared
anova(model1, model4)

  #p-value for testing null hypothesis. Since this value is higher than 0.05,
  #That's why the Adjusted R-squared is not statistically significant

# Score Test for Heteroskedasticity
ols_test_score(model4)

  #p-value is less than the significance level 0.05.
  #Hence, we may reject the null hypothesis and conclude that the variance is not homogeneous. 

# Checking effect of Auto-correlation
durbinWatsonTest(model4)
 
  #p-value > 0.05, Hence, we may accept the null hypothesis and 
  #conclude that there is no autocorrelation between errors. i.e., Errors are uncorrelated.

#Detecting Multicolinearity
vif(model4)

  #variance inflation factor for all predictor variables are less than 5 (as a rule of thumb) 
  #Hence there is no multicolinearity between predictors.

#Checking Normality of Errors
shapiro.test(model1$residuals)

  #Normality does not hold since p-value < 0.05 for all the models

# Plotting Histogram for Residuals
hist(model1$residuals)

  #We see that there is some problem with right tail


#Stepwise selection
step(full, data = train.data, direction = "backward")

  #This method suggests this model
step_backward <- lm(HSEPRICE ~ TD + HSEAGE + DIST_TO_MRT + LAT + LONG, data = train.data)
summary(step_backward)
  #In this model LONG is insignifcant
  #Implying (model1) if we remove the insignificant variable

step(full, data = data, direction = "forward")
  
  #This method suggests this model
step_forward<- lm(HSEPRICE ~ TD + HSEAGE + DIST_TO_MRT + NO_OF_STR + LAT + LONG, data = train.data)
summary(step_forward)
  #This model too traces back to (model1) after removing the insignificant variables 

step(full, data = data, direction = "both")

  #This method suggests this model
step_both <- lm(HSEPRICE ~ TD + HSEAGE + DIST_TO_MRT + LAT + LONG, data = train.data)
summary(step_both)
   #In this model LONG is insignificant.
   #Implying (model1) if we remove the insignificant variable


#Fitting polynomial models to model 1, 2 and 3
model5 <- lm(HSEPRICE ~ poly(HSEAGE,2)+poly(DIST_TO_MRT,2)+poly(TD,2)+poly(LAT,2)+HSEAGE:DIST_TO_MRT:TD:LAT, data = train.data)
summary(model5)

model5a <- lm(HSEPRICE ~ poly(HSEAGE,3)+poly(DIST_TO_MRT,3)+poly(TD,3)+poly(LAT,3)+HSEAGE:DIST_TO_MRT:TD:LAT, data = train.data)
summary(model5a)

model5b <- lm(HSEPRICE ~ poly(HSEAGE,4)+poly(DIST_TO_MRT,4)+poly(TD,4)+poly(LAT,4)+HSEAGE:DIST_TO_MRT:TD:LAT, data = train.data)
summary(model5b)

model6 <- lm(HSEPRICE ~ poly(HSEAGE,3)+poly(DIST_TO_MRT,3)+poly(NO_OF_STR,3)+poly(LAT,3)+HSEAGE:DIST_TO_MRT:NO_OF_STR:LAT, data = train.data)
summary(model6)

model6a <- lm(HSEPRICE ~ poly(HSEAGE)+poly(DIST_TO_MRT,2)+poly(NO_OF_STR,2)+poly(LAT,2)+HSEAGE:DIST_TO_MRT:NO_OF_STR:LAT, data = train.data)
summary(model6a)


model7 <- lm(HSEPRICE ~ poly(HSEAGE,2)+poly(DIST_TO_MRT,2)+poly(NO_OF_STR,2)+poly(LAT,2)+poly(TD,2)+HSEAGE:DIST_TO_MRT:NO_OF_STR:LAT, data = train.data)
summary(model7)



anova(model1, model3)

ridge <- train(HSEPRICE ~., train.data, method = 'glmnet', tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, length = 5)))
plot(ridge)
ridge

lasso <- train(HSEPRICE ~., train.data, method = 'glmnet', tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 5)))
plot(lasso)
lasso

p1 <- predict(ridge, test.data)
p1

p2 <- predict(lasso, test.data)
p2



