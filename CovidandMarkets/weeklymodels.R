library(tidyverse)
library(dplyr)

dfweekly <- read.csv("weeklycovidandmarkets.csv") %>%
  mutate(result = as.factor(result),
         vix.result = as.factor(vix.result),
         company = as.factor(company),
         category = as.factor(category),
         confirmed.result = as.factor(confirmed.result),
         deaths.result = as.factor(deaths.result))

str(dfweekly)

weekly.model <- dfweekly %>%
  mutate(vix.change = vix.previous - vix.close) %>%
  na.omit() %>%
  select(vix.change, week, open, high, low, close, previous, change.percent, 
         volume, vix.previous, vix.change.percent, confirmed.total, confirmed.previous, 
         confirmed.change.percent, deaths.total, deaths.previous, deaths.change.percent)

str(weekly.model)

#VIX OPEN (FULL - unreliable)
weekly.lm <- lm(vix.open ~ ., data = weekly.model)
summary(weekly.lm)

#-----------------
#VIX OPEN (Model 1 - assumption)
weekly1.lm <- lm(vix.open ~ week + category + open + high + low + close + 
                   previous + change.percent + result + volume + vix.previous + 
                   confirmed.total + confirmed.previous + confirmed.change.percent + 
                   deaths.total + deaths.previous + deaths.change.percent, 
                data = weekly.model)
summary(weekly1.lm)

#------------
#VIX CLOSE (Model 2 - assumption)
weekly2.lm <- lm(vix.close ~ week + category + open + high + low + close + 
                   previous + change.percent + result + volume + vix.previous + 
                   confirmed.total + confirmed.previous + confirmed.change.percent + 
                   deaths.total + deaths.previous + deaths.change.percent, 
                 data = weekly.model)
summary(weekly2.lm)

#------------
#VIX CHANGE (Model 3 - assumption)
weekly3.lm <- lm(vix.change ~ week + open + high + low + close + 
                   previous + change.percent + volume + vix.previous + 
                   vix.change.percent + 
                   confirmed.total + confirmed.previous + 
                   confirmed.change.percent + deaths.total + deaths.previous + 
                   deaths.change.percent,
                 data = weekly.model)
summary(weekly3.lm)

# Testing for Heteroskedasticity
library(MASS) # Contains the “Boston" housing data set
library(lmtest) # Contains bptest( ) and more
lm.ols <- weekly3.lm # Fit the model using all predictors for medv
bptest(lm.ols, data=weekly.model) # Breusch-Pagan test - Small p-value = Heteroskedastic
plot(lm.ols, which=1) # First plot() renders the residual plot

# Correcting Heteroskedastic - Fitting WLS
lm.ols <- weekly3.lm # Fit OLS regression
lm.abs.res <- lm( abs(residuals(lm.ols)) ~ fitted(lm.ols) ) # Regress absolute value of OLS residuals on fitted values 
wts <- 1/fitted(lm.abs.res)^2 # Weight vector, inverse of predicted absolute value of residuals, squared
lm.wls <- lm(vix.change ~ .,data=weekly.model, weights=wts)
summary(lm.wls) # More stable model (i.e., lower variance) than OLS, the coefficients are similar.

lm.wls # Fit the model using all predictors for medv
bptest(lm.wls, data=weekly.model) # Breusch-Pagan test - Small p-value = Heteroskedastic
plot(lm.wls, which=1)

# Serial Correlation Visual Inspection
plot(weekly.model$week, weekly3.lm$residuals) # Plot residuals against ordered time T
abline(0, 0, col="red") # Red line with intercept and slope equal to 0

# Durbin-Watson Test for Serial Correlation 
#Testing for Serial Correlation
library(lmtest) # Activate the package
summary(weekly3.lm)
dwtest(weekly3.lm)

#Multi-Collinearity Testing
library(perturb)
library(car)
library(MASS)

colldiag(weekly3.lm, scale=F, center=F, add.intercept=T)
vif(weekly3.lm)

library(ISLR)
test.null <- glm(vix.change ~ 1, data = weekly.model)
test.step.backward <- step(weekly3.lm, scope=list(lower=test.null, upper=weekly3.lm), direction = "both", test="F")

summary(test.step.backward)

anova(weekly3.lm, test.step.backward)

#------- Train and Bagging
# The following command forces the "class" of the data set to be a data frame.
# This can eliminate potential errors with how some functions will interpret objects of other classes
weekly <- weekly.model %>%
  select(-company, -result, -vix.result, -confirmed.result, -deaths.result) %>%
  as.data.frame()

# The set.seed function sets the starting point for random number generation
# When using the same seed, random processes will always produce the same output
# It's useful for being able to reproduce your results
set.seed(12345)

# The following command will randomly select 60% of the row numbers in the data set to represent the training data
training <- sample(1:nrow(weekly), 0.6*nrow(weekly))

# The following line avoids having to change the code manually for data sets with different numbers of columns
nvars <- ncol(weekly)

# The following two commands separate the training data into two objects; one has interest rate removed, the other contains only interest rate
weekly.training <- weekly[training,-nvars]
weekly.training.results <- weekly[training,nvars]

# The following two commands do the same for the remaining 40% of the data
weekly.test <- weekly[-training,-nvars]
weekly.test.results <- weekly[-training,nvars]


#------------------------------------------------
# BAGGING
library(tree)

# Bagging parameters
bag.proportion <- 0.3 #proportion of training set used for each tree
bag.numtrees <- 25 #number of trees
bag.mindev <- 0.005 #controls the size of the trees (higher mindev -> smaller trees)

# Empty lists of trees & predictions that will be populated during the bagging process
bag.trees <- vector(mode="list",length=bag.numtrees) #creates the empty list of trees
bag.predictions <- vector(mode="list",length=bag.numtrees) #creates the empty list of prediction vectors
bagged.predictions <- 0

# The following for loop creates the trees using the Lending Club variables
for (i in 1:bag.numtrees){
  set.seed(12345+i) #if we used 12345 every time, we wouldn't get different samples from the training set
  weekly.subset <- weekly[sample(training,bag.proportion*length(training)),] #selects a random subset of the training set
  bag.trees[[i]] <- tree(vix.change ~ week + category + open + high + low + close + 
                           previous + change.percent + volume + vix.previous + 
                           confirmed.total + confirmed.previous + 
                           confirmed.change.percent + deaths.total + deaths.previous + 
                           deaths.change.percent, 
                         data=weekly.subset, mindev=bag.mindev)
  bag.predictions[[i]] <- predict(bag.trees[[i]],weekly)[-training]
  bagged.predictions <- bagged.predictions + bag.predictions[[i]] #Keeps a running total of the predictions of the test set
}
bagged.predictions = bagged.predictions / bag.numtrees #divides the totals by the # of trees to get the average predictions for the test set
(mean((weekly.test.results-bagged.predictions)^2))^0.5 #computes RMSE

#------------------------------------------------
# RANDOM TREES

# Random tree parameters
rt.vars <- 3 #number of independent variables used in each tree
rt.numtrees <- 25 #number of trees
rt.mindev <- 0.005 #controls the size of the trees (higher mindev -> smaller trees)

# Empty lists of trees & predictions that will be populated during the random trees process
rt.trees <- vector(mode="list",length=rt.numtrees) #creates the empty list of trees
rt.predictions <- vector(mode="list",length=rt.numtrees) #creates the empty list of prediction vectors
randomtree.predictions <- 0

# The following for loop creates the trees using the Lending Club variables
for (i in 1:rt.numtrees){
  set.seed(12345+i) #if we used 12345 every time, we wouldn't get different subsets of variables
  weekly.subset <- weekly[training,sample(1:(nvars-1),rt.vars)] #selects a random subset of the variables
  weekly.subset[,rt.vars+1] <- weekly.training.results
  names(weekly.subset)[rt.vars+1] = "vix.close" #this is necessary for the predict function to be able to match variables correctly
  rt.trees[[i]] <- tree(vix.open ~ week + category + open + high + low + close + 
                          previous + change.percent + result + volume + vix.previous + 
                          confirmed.total + confirmed.previous + 
                          confirmed.change.percent + deaths.total + deaths.previous + 
                          deaths.change.percent + deaths.previous, data=weekly.subset, mindev=rt.mindev) #include as many independent variables as are being used
  rt.predictions[[i]] <- predict(rt.trees[[i]],weekly)[-training]
  randomtree.predictions <- randomtree.predictions + rt.predictions[[i]] #Keeps a running total of the predictions of the test set
}
randomtree.predictions = randomtree.predictions / rt.numtrees #divides the totals by the # of trees to get the average predictions for the test set
(mean((weekly.test.results-randomtree.predictions)^2))^0.5 #computes RMSE
