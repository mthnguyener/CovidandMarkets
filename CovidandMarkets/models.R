df <- read.csv("covidandmarkets.csv") %>%
  mutate(timestamp = as.Date(timestamp), 
         result = as.factor(result),
         vix.result = as.factor(vix.result),
         company = as.factor(company),
         category = as.factor(category),
         confirmed.result = as.factor(confirmed.result),
         deaths.result = as.factor(deaths.result))

str(df)

#previous.rate = open/previous,
#vix.previous.rate = vix.open/vix.previous
#previous.confirmed.rate = confirmed.total/previous.confirmed
#previous.deaths.rate = deaths.total/previous.deaths
model.data <- df %>%
  filter(company == "apple") %>%
  mutate(change.percent = (close-previous)/previous, 
         vix.change = vix.close - vix.previous,
         vix.change.percent = (vix.close - vix.previous)/vix.previous,
         confirmed.change.percent = (confirmed.total - confirmed.previous)/confirmed.previous,
         deaths.change.percent = (deaths.total - deaths.previous)/deaths.previous) %>%
  na.omit() %>%
  select(vix.change, open, high, low, close, previous, change.percent, 
         volume, vix.previous, vix.change.percent, confirmed.total, confirmed.previous, 
         confirmed.change.percent, deaths.total, deaths.previous, deaths.change.percent)

str(model.data)

model.data <- model.data[-1,]

model.data <- model.data %>%
  mutate(day=1:115) %>%
  select(vix.change, day, open:deaths.change.percent)

write.csv(model.data, "apple.csv",  row.names = FALSE)

#------
#Select Criteria
selcri<-function(lmout)
{
  n <- length(lmout$fit)
  rsq <- summary(lmout)$r.sq
  adj.rsq <- summary(lmout)$adj.r.sq
  aic <- extractAIC(lmout)[2]
  bic <- extractAIC(lmout, k = log(n))[2]
  press <- sum((lmout$residuals/(1 - hatvalues(lmout)))^2)
  cbind(rsq, adj.rsq, aic, bic, press)
}

#---------
#VIX OPEN (Model 1 - assumption)
model1.lm <- lm(vix.open ~ timestamp + category + open + high + low + close + previous + 
                  vix.previous + confirmed.total + confirmed.previous + 
                  confirmed.change.percent + deaths.total + deaths.previous + 
                  deaths.change.percent, 
                 data = model.data)
summary(model1.lm)

#-----------
#VIX CLOSE (Model 2 - assumption)
weekly2.lm <- lm(vix.close ~ timestamp + category + open + high + low + close + 
                   previous + vix.previous + confirmed.total + confirmed.previous + 
                   confirmed.change.percent + deaths.total + deaths.previous + 
                   deaths.change.percent, 
                 data = model.data)
summary(weekly2.lm)


#------
#VIX CHANGE (Model 3 - assumption)
model3.lm <- lm(vix.change ~ . - day, data = model.data)
summary(model3.lm)

# Testing for Heteroskedasticity (Pred 3)
library(MASS)
library(lmtest) # Contains bptest( ) and more
daily.ols <- model3.lm # Fit the model using all predictors for medv
bptest(daily.ols, data=model.data) # Breusch-Pagan test - Small p-value = Heteroskedastic
plot(daily.ols, which=1) # First plot() renders the residual plot

# Correcting Heteroskedastic - Fitting WLS (Pred 3)
daily.ols <- model3.lm # Fit OLS regression
daily.abs.res <- lm( abs(residuals(daily.ols)) ~ fitted(daily.ols)) # Regress absolute value of OLS residuals on fitted values 
wts <- 1/fitted(daily.abs.res)^2 # Weight vector, inverse of predicted absolute value of residuals, squared
daily.wls <- lm(vix.change ~ .,data=model.data, weights=wts)
summary(daily.wls) # More stable model (i.e., lower variance) than OLS, the coefficients are similar.

bptest(daily.wls, data=weekly.model) # Breusch-Pagan test - Small p-value = Heteroskedastic
plot(lm.wls, which=1)

summary(daily.ols)
summary(daily.wls)

confint(daily.ols)
confint(daily.wls)

# Serial Correlation Visual Inspection
plot(model.data$day, model3.lm$residuals) # Plot residuals against ordered time T
abline(0, 0, col="red") # Red line with intercept and slope equal to 0

# Durbin-Watson Test for Serial Correlation 
#Testing for Serial Correlation
library(lmtest) # Activate the package
summary(model3.lm)
dwtest(model3.lm)

#Multi-Collinearity Testing
library(perturb)
library(car)
library(MASS)

colldiag(model3.lm, scale=F, center=F, add.intercept=T)
vif(model3.lm)

library(ISLR)
test.null <- lm(vix.change ~ 1, data = weekly.model)
test.step.backward <- step(model3.lm, scope=list(lower=test.null, upper=model3.lm), direction = "both", test="F")

summary(test.step.backward)

anova(model3.lm, test.step.backward)

#------- Training and Bagging
# The following command forces the "class" of the data set to be a data frame.
# This can eliminate potential errors with how some functions will interpret objects of other classes
daily <- as.data.frame(model.data)

# The set.seed function sets the starting point for random number generation
# When using the same seed, random processes will always produce the same output
# It's useful for being able to reproduce your results
set.seed(12345)

# The following command will randomly select 70% of the row numbers in the data set to represent the training data
training <- sample(1:nrow(daily), 0.7*nrow(daily))

# The following line avoids having to change the code manually for data sets with different numbers of columns
nvars <- ncol(daily)

# The following two commands separate the training data into two objects; one has vix.change removed, the other contains only vix.change
daily.training <- daily[training,-nvars]
daily.training.results <- daily[training,nvars]

# The following two commands do the same for the remaining 30% of the data
daily.test <- daily[-training,-nvars]
daily.test.results <- daily[-training,nvars]


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

# The following for loop creates the trees using the model.data variables
for (i in 1:bag.numtrees){
  set.seed(12345+i) #if we used 12345 every time, we wouldn't get different samples from the training set
  daily.subset <- daily[sample(training,bag.proportion*length(training)),] #selects a random subset of the training set
  bag.trees[[i]] <- tree(vix.change ~ day + open + high + low + close + 
                           previous + change.percent + volume + vix.previous + 
                           vix.change.percent + confirmed.total + confirmed.previous + 
                           confirmed.change.percent + deaths.total + deaths.previous + 
                           deaths.change.percent,
                         data=daily.subset, mindev=bag.mindev)
  bag.predictions[[i]] <- predict(bag.trees[[i]],daily)[-training]
  bagged.predictions <- bagged.predictions + bag.predictions[[i]] #Keeps a running total of the predictions of the test set
}
bagged.predictions = bagged.predictions / bag.numtrees #divides the totals by the # of trees to get the average predictions for the test set
(mean((daily.test.results-bagged.predictions)^2))^0.5 #computes RMSE

##CHECK THIS -----------------------------
daily <- as.data.frame(model.data)
names(salaries)[2]<-"Education"
set.seed(12345)
training <- sample(1:nrow(salaries), 0.6*nrow(salaries))

salaries.training <- salaries[training,-3]
salaries.training.results <- salaries[training,3]

salaries.test <- salaries[-training,-3]
salaries.test.results <- salaries[-training,3]

best.mindev <- -1
RMSE <- -1
best.RMSE <- 99999999

for (i in 1:100) {
  salaries.tree <- tree(Salary ~ Age + Education, data=salaries[training,], mindev=0.0005*i)
  salaries.tree.predictions <- predict(salaries.tree,salaries)[-training]
  RMSE <- (mean((salaries.test.results-salaries.tree.predictions)^2))^0.5
  if (RMSE < best.RMSE) {
    best.mindev <- 0.0005*i
    best.RMSE <- RMSE
  }
}
print(paste("The optimal value of mindev is",best.mindev,"with a RMSE of",best.RMSE))

salaries.best.tree <- tree(Salary ~ Age + Education, data=salaries[training,], mindev=best.mindev)
plot(salaries.best.tree)
text(salaries.best.tree, cex=0.7)
names(salariesNew)[2] <- "Education"
predict(salaries.best.tree,salariesNew)