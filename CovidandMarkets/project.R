# -------------
#PHASE 1 - FULL MODEL

#Full Model
#Even though R^2 and adjusted R^2 are high, this may not necessarily confirm the model being a favorable one. Also, R^2 and adjusted R^2 are good comparisons for models with same number of parameters. 
project <- model.data %>%
  select(-day)

project.full <- lm(vix.change ~ ., data=project)
summary(project.full)

#Main focus is to predict vix.change based on covid indicators but the subset choice below of variables differs from our main focus since it has chosen stock related variables. 

testsub<-regsubsets(vix.change ~ ., data=project)
bestsub<-summary(testsub)
bestsub

bestsub.data <- project %>%
  select(vix.change, open, high, low, previous, change.percent, vix.change.percent, confirmed.change.percent)

bestsub.model <- lm(vix.change ~ . , data = bestsub.data)
summary(bestsub.model)

anova(project.full, bestsub.model) # Fail to reject the full model

new.full<- model.data %>%
  select(vix.change, vix.previous, vix.change.percent, confirmed.total, confirmed.previous, confirmed.change.percent, deaths.total, deaths.previous, deaths.change.percent)
summary(new.full)

#Partial Regression Plots without stock related variable 

newfull.model <- lm(vix.change ~ . , data=new.full)
summary(newfull.model)

#Added-variable plot (partial regression plot)
#Reference line appears almost flat for most does not seem to be highly correlated with the response variable
avPlots(newfull.model)
avPlots(bestsub.model)

#DW=2.254 seems favorable
dwtest(project.full) #2.0285 seems favorable
dwtest(bestsub.model) #2.0734 seems favorable
dwtest(newfull.model) #2.254 seems favorable

#Condition Index (CI) and Variance Inflation Factor (VIF)
#There seems moderate correlation between predictors for VIX and volume. 
#Others are highly correlated
colldiag(project.full, scale=F, center=F, add.intercept=T)
colldiag(bestsub.model, scale=F, center=F, add.intercept=T)
vif(project.full)
vif(bestsub.model)

colldiag(newfull.model, scale=F, center=F, add.intercept=T)
vif(newfull.model)

bptest(project.full, data=project) # Breusch-Pagan test - Small p-value = Heteroskedastic
plot(project.full, which=1)

anova(project.full, bestsub.model)
# --------------
# PHASE 2 - REDUCED MODEL

head(project)
str(project)
par(mfrow=c(2,2))
hist(project$vix.change, col = "blue") 
hist(project$vix.change.percent, col = "blue") 
hist(project$confirmed.change.percent, col = "blue")
hist(project$deaths.change.percent, col = "blue")
ls(project)
summary(project)
par(mfrow=c(1,1)) 

model.data %>%
  ggplot(aes(x=confirmed.change.percent, y=vix.change.percent)) +
  geom_point()

mean(model.data$vix.change)
median(model.data$vix.change)
range(model.data$vix.change)
sd(model.data$vix.change)
var(model.data$vix.change)

mean(model.data$confirmed.change.percent)
median(model.data$confirmed.change.percent)
range(model.data$confirmed.change.percent)
sd(model.data$confirmed.change.percent)
var(model.data$confirmed.change.percent)

mean(model.data$deaths.change.percent)
median(model.data$deaths.change.percent)
range(model.data$deaths.change.percent)
sd(model.data$deaths.change.percent)
var(model.data$deaths.change.percent)

mean(project$confirmed.total)
median(project$confirmed.total)
sd(project$confirmed.total)
var(project$confirmed.total)

# Testing for Heteroskedasticity
time.model <- lm(vix.change ~., data=model.data)
summary(time.model) # Fit the model using full model
bptest(time.model, data=model.data) # Breusch-Pagan test - Small p-value = Heteroskedastic
plot(time.model, which=1) # First plot() renders the residual plot - slight indication of non-linearity and heteroskedasticity

# Correcting Heteroskedastic - Fitting WLS
time.model # Fit OLS regression
time.abs.res <- lm( abs(residuals(time.model)) ~ fitted(time.model)) # Regress absolute value of OLS residuals on fitted values 
wts <- 1/fitted(time.abs.res)^2 # Weight vector, inverse of 
time.wls <- lm(vix.change ~ ., data=model.data, weights=wts)
summary(time.wls) # More stable model (i.e., lower variance) than OLS, the coefficients are similar.
bptest(time.wls, data=model.data) # Breusch-Pagan test - Small p-value = Heteroskedastic
plot(time.wls, which=1)

# Serial Correlation Visual Inspection
plot(model.data$day, time.model$residuals, main="Residuals Against Day") # Plot residuals against ordered by day
abline(0, 0, col="red") # Red line with intercept and slope equal to 0

dwtest(time.model)

# Stepwise
time.step.lm <- lm(vix.change ~ 1, data = model.data)
time.step.backward <- step(time.model, scope=list(lower=time.step.lm, upper=time.model), direction = "both", test="F")

summary(time.step.backward)

anova(time.model, time.step.backward)

# ----------

# Correcting Heteroskedastic - Fitting WLS
project.full # Fit OLS regression
project.abs.res <- lm( abs(residuals(project.full)) ~ fitted(project.full)) # Regress absolute value of OLS residuals on fitted values 
wts <- 1/fitted(project.abs.res)^2 # Weight vector, inverse of 
project.wls <- lm(vix.change ~ ., data=project, weights=wts)
summary(project.wls) # More stable model (i.e., lower variance) than OLS, the coefficients are similar.
bptest(project.wls, data=model.data) # Breusch-Pagan test - Small p-value = Heteroskedastic
plot(lm.wls, which=1)

# Serial Correlation Visual Inspection
plot(model.data$day, full.lm$residuals) # Plot residuals against ordered time T
abline(0, 0, col="red") # Red line with intercept and slope equal to 0

# Stepwise
step.lm <- lm(vix.change ~ 1, data = project)
step.backward <- step(project.full, scope=list(lower=step.lm, upper=project.full), direction = "both", test="F")

summary(step.backward)

anova(project.full, bestsub.model, step.backward)

# --------------
# PHASE 3 - FURTHER TEST AND COMPARISON

# Regression Trees
tree.small <- tree(vix.change ~ ., data = model.data, mindev=0.1)
plot(tree.small)
text(tree.small, pretty=0)

tree.large <- tree(vix.change ~ ., data = model.data, mindev=0.005)
plot(tree.large)
text(tree.large, pretty=0)

RNGkind(sample.kind="default")
set.seed(1)
tree.vix <- tree(vix.change ~ ., data = model.data)

cv.tree <- cv.tree(tree.vix)
plot(cv.tree$size, cv.tree$dev, type='b')

cbind("Size"=cv.tree$size, "Deviance"=cv.tree$dev)

min.dev <- min(cv.tree$dev)
best.ind <- which(cv.tree$dev == min.dev)
best.size <- cv.tree$size[best.ind]

c("Min Dev"=min.dev, "Tree No."=best.ind, "Tree Size"=best.size)

prune.sal <- prune.tree(tree.vix, best=best.size)
plot(prune.sal) 
text(prune.sal, pretty=0)

# Bagging Tree Predictions
set.seed(1)
train <- sample(1:nrow(model.data), 0.9*nrow(model.data))

bag.vix <- randomForest(vix.change ~ ., data=model.data[train,], mtry=16, importance=T)
plot(bag.vix, main = "Bagging Tree Plot", xlim=c(0, 50))
bag.vix

varImpPlot(bag.vix)
importance(bag.vix)

bag.pred <- predict(bag.vix, newdata=model.data[-train,]) # Predict with the train model and test data
plot(bag.pred, model.data$vix.change[-train], xlab="Predicted", ylab="Actual", main="Bagging Actual vs. Predicted") # Plot predicted vs. actual
abline(0,1) # Draw a 45 degree line (intercept=0; slope=1)
mean((bag.pred-model.data$vix.change[-train])^2)

#RANDOM FOREST
set.seed(1)
bag.rf <- randomForest(vix.change ~ ., data=model.data[train,], mtry=7, importance=T)
plot(bag.rf, main = "RF Tree Plot", xlim=c(0, 50))
bag.rf

varImpPlot(bag.rf)
importance(bag.rf)

rf.pred <- predict(bag.rf,newdata=model.data[-train,]) # Predict with the train model and test data
plot(rf.pred , model.data$vix.change[-train], xlab="Predicted", ylab="Actual", main="RF Actual vs. Predicted") # Plot predicted vs. actual 
abline(0,1) # Draw a 45 degree line  (intercept=0; slope=1)
mean((rf.pred-model.data$vix.change[-train])^2)

bagrf<- cbind(importance(bag.vix),importance(bag.rf)) %>%
  as.data.frame()

names(bagrf)[1] <- "bagging.per.MSE"
names(bagrf)[2] <- "bagging.purity"
names(bagrf)[3] <- "rf.per.MSE"
names(bagrf)[4] <- "rf.purity"

bagrf

write.csv(bagrf, "bagrf.csv")

#Ridge Regression
# We need to eliminate with missing data
x <- model.matrix(vix.change~., data=model.data) [,-1]  # Predictor matrix, with intercept column removed
y <- model.data$vix.change # y vector with just the actual values of the response
ridge.mod <- glmnet(x, y, alpha=0) # alpha=0 - Ridge regressions for various values of lambda
# In glmnet() use family=“binomial” for logit; and family=“poisson” for count data
plot(ridge.mod, label=T) # Plot the L2 Norm (the diagram says L1, but for Ridge it’s called L2)

# Optimal Ridge Lambda
set.seed(1) # To get repeatable results
cv.10Fold <- cv.glmnet(x, y, alpha=0) # 10-Fold is the default
cbind("Lambda"=cv.10Fold$lambda, "10-Fold MSE"=cv.10Fold$cvm)
plot(cv.10Fold) # Plot all lambdas vs. MSEs
best.lambda <- cv.10Fold$lambda.min # Best lambda and log(lambda)
min.mse <- min(cv.10Fold$cvm) # Lowest 10FCV MSE
cbind("Best Lambda"=best.lambda, "Log(Lambda)"=log(best.lambda), "Best 10FCV MSE"=min.mse)

# Predicting with Ridge
ridge.coeff <- predict(ridge.mod, s=best.lambda, type="coefficients") # Display Ridge coefficients for lambda=s
ridge.coeff
set.seed(1)
test <- sample(1:nrow(x), 0.10*nrow(x)) # 10% test data, for prediction example
ridge.pred <- predict(ridge.mod, s=best.lambda, newx=x[test,]) # Or any x you wish predictions for x
ridge.pred # Take a look

#LASSO
lasso.mod <- glmnet(x, y, alpha=1) # alpha=1 - LASSO regressions for various values of lambda
# In glmnet() use family=“binomial” for logit; and family=“poisson” for count data
plot(lasso.mod) # Plot the L1 Norm

# Optimal LASSO Lambda
set.seed(1) # To get repeatable results
cv.10Fold.lasso <- cv.glmnet(x,y,alpha=1) # 10-Fold is the default
cbind("Lambda"=cv.10Fold.lasso$lambda, "10-Fold MSE"=cv.10Fold.lasso$cvm)
plot(cv.10Fold.lasso) # Plot all lambdas vs. MSEs
best.lambda.lasso <- cv.10Fold.lasso$lambda.min # Best lambda 
log(best.lambda.lasso) # Best log(lambda)
min.mse.lasso <- min(cv.10Fold.lasso$cvm) # Lowest 10FCV MSE
cbind("Best Lambda"=best.lambda.lasso,"Log(Lambda)"=log(best.lambda.lasso),
      "Best 10FCV MSE"=min.mse.lasso)

# Predicting with LASSO
# Listed Ridge results as a reference
lasso.coeff <- predict(lasso.mod, s=best.lambda.lasso, type="coefficients") # Display LASSO coefficients for lambda=s
lasso.coeff
set.seed(1)
test <- sample(1:nrow(x), 0.10*nrow(x)) # 10% test data, for prediction example
lasso.pred <- predict(lasso.mod, s=best.lambda, newx=x[test,]) # Or, any x you want predictions for
lasso.pred # Take a look

coeff <- cbind("ridge coeff"=ridge.coeff, "lasso coeff"=lasso.coeff)
coeff <- coeff %>%
  as.data.frame()

ridgelasso.predict <- cbind("ridge"=ridge.pred, "lasso"=lasso.pred)
ridgelasso.predict

rownumbers <- c(68,39,1,34,87,43,14,82,9,51,97)
vix.result <- model.data[rownumbers,1]
vix.result
test.data <- model.data[rownumbers,2:17]
test.data

time.results <- predict(time.model, newdata = test.data)
time.results

full.results <- predict(project.full, newdata = test.data)
full.results

bestsub.results <- predict(bestsub.model, newdata = test.data)
bestsub.results

pred.results <- cbind(full.results, bestsub.results, ridge.pred, lasso.pred, time.results, vix.result) %>%
  as.data.frame()
pred.results

names(pred.results)[1] <- "full"
names(pred.results)[2] <- "bestsub"
names(pred.results)[3] <- "ridge"
names(pred.results)[4] <- "lasso"
names(pred.results)[5] <- "time"
pred.results

pred.results <- pred.results %>%
  mutate(fulldiff = (vix.result-full)^2,
         bestsubdiff = (vix.result-bestsub)^2,
         ridgediff = (vix.result-ridge)^2,
         lassodiff = (vix.result-lasso)^2,
         timediff = (vix.result-time)^2)
pred.results
pred.results$model <- colnames(pred.results[7:11])[apply(pred.results[7:11],1,which.min)]
pred.results

write.csv(pred.results, "pred.results.csv", row.names = FALSE)

mse.results <- c("full.mse"=mean(pred.results$fulldiff), "bestsub.mse"=mean(pred.results$bestsubdiff), "ridge.mse"=mean(pred.results$ridgediff), "lasso.mse"=mean(pred.results$lassodiff), "time.mse"=mean(pred.results$timediff))
mse.results <- mse.results %>%
  as.data.frame() %>%
  rename(c("mse"="."))
mse.results

write.csv(mse.results, "mse.results.csv")

# ---------- 
# Training and Bagging
# The following command forces the "class" of the data set to be a data frame.
# This can eliminate potential errors with how some functions will interpret objects of other classes
daily <- as.data.frame(model.data)

# The set.seed function sets the starting point for random number generation
# When using the same seed, random processes will always produce the same output
# It's useful for being able to reproduce your results
set.seed(12345)

# The following command will randomly select 70% of the row numbers in the data set to represent the training data
training <- sample(1:nrow(daily), 0.9*nrow(daily))

# The following line avoids having to change the code manually for data sets with different numbers of columns
nvars <- ncol(daily)

# The following two commands separate the training data into two objects; one has vix.change removed, the other contains only vix.change
daily.training <- daily[training,-nvars]
daily.training.results <- daily[training,nvars]

# The following two commands do the same for the remaining 30% of the data
daily.test <- daily[-training,-nvars]
daily.test.results <- daily[-training,nvars]

# ----------
##  The {caret} Package
library(caret)
set.seed(1) # Set the seed
# train() is a universal function in {caret} tat
lm.fit.caret <- train(vix.change ~ previous + volume + confirmed.total + confirmed.previous + confirmed.change.percent + deaths.total + deaths.previous + deaths.change.percent, data=project, method = "lm") # Fit an lm() model
# The default cross validation is done with bootstrapping with 25 samples
lm.fit.caret # Reports RMSE and R squared
lm.fit.caret$results$RMSE # This is where RMSE is stored
lm.fit.caret$results$RMSE^2 # To get the MSE
summary(lm.fit.caret) # Same as lm() results – in fact, train() uses lm() internally


#------------------------------------------------
# BAGGING
library(tree)

# Bagging parameters
bag.proportion <- 0.3 #proportion of training set used for each tree
bag.numtrees <- 25 #number of trees
bag.mindev <- -1  #controls the size of the trees (higher mindev -> smaller trees)

# Empty lists of trees & predictions that will be populated during the bagging process
bag.trees <- vector(mode="list",length=bag.numtrees) #creates the empty list of trees
bag.predictions <- vector(mode="list",length=bag.numtrees) #creates the empty list of prediction vectors
bagged.predictions <- 0

# The following for loop creates the trees using the model.data variables
for (i in 1:bag.numtrees){
  set.seed(12345+i) #if we used 12345 every time, we wouldn't get different samples from the training set
  daily.subset <- daily[sample(training,bag.proportion*length(training)),] #selects a random subset of the training set
  bag.trees[[i]] <- tree(vix.change ~ .,
                         data=project, mindev=bag.mindev)
  bag.predictions[[i]] <- predict(bag.trees[[i]],daily)[-training]
  bagged.predictions <- bagged.predictions + bag.predictions[[i]] #Keeps a running total of the predictions of the test set
}
bagged.predictions = bagged.predictions / bag.numtrees #divides the totals by the # of trees to get the average predictions for the test set
(mean((daily.test.results-bagged.predictions)^2))^0.5 #computes RMSE

##CHECK THIS -----------------------------
daily <- as.data.frame(model.data)
set.seed(12345)
training <- sample(1:nrow(daily), 0.9*nrow(daily))

daily.training <- daily[training,-1]
daily.training.results <- daily[training,1]

daily.test <- daily[-training,-1]
daily.test.results <- daily[-training,1]

best.mindev <- -1
RMSE <- -1
best.RMSE <- 99999999

for (i in 1:100) {
  daily.tree <- tree(vix.change ~ ., data=daily[training,], mindev=0.0005*i)
  daily.tree.predictions <- predict(daily.tree,daily)[-training]
  RMSE <- (mean((daily.test.results-daily.tree.predictions)^2))^0.5
  if (RMSE < best.RMSE) {
    best.mindev <- 0.0005*i
    best.RMSE <- RMSE
  }
}
print(paste("The optimal value of mindev is",best.mindev,"with a RMSE of",best.RMSE))

daily.best.tree <- tree(vix.change ~ ., data=model.[training,], mindev=best.mindev)
plot(daily.best.tree)
text(daily.best.tree, cex=0.7)
predict(daily.best.tree,test.data)
