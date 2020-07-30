model1 <- vix.open ~ timestamp + category + open + high + low + close + previous + 
  vix.previous + confirmed.total + confirmed.previous + 
  confirmed.change.percent + deaths.total + deaths.previous + 
  deaths.change.percent
  
model2 <- vix.close ~ timestamp + category + open + high + low + close + previous + 
  vix.previous + confirmed.total + confirmed.previous + 
  confirmed.change.percent + deaths.total + deaths.previous + 
  deaths.change.percent

model3 <- vix.change ~ timestamp + category + open + high + low + close + previous + 
  vix.previous + confirmed.total + confirmed.previous + 
  confirmed.change.percent + deaths.total + deaths.previous + 
  deaths.change.percent

# Serial Correlation Visual Inspection
dailymodel3 <- read.csv("covidandmarkets.csv", header=T, na.strings="?")
str(dailymodel3)
dailymodel3 <- dailymodel3 %>%
  mutate(vix.change = vix.close - vix.previous,
         timestamp = as.Date(timestamp),
         category = as.factor(category),
         confirmed.previous = as.numeric(confirmed.total),
         confirmed.change.percent = as.numeric(confirmed.change.percent),
         deaths.previous = as.numeric(deaths.previous),
         deaths.change.percent = as.numeric(deaths.change.percent)) %>%
  select(timestamp, vix.change, category, open, high, low, close, previous,  
           vix.previous, confirmed.total, confirmed.previous, 
           confirmed.change.percent, deaths.total, deaths.previous, 
           deaths.change.percent) %>%
  na.omit()
  

daily3.diagnostics <- lm(vix.change ~ timestamp + category + open + high + low + close + 
                          previous + vix.previous + confirmed.total + confirmed.previous + 
                          confirmed.change.percent + deaths.total + deaths.previous + 
                          deaths.change.percent, data=dailymodel3)
summary(daily3.diagnostics)
plot(dailymodel3$timestamp, daily3.diagnostics$residuals) # Plot residuals against ordered time T
abline(0, 0, col="red") # Red line with intercept and slope equal to 0

# Durbin-Watson Test for Serial Correlation 
#Testing for Serial Correlation
library(lmtest) # Activate the package
summary(daily3.diagnostics)
dwtest(daily3.diagnostics) #2.3806 which is considered okay

# Multivariate Time Series Models
library(DataCombine) # Contains the slide() function
dailymodel3 <- slide(dailymodel3, Var="KUnits", NewVar="KUnits.L1", slideBy = -1)
dailymodel3 <- slide(dailymodel3, Var="KUnits", NewVar="KUnits.L2", slideBy = -2)
dailymodel3 <- slide(dailymodel3, Var="KUnits", NewVar="KUnits.L3", slideBy = -3)
dailymodel3 <- slide(dailymodel3, Var="KUnits", NewVar="KUnits.L4", slideBy = -4)
lm.KUnits.all <- lm(vix.change~T+S.P+Q2+Q3+Q4+
                      KUnits.L1+KUnits.L2+KUnits.L3+KUnits.L4, data=dailymodel3)
summary(lm.KUnits.all)
dwtest(lm.KUnits.all);

# Stepwise Selection w/step() Function
library(ISLR) # Contains the Hitters dataset
dailymodel3.null <- lm(vix.change ~ 1, data=dailymodel3) # Fit the Null (or reduced) model
dailymodel3.full <- lm(vix.change ~., data=dailymodel3) # Fit the Full (or large) model
# Stepwise can start with the Null model and proceed forward in both directions - NOT preferred
# Or, start with the Full model and proceed backwards in both directions - Preferred:
dailymodel3.step.backward <- step(dailymodel3.full, #(Starting with the Full model in this case)
                              scope=list(lower=dailymodel3.null, upper=dailymodel3.full), direction="both", test="F")
summary(dailymodel3.step.backward)

### Multicollinearity Testing
library(perturb) # Contains the colldiag( ) function to compute the CI
library(car) # Contains the vif( ) function to compute VIF’s
library(MASS) # Contains the Boston housing market data set
lm.fit <- lm(vix.change ~ ., data=dailymodel3) # Full model with all predictors
summary(lm.fit)
colldiag(mod=lm.fit, scale=F, center=F, add.intercept=T) # Try scale and center = T
vif(lm.fit)
lm.fit.red <- lm(vix.change~timestamp+open+high+low+close+previous+vix.previous+
                   confirmed.total+confirmed.change.percent+deaths.total+
                   deaths.previous+deaths.change.percent, data=dailymodel3) # Reduced model
colldiag(mod=lm.fit.red, scale=F, center=F, add.intercept=T) # Try scale and center = T
vif(lm.fit.red)

##  The {caret} Package
library(caret)
set.seed(1) # Set the seed
# train() is a universal function in {caret} tat
lm.fit.caret <- train(vix.change~timestamp+open+high+low+close+previous+vix.previous+
                        confirmed.total+confirmed.change.percent+deaths.total+
                        deaths.previous+deaths.change.percent, 
                      data=dailymodel3, method = "lm") # Fit an lm() model
# The default cross validation is done with bootstrapping with 25 samples
lm.fit.caret # Reports RMSE and R squared
lm.fit.caret$results$RMSE # This is where RMSE is stored
lm.fit.caret$results$RMSE^2 # To get the MSE
summary(lm.fit.caret) # Same as lm() results – in fact, train() uses lm() internally
