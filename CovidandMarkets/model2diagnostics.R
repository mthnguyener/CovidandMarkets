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
dailymodel2 <- read.csv("covidandmarkets.csv", header=T, na.strings="?")
str(dailymodel2)
dailymodel2 <- dailymodel2 %>%
  mutate(timestamp = as.Date(timestamp),
         category = as.factor(category),
         confirmed.previous = as.numeric(confirmed.total),
         confirmed.change.percent = as.numeric(confirmed.change.percent),
         deaths.previous = as.numeric(deaths.previous),
         deaths.change.percent = as.numeric(deaths.change.percent)) %>%
  select(timestamp, vix.close, category, open, high, low, close, previous, 
           vix.previous, confirmed.total, confirmed.previous, 
           confirmed.change.percent, deaths.total, deaths.previous, 
           deaths.change.percent) %>%
  na.omit()


daily2.diagnostics <- lm(vix.close ~ category + open + high + low + close + previous + 
                          vix.previous + confirmed.total + confirmed.previous + 
                          confirmed.change.percent + deaths.total + deaths.previous + 
                          deaths.change.percent, data=dailymodel2)
summary(daily2.diagnostics)
plot(dailymodel2$timestamp, daily2.diagnostics$residuals) # Plot residuals against ordered time T
abline(0, 0, col="red") # Red line with intercept and slope equal to 0

# Durbin-Watson Test for Serial Correlation 
#Testing for Serial Correlation
library(lmtest) # Activate the package
summary(daily2.diagnostics)
dwtest(daily2.diagnostics) #2.591 which is considered okay

# Multivariate Time Series Models
library(DataCombine) # Contains the slide() function
dailymodel2 <- slide(dailymodel2, Var="KUnits", NewVar="KUnits.L1", slideBy = -1)
dailymodel2 <- slide(dailymodel2, Var="KUnits", NewVar="KUnits.L2", slideBy = -2)
dailymodel2 <- slide(dailymodel2, Var="KUnits", NewVar="KUnits.L3", slideBy = -3)
dailymodel2 <- slide(dailymodel2, Var="KUnits", NewVar="KUnits.L4", slideBy = -4)
lm.KUnits.all <- lm(vix.close~T+S.P+Q2+Q3+Q4+
                      KUnits.L1+KUnits.L2+KUnits.L3+KUnits.L4, data=dailymodel2)
summary(lm.KUnits.all)
dwtest(lm.KUnits.all);

# Stepwise Selection w/step() Function
library(ISLR) # Contains the Hitters dataset
dailymodel2.null <- lm(vix.close ~ 1, data=dailymodel2) # Fit the Null (or reduced) model
dailymodel2.full <- lm(vix.close ~., data=dailymodel2) # Fit the Full (or large) model
# Stepwise can start with the Null model and proceed forward in both directions - NOT preferred
# Or, start with the Full model and proceed backwards in both directions - Preferred:
dailymodel2.step.backward <- step(dailymodel2.full, #(Starting with the Full model in this case)
                                 scope=list(lower=dailymodel2.null, upper=dailymodel2.full), direction="both", test="F")
summary(dailymodel2.step.backward)

### Multicollinearity Testing
library(perturb) # Contains the colldiag( ) function to compute the CI
library(car) # Contains the vif( ) function to compute VIF’s
library(MASS) # Contains the Boston housing market data set
lm.fit <- lm(vix.close ~ ., data=dailymodel2) # Full model with all predictors
summary(lm.fit)
colldiag(mod=lm.fit, scale=F, center=F, add.intercept=T) # Try scale and center = T
vif(lm.fit)
lm.fit.red <- lm(vix.close~timestamp+open+high+low+close+previous+vix.previous+
                   confirmed.total+confirmed.change.percent+deaths.total+
                   deaths.previous+deaths.change.percent, data=dailymodel2) # Reduced model
colldiag(mod=lm.fit.red, scale=F, center=F, add.intercept=T) # Try scale and center = T
vif(lm.fit.red)

##  The {caret} Package
library(caret)
set.seed(1) # Set the seed
# train() is a universal function in {caret} tat
lm.fit.caret <- train(vix.change~timestamp+open+high+low+close+previous+vix.previous+
                        confirmed.total+confirmed.change.percent+deaths.total+
                        deaths.previous+deaths.change.percent, 
                      data=dailymodel2, method = "lm") # Fit an lm() model
# The default cross validation is done with bootstrapping with 25 samples
lm.fit.caret # Reports RMSE and R squared
lm.fit.caret$results$RMSE # This is where RMSE is stored
lm.fit.caret$results$RMSE^2 # To get the MSE
summary(lm.fit.caret) # Same as lm() results – in fact, train() uses lm() internally
