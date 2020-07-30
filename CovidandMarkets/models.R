df <- read.csv("covidandmarkets.csv") %>%
  mutate(result = as.factor(result),
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
  mutate(timestamp = as.Date(timestamp),
         vix.change = vix.close - vix.previous,
         vix.change.percent = lag((vix.close-vix.previous)/vix.previous)) %>%
  select(timestamp:close, previous, change.percent, volume, result:deaths.result, vix.change)

str(model.data)

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

#VIX OPEN (Model 1 - assumption)
model1.lm <- lm(vix.open ~ timestamp + category + open + high + low + close + previous + 
                  vix.previous + confirmed.total + confirmed.previous + 
                  confirmed.change.percent + deaths.total + deaths.previous + 
                  deaths.change.percent, 
                 data = model.data)
summary(model1.lm)

#VIX CLOSE (Model 2 - assumption)
weekly2.lm <- lm(vix.close ~ timestamp + category + open + high + low + close + 
                   previous + vix.previous + confirmed.total + confirmed.previous + 
                   confirmed.change.percent + deaths.total + deaths.previous + 
                   deaths.change.percent, 
                 data = model.data)
summary(weekly2.lm)

#VIX CHANGE (Model 3 - assumption)
weekly3.lm <- lm(vix.change ~ timestamp + category + open + high + low + close + 
                   previous + vix.previous + confirmed.total + confirmed.previous + 
                   confirmed.change.percent + deaths.total + deaths.previous + 
                   deaths.change.percent,
                 data = model.data)
summary(weekly3.lm)

#VIX RESULT (Model 4 - assumption)
weekly4.lm <- glm(vix.result ~ timestamp + category + open + high + low + close + 
                   previous + vix.previous + confirmed.total + confirmed.previous + 
                   confirmed.change.percent + deaths.total + deaths.previous + 
                   deaths.change.percent, 
                  family=binomial(link="logit"), 
                  data = model.data)
summary(weekly4.lm)


#BY CATEGORY
techstocks <- model.data %>%
  filter(category == "technology") %>%
  select(-category)

str(techstocks)

hospstocks <- model.data %>%
  filter(category == "hospitality") %>%
  select(-category)

str(hospstocks)

##CHECK THIS -----------------------------
# The data sets were imported as salaries and salariesNew

salaries <- as.data.frame(salaries)
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