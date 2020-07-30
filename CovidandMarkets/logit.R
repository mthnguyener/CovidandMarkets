logitweekly <- read.csv("weeklycovidandmarkets.csv") %>%
  mutate(result = as.factor(result),
         vix.result = as.factor(vix.result),
         company = as.factor(company),
         category = as.factor(category),
         confirmed.result = as.factor(confirmed.result),
         deaths.result = as.factor(deaths.result))

str(logitweekly)

#VIX GAIN - gain=1, no gain/loss=0
logitweekly <- logitweekly %>%
  mutate(vix.change = vix.previous - vix.close,
         vix.result=ifelse(close > previous, 1, 0),
         category=ifelse(category == "technology", 1, 0)) %>%
  rename(c("vix.gain"="vix.result", "technology"="category")) %>%
  na.omit() %>%
  select(-company, -result, -confirmed.result, -deaths.result)

str(logitweekly)

## Binomial Logistic Regression
#VIX GAIN - gain=1, no gain/loss=0
logitweekly.fit <- glm(vix.gain ~ week + technology + open + high + low + close + 
                         previous + change.percent + volume + vix.previous + 
                         confirmed.total + confirmed.previous + confirmed.change.percent + 
                         deaths.total + deaths.previous + deaths.change.percent - 
                         vix.change, 
                       family=binomial(link="logit"), 
                       data=logitweekly)
summary(logitweekly.fit)
require(coefplot)
coefplot(logitweekly.fit)
logitweekly.fit

test <- glm(vix.result ~ week + open + high + low + close + 
              previous + change.percent + volume + vix.previous + 
              confirmed.total + confirmed.previous + confirmed.change.percent + 
              deaths.total + deaths.previous + deaths.change.percent, 
            family=binomial(link="logit"), 
            data = weekly.model)
summary(test)
