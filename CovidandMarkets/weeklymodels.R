dfweekly <- read.csv("weeklycovidandmarkets.csv") %>%
  mutate(result = as.factor(result),
         vix.result = as.factor(vix.result),
         company = as.factor(company),
         category = as.factor(category),
         confirmed.result = as.factor(confirmed.result),
         deaths.result = as.factor(deaths.result))

str(dfweekly)

#previous.rate = open/previous,
#vix.previous.rate = vix.open/vix.previous
#previous.confirmed.rate = confirmed.total/previous.confirmed
#previous.deaths.rate = deaths.total/previous.deaths
weekly.model <- dfweekly %>%
  mutate(vix.change = vix.previous - vix.close)

str(weekly.model)

#VIX OPEN (FULL - unreliable)
weekly.lm <- lm(vix.open ~ . - week - company, data = weekly.model)
summary(weekly.lm)

#VIX OPEN (Model 1 - assumption)
weekly1.lm <- lm(vix.open ~ category + open + previous + vix.previous +
                   confirmed.previous + deaths.previous, 
                data = weekly.model)
summary(weekly1.lm)

#VIX CLOSE (Model 2 - assumption)
weekly2.lm <- lm(vix.close ~ category + open + high + low + close + previous + 
                   vix.previous + confirmed.total + confirmed.change.percent + 
                   deaths.total + deaths.change.percent, 
                 data = weekly.model)
summary(weekly2.lm)

#VIX CHANGE (Model 3 - assumption)
weekly3.lm <- lm(vix.change ~ category + open + high + low + close + previous + 
                   change.percent + vix.change.percent + confirmed.total + 
                   confirmed.previous + confirmed.change.percent + 
                   deaths.total + deaths.previous + deaths.change.percent,
                data = weekly.model)
summary(weekly3.lm)

techstocks <- weekly.model %>%
  filter(category == "technology") %>%
  select(-category)

str(techstocks)

hospstocks <- weekly.model %>%
  filter(category == "hospitality") %>%
  select(-category)

str(hospstocks)

# Durbin- Watson Test
summary(model)
dwtest(model)
