full.data <- read.csv("C:/Users/mthng/OneDrive/Documents/Personal/School/4 Summer 2020/Regression (STAT 615-001)/Project/CovidandMarkets/CovidandMarkets/data/covidandmarkets.csv") %>%
  mutate(timestamp = as.Date(timestamp),
         result = as.factor(result),
         vix.result = as.factor(vix.result),
         company = as.factor(company),
         category = as.factor(category),
         confirmed.result = as.factor(confirmed.result),
         deaths.result = as.factor(deaths.result))

str(full.data)

model.data <- full.data

str(model.data)

full.lm <- lm(close ~ category + open + high + low + result +
                 previous + volume + vix.open + vix.high + vix.low + 
                 confirmed.Total + deaths.Total + closerate, data = model.data)
summary(full.lm)

techstocks <- model.data %>%
  filter(category == "technology") %>%
  select(-category)

str(techstocks)

hospstocks <- model.data %>%
  filter(category == "hospitality") %>%
  select(-category)

str(hospstocks)
