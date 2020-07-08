covidtotal <- read.csv("covidtotal.csv")

tech <- read.csv("tech.csv")

hosp <- read.csv("hosp.csv")

full.data <- rbind(tech, hosp) %>%
  filter(timestamp > as.Date("2020-01-21"))

full.data <- left_join(full.data, covidtotal, by = "timestamp")

full.data <- full.data %>%
  mutate(previous.confirmed = lag(confirmed.total),
         previous.deaths = lag(deaths.total),
         previous.confirmed.rate = lag(confirmed.total/previous.confirmed),
         previous.deaths.rate = lag(deaths.total/previous.deaths))

full.data <- full.data %>%
  na.omit() %>% 
  mutate(result = ifelse(close > previous, "up", "down"),
         vix.result = ifelse(vix.close > vix.previous, "up",
                             ifelse(vix.close < vix.previous, "down",
                                    "neautral")),
         confirmed.result = ifelse(confirmed.total > previous.confirmed, "up",
                                   ifelse(confirmed.total < previous.confirmed, "down",
                                          "neutral")),
         deaths.result = ifelse(deaths.total > previous.deaths, "up",
                                ifelse(deaths.total < previous.deaths, "down",
                                       "neutral"))) %>%
  select(timestamp:close, previous, previous.rate, result, vix.open:vix.previous.rate, vix.result, confirmed.total, previous.confirmed, previous.confirmed.rate, confirmed.result, deaths.total, previous.deaths, previous.deaths.rate, deaths.result)

full.data <- full.data %>%
  mutate(timestamp = as.Date(timestamp),
         result = as.factor(result),
         vix.result = as.factor(vix.result),
         company = as.factor(company),
         category = as.factor(category),
         confirmed.result = as.factor(confirmed.result),
         deaths.result = as.factor(deaths.result))

str(full.data)

write.csv(full.data, "covidandmarkets.csv", row.names = FALSE)
