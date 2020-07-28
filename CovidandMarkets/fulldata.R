covidtotal <- read.csv("covidtotal.csv")

tech <- read.csv("tech.csv")

hosp <- read.csv("hosp.csv")

full.data <- rbind(tech, hosp) %>%
  filter(timestamp > as.Date("2020-01-21"))

full.data <- left_join(full.data, covidtotal, by = "timestamp")

full.data <- full.data %>%
  rename(c("vix.change.percent"="vix.previous.rate", 
           "change.percent"="previous.rate")) %>%
  mutate(change.percent = lag((close-previous)/previous),
         vix.change.precent = lag((vix.close-vix.previous)/vix.previous),
         confirmed.previous = lag(confirmed.total),
         deaths.previous = lag(deaths.total),
         confirmed.change.percent = lag((confirmed.total - confirmed.previous)/
                                          confirmed.previous),
         deaths.change.percent = lag((deaths.total - deaths.previous)/deaths.previous))
  

full.data <- full.data %>%
  mutate(result = ifelse(close > previous, "up", "down"),
         vix.result = ifelse(vix.close > vix.previous, "up",
                             ifelse(vix.close < vix.previous, "down",
                                    "neutral")),
         confirmed.result = ifelse(confirmed.total > confirmed.previous, "up",
                                   ifelse(confirmed.total < confirmed.previous, "down",
                                          "neutral")),
         deaths.result = ifelse(deaths.total > deaths.previous, "up",
                                ifelse(deaths.total < deaths.previous, "down",
                                       "neutral"))) %>%
  select(timestamp:close, volume, previous, change.percent, result, vix.open:vix.change.percent, vix.result, confirmed.total, confirmed.previous, confirmed.change.percent, confirmed.result, deaths.total, deaths.previous, deaths.change.percent, deaths.result)

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
