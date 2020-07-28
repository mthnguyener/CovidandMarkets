df <- read.csv("covidandmarkets.csv") %>%
  mutate(timestamp = as.Date(timestamp),
         result = as.factor(result),
         vix.result = as.factor(vix.result),
         company = as.factor(company),
         category = as.factor(category),
         confirmed.result = as.factor(confirmed.result),
         deaths.result = as.factor(deaths.result)) %>%
  select(-result, -vix.result, -confirmed.result, -deaths.result) %>%
  na.omit()

str(df)

week0 <- df %>%
  filter(between(timestamp, as.Date("2020-01-22"), as.Date("2020-01-24"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = -1) %>%
  select(timestamp, company:deaths.change.percent)

week00 <- df %>%
  filter(between(timestamp, as.Date("2020-01-27"), as.Date("2020-01-31"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 0) %>%
  select(timestamp, company:deaths.change.percent)

week1 <- df %>%
  filter(between(timestamp, as.Date("2020-02-03"), as.Date("2020-02-07"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 1) %>%
  select(timestamp, company:deaths.change.percent)

week2 <- df %>%
  filter(between(timestamp, as.Date("2020-02-10"), as.Date("2020-02-14"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 2) %>%
  select(timestamp, company:deaths.change.percent)

week3 <- df %>%
  filter(between(timestamp, as.Date("2020-02-17"), as.Date("2020-02-21"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 3) %>%
  select(timestamp, company:deaths.change.percent)

week4 <- df %>%
  filter(between(timestamp, as.Date("2020-02-24"), as.Date("2020-02-28"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 4) %>%
  select(timestamp, company:deaths.change.percent)

week5 <- df %>%
  filter(between(timestamp, as.Date("2020-03-02"), as.Date("2020-03-06"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 5) %>%
  select(timestamp, company:deaths.change.percent)

week6 <- df %>%
  filter(between(timestamp, as.Date("2020-03-09"), as.Date("2020-03-13"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 6) %>%
  select(timestamp, company:deaths.change.percent)

week7 <- df %>%
  filter(between(timestamp, as.Date("2020-03-16"), as.Date("2020-03-20"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 7) %>%
  select(timestamp, company:deaths.change.percent)

week8 <- df %>%
  filter(between(timestamp, as.Date("2020-03-23"), as.Date("2020-03-27"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 8) %>%
  select(timestamp, company:deaths.change.percent)

week9 <- df %>%
  filter(between(timestamp, as.Date("2020-03-30"), as.Date("2020-04-03"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 9) %>%
  select(timestamp, company:deaths.change.percent)

week10 <- df %>%
  filter(between(timestamp, as.Date("2020-04-06"), as.Date("2020-04-10"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 10) %>%
  select(timestamp, company:deaths.change.percent)

week11 <- df %>%
  filter(between(timestamp, as.Date("2020-04-13"), as.Date("2020-04-17"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 11) %>%
  select(timestamp, company:deaths.change.percent)

week12 <- df %>%
  filter(between(timestamp, as.Date("2020-04-20"), as.Date("2020-04-24"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 12) %>%
  select(timestamp, company:deaths.change.percent)

week13 <- df %>%
  filter(between(timestamp, as.Date("2020-04-27"), as.Date("2020-05-01"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 13) %>%
  select(timestamp, company:deaths.change.percent)

week14 <- df %>%
  filter(between(timestamp, as.Date("2020-05-04"), as.Date("2020-05-08"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 14) %>%
  select(timestamp, company:deaths.change.percent)

week15 <- df %>%
  filter(between(timestamp, as.Date("2020-05-11"), as.Date("2020-05-15"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 15) %>%
  select(timestamp, company:deaths.change.percent)

week16 <- df %>%
  filter(between(timestamp, as.Date("2020-05-18"), as.Date("2020-05-22"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 16) %>%
  select(timestamp, company:deaths.change.percent)

week17 <- df %>%
  filter(between(timestamp, as.Date("2020-05-25"), as.Date("2020-05-29"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 17) %>%
  select(timestamp, company:deaths.change.percent)

week18 <- df %>%
  filter(between(timestamp, as.Date("2020-06-01"), as.Date("2020-06-05"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 18) %>%
  select(timestamp, company:deaths.change.percent)

week19 <- df %>%
  filter(between(timestamp, as.Date("2020-06-08"), as.Date("2020-06-12"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 19) %>%
  select(timestamp, company:deaths.change.percent)

week20 <- df %>%
  filter(between(timestamp, as.Date("2020-06-15"), as.Date("2020-06-19"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 20) %>%
  select(timestamp, company:deaths.change.percent)

week21 <- df %>%
  filter(between(timestamp, as.Date("2020-06-22"), as.Date("2020-06-26"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 21) %>%
  select(timestamp, company:deaths.change.percent)

week22 <- df %>%
  filter(between(timestamp, as.Date("2020-06-29"), as.Date("2020-07-03"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 22) %>%
  select(timestamp, company:deaths.change.percent)

week23 <- df %>%
  filter(between(timestamp, as.Date("2020-07-06"), as.Date("2020-07-10"))) %>%
  group_by(company, category) %>%
  summarize_all(funs(mean)) %>%
  mutate(timestamp = 23) %>%
  select(timestamp, company:deaths.change.percent)


fullweekly.data <- rbind(week0, week00, week1, week2, week3, week4, week5, week6, 
                         week7, week8, week9, week10, week11, week12, week13, week14, 
                         week15, week16, week17, week18, week19, week20, week21, week22, 
                         week23) %>% 
  rename(c("week"="timestamp"))

fullweekly.data <- fullweekly.data %>%
  mutate(previous = lag(close),
         vix.previous = lag(vix.close),
         confirmed.previous = lag(confirmed.total),
         deaths.previous = lag(deaths.total),
         change.percent = lag((close-previous)/previous),
         vix.change.percent = lag((vix.close-vix.previous)/vix.previous),
         confirmed.change.percent = lag((confirmed.total-confirmed.previous)/confirmed.previous),
         deaths.change.percent = lag((deaths.total-deaths.previous)/deaths.previous),
         result = ifelse(close > previous, "up", "down"),
         vix.result = ifelse(vix.close > vix.previous, "up",
                             ifelse(vix.close < vix.previous, "down",
                                    "neutral")),
         confirmed.result = ifelse(confirmed.total > confirmed.previous, "up",
                                   ifelse(confirmed.total < confirmed.previous, "down",
                                          "neutral")),
         deaths.result = ifelse(deaths.total > deaths.previous, "up",
                                ifelse(deaths.total < deaths.previous, "down",
                                       "neutral"))) %>%
  select(week:close, previous:change.percent, result, volume, vix.open:vix.change.percent, vix.result, confirmed.total:confirmed.change.percent, confirmed.result, deaths.total:deaths.change.percent, deaths.result)

str(fullweekly.data)

write.csv(fullweekly.data, "weeklycovidandmarkets.csv", row.names = FALSE)
