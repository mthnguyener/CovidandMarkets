av_api_key("Y6CSLVUZ62BOXC8U")

alphabet <- av_get("GOOGL", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
alphabet <- alphabet %>%
  mutate(company = "alphabet")
amazon <- av_get("AMZN", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
amazon <- amazon %>%
  mutate(company = "amazon")
apple <- av_get("AAPL", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
apple <- apple %>%
  mutate(company = "apple")
facebook <- av_get("FB", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
facebook <- facebook %>%
  mutate(company = "facebook")
microsoft <- av_get("MSFT", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
microsoft <- microsoft %>%
  mutate(company = "microsoft")
netflix <- av_get("NFLX", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
netflix <- netflix %>%
  mutate(company = "netflix")
zoom <- av_get("ZM", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
zoom <- zoom %>%
  mutate(company = "zoom")

carnival <- av_get("CCL", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
carnival <- carnival %>%
  mutate(company = "carnival")
darden <- av_get("DRI", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
darden <- darden %>%
  mutate(company = "darden")
hilton <- av_get("HLT", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
hilton <- hilton %>%
  mutate(company = "hilton")
marriott <- av_get("MAR", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
marriott <- marriott %>%
  mutate(company = "marriott")
mcdonald <- av_get("MCD", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
mcdonald <- mcdonald %>%
  mutate(company = "mcdonald")
royal.caribbean <- av_get("RCL", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
royal.caribbean <- royal.caribbean %>%
  mutate(company = "royal.caribbean")
starbucks <- av_get("SBUX", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
starbucks <- starbucks %>%
  mutate(company = "starbucks")

sectors <- av_get(av_fun = "SECTOR")
vix <- av_get("VIX", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full") %>%
  select(-adjusted_close, -dividend_amount, -split_coefficient)
vix <- vix %>%
  mutate(company = "vix") %>%
  filter(timestamp > as.Date("2019-12-31"))

#alphabet, amazon, apple, facebook, microsoft, netflix, zoom
tech <- rbind(alphabet, amazon, apple, facebook, microsoft, netflix, zoom) %>%
  filter(timestamp > as.Date("2019-12-31")) %>%
  mutate(category = "technology") %>%
  select(timestamp, company, category, open:volume)

tech <- left_join(tech, vix, by = "timestamp") %>%
  rename(c("company"="company.x", "volume"="volume.x",
           "open"="open.x", "high"="high.x", "low"="low.x", "close"="close.x", 
           "vix.open"="open.y", "vix.high"="high.y", "vix.low"="low.y", "vix.close"="close.y")) %>%
  select(-volume.y, -company.y)
  

#carnival, darden, hilton, marriott, mcdonald, royal.caribbean, starbucks
hosp <- rbind(darden, hilton, marriott, mcdonald, royal.caribbean, starbucks) %>%
  filter(timestamp > as.Date("2019-12-31")) %>%
  mutate(category = "hospitality") %>%
  select(timestamp, company, category, open:volume)

hosp <- left_join(hosp, vix, by = "timestamp") %>%
  rename(c("company"="company.x", "volume"="volume.x",
           "open"="open.x", "high"="high.x", "low"="low.x", "close"="close.x", 
           "vix.open"="open.y", "vix.high"="high.y", "vix.low"="low.y", "vix.close"="close.y")) %>%
  select(-volume.y, -company.y)

## export data 
write.csv(alphabet, "alphabet.csv")
write.csv(amazon, "amazon.csv")
write.csv(apple, "apple.csv")
write.csv(facebook, "facebook.csv")
write.csv(microsoft, "microsoft.csv")
write.csv(netflix, "netflix.csv")
write.csv(zoom, "zoom.csv")

write.csv(carnival, "carnival.csv")
write.csv(darden, "darden.csv")
write.csv(hilton, "hilton.csv")
write.csv(marriott, "marriott.csv")
write.csv(mcdonald, "mcdonald.csv")
write.csv(royal.caribbean, "royal.caribbean.csv")
write.csv(starbucks, "starbucks.csv")

write.csv(vix, "vix.csv")
write.csv(sectors, "sectors.csv")
write.csv(tech, "tech.csv")
write.csv(hosp, "hosp.csv")
