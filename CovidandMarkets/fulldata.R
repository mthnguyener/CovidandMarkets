covidtotal <- read.csv("covidtotal.csv") %>%
  select(-X)

tech <- read.csv("tech.csv")

hosp <- read.csv("hosp.csv")

full.data <- rbind(tech, hosp) %>%
  filter(timestamp > as.Date("2020-01-21")) %>% 
  select(-X)

full.data <- left_join(full.data, covidtotal, by = "timestamp")

## export data 
write.csv(full.data, "covidandmarkets.csv")
