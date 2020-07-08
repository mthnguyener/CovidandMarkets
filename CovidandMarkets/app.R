# Load Packages -----------------------------------------------------------
library(shiny)
library(tidyverse)
library(readr)
library(lubridate)
library(readxl)
library(scales)
library(gridExtra) 
library(grid)
library(png) 
library(downloader) 
library(grDevices)
library(broom)
library(stringr)
library(reshape)
library(leaflet)

# Load Data ---------------------------------------------------------------
alphabet <- read_csv("alphabet.csv", 
                     col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
amazon <- read_csv("amazon.csv", 
                   col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
apple <- read_csv("apple.csv", 
                  col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
facebook <- read_csv("facebook.csv", 
                     col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
microsoft <- read_csv("microsoft.csv", 
                      col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
netflix <- read_csv("netflix.csv", 
                    col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
zoom <- read_csv("zoom.csv", 
                 col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
carnival <- read_csv("carnival.csv", 
                     col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
darden <- read_csv("darden.csv", 
                   col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
hilton <- read_csv("hilton.csv", 
                   col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
marriott <- read_csv("marriott.csv", 
                     col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
mcdonalds <- read_csv("mcdonald.csv", 
                      col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
royal.caribbean <- read_csv("royal.caribbean.csv", 
                            col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)
starbucks <- read_csv("starbucks.csv", 
                      col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)

vix <- read_csv("vix.csv", 
                col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))) %>%
    select(-X1)

sp500.68 <- read_csv("sp500.68.csv")
sp500.57 <- read_csv("sp500.57.csv")

dowjones2 <- read_excel("dowjones2.xlsx")

devicesent <- read_csv("devices_twittercovsentiment.csv") %>%
    mutate(sources = as.factor(sources)) %>%
    pivot_longer(anger:trust, names_to = "sentiments", names_ptypes = list(factor()),
                 values_to = "sentiment_count") %>%
    mutate(sentiments = as.factor(sentiments))

covsent <- read_csv("twittercovsentiment.csv") %>%
    pivot_longer(anger:trust, names_to = "sentiments", names_ptypes = list(factor()),
                 values_to = "sentiment_count") %>%
    mutate(sentiments = as.factor(sentiments))

deaths <- read_csv("time_series_covid19_deaths_global.csv")
confirmed <- read.csv("time_series_covid19_confirmed_global.csv")
cases <- read.csv("time_series_19-covid-Confirmed.csv")


# Tidy Data ---------------------------------------------------------------
colnames(mcdonalds)
# fixing cases for spread
keep_vars <- c("Province.State", "Country.Region",
               "Lat", "Long")

date_vars <- names(cases)[!(names(cases) %in% keep_vars)]

all_cases <- melt(data = cases, id.vars = keep_vars, 
                  measure.vars = date_vars, 
                  variable_name = "date")

all_cases$date <- gsub("X", "", all_cases$date)

all_cases %>%
    filter(Province.State!="") %>%
    arrange(Lat, Long) %>%
    mutate(cases = value) %>%
    mutate(new_date= as.Date(as.character(date), 
                             "%m.%d.%y")) %>%
    mutate(Country.Region = as.character(Country.Region),
           Province.State = as.character(Province.State)) %>%
    select(-value) -> total_cases

countries <- unique(total_cases$Country.Region)

sp500.68 <- sp500.68 %>% 
    dplyr::rename(timestamp = Date,
                  open = Open, 
                  high = High,
                  low = Low,
                  close = Close)

sp500.57 <- sp500.57 %>% 
    dplyr::rename(timestamp = Date,
                  open = Open, 
                  high = High,
                  low = Low,
                  close = Close)

dowjones2 <- dowjones2 %>% 
    mutate(Date = as.Date(Date)) %>% 
    dplyr::rename(timestamp = Date, 
                  close = 'Closing Value') %>% 
    mutate(open = close, 
           high = close, 
           low = close)

marvsafter <- devicesent %>%
    mutate(timeframe = as.factor(ifelse(date <= as.Date("2020/03/14"), "march", "after"))) %>%
    select(date, timeframe, everything())

marvsafter$timeframe <- relevel(marvsafter$timeframe, ref = "march")

covmarvsafter <- covsent %>%
    mutate(timeframe = as.factor(ifelse(date <= as.Date("2020/03/14"), "march", "after"))) %>%
    select(date, timeframe, everything())

covmarvsafter$timeframe <- relevel(covmarvsafter$timeframe, ref = "march")

confirmed.total <- aggregate(. ~ Country.Region, confirmed, sum) %>%
    mutate(Country.Region = as.character(Country.Region)) %>%
    select(-Province.State, -Lat, -Long)

confirmed.total$Country.Region = replace(x = confirmed.total$Country.Region, 
                                         list =  !confirmed.total$Country.Region %in% c('China', 'US', 'Italy'), values =  'Others')

confirmed.total %>%
    group_by(Country.Region) %>%
    summarize_all(sum) -> confirmed.total

confirmed.total %>%
    bind_rows(summarize_all(., funs(if(is.numeric(.)) sum(.) else "Total"))) ->
    confirmed.total

confirmed.total <- confirmed.total %>% 
    pivot_longer(X1.22.20:X5.2.20, names_to = "date", names_prefix = "X") %>% 
    mutate(date = as.Date(date, "%m.%d.%y"))

deaths <- deaths %>% 
    dplyr::rename(Country.Region = 'Country/Region') %>% 
    mutate(Country.Region = as.character(Country.Region)) %>%
    select(-'Province/State', -Lat, -Long)

deaths.total <- 
    aggregate(. ~ Country.Region, deaths, sum)

deaths.total$Country.Region = replace(x = deaths.total$Country.Region, 
                                      list =  !deaths.total$Country.Region %in% c('China', 'US', 'Italy'), values =  'Others')

deaths.total %>%
    group_by(Country.Region) %>%
    summarize_all(sum) -> deaths.total

deaths.total %>%
    bind_rows(summarize_all(., funs(if(is.numeric(.)) sum(.) else "Total"))) ->
    deaths.total

deaths.total <- deaths.total %>% 
    pivot_longer('1/22/20':'5/2/20', names_to = "date") %>% 
    mutate(date = as.Date(date, "%m/%d/%y"))

cases.total <- 
    merge(confirmed.total, deaths.total, by = c("Country.Region", "date"))

cases.total <- cases.total %>% 
    dplyr::rename(confirmed = value.x, 
                  deaths = value.y)

dj <- dowjones %>% 
    dplyr::rename(date = timestamp)

sp <- sp500 %>% 
    dplyr::rename(date = timestamp)

vx <- vix %>% 
    dplyr::rename(date = timestamp)

cases.dj <- 
    merge(cases.total, dj, by = "date", all.y = TRUE)

cases.sp <- 
    merge(cases.total, sp, by = "date", all.y = TRUE)

cases.vx <- 
    merge(cases.total, vx, by = "date", all.y = TRUE)

cases.dj$after_con <- NA
cases.sp$after_con <- NA
cases.vx$after_con <- NA

for(i in 1:nrow(cases.dj)){
    if(cases.dj$date[i] >= "2020-01-22"){
        cases.dj$after_con[i] <- 1
    }else{
        cases.dj$after_con[i] <- 0
    }
}

for(i in 1:nrow(cases.sp)){
    if(cases.sp$date[i] >= "2020-01-22"){
        cases.sp$after_con[i] <- 1
    }else{
        cases.sp$after_con[i] <- 0
    }
}

for(i in 1:nrow(cases.vx)){
    if(cases.vx$date[i] >= "2020-01-22"){
        cases.vx$after_con[i] <- 1
    }else{
        cases.vx$after_con[i] <- 0
    }
}

cases.sp.adj <- cases.sp %>% 
    select(date, Country.Region, confirmed, deaths, close, after_con) %>% 
    filter(date >= "2019-09-01")

cases.dj.adj <- cases.dj %>% 
    select(date, Country.Region, confirmed, deaths, close, after_con) %>% 
    filter(date >= "2019-09-01")

cases.vx.adj <- cases.vx %>% 
    select(date, Country.Region, confirmed, deaths, close, after_con) %>% 
    filter(date >= "2019-09-01")

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
