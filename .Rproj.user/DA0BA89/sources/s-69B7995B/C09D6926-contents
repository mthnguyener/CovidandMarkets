library(tidyverse)
library(lubridate)
library(stringr)
library(countrycode)

##Get the data files
url_in <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

df <- tibble(file_names = c("time_series_covid19_confirmed_global.csv",
                            "time_series_covid19_deaths_global.csv",
                            "time_series_covid19_confirmed_US.csv",
                            "time_series_covid19_deaths_US.csv"))
df %>% 
  mutate(url = str_c(url_in, file_names, sep =""),
         data = map(url, ~read_csv(., na = "")),
         case_type = as.factor(str_extract(file_names,"[:alpha:]*_[gU][:alpha:]*"))) %>%
  select(case_type, data) ->
  df

#Get current Data in the four files
#url_in <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
# file_names <- c("time_series_covid19_confirmed_global.csv",
#                 "time_series_covid19_deaths_global.csv",
#                 "time_series_covid19_confirmed_US.csv",
#                 "time_series_covid19_deaths_US.csv")
# 
#     df <- tibble(case_type = as.factor(str_extract(file_names,"[:alpha:]*_[gU][:alpha:]*")))
# 
# for (i in seq_along(file_names)){
#   df$data[[i]] <- read_csv(str_c(url_in,file_names[i],collapse = ""), na = "")
# }

#Clean Data
#Use map() to add the names from each of the four data frames to a new variable in df called vars and visually compare them to identify issues.
# get names
df$vars <- map(df$data,  names)

#Fix any issues and create consistent data frames
#Create a helper function
fix_names <- function(df, pattern, repl){
  stopifnot(is.data.frame(df), is.character(pattern), is.character(repl))
  names(df) <- str_replace_all(names(df), pattern, repl)
  return(df)
}

df %>% 
  mutate(data = map(data, ~ fix_names(., "([ey])/", "\\1_")),
         data = map(data, ~ fix_names(., "Admin2", "County")),
         data = map(data, ~ fix_names(., "Long_", "Long")),
         data = map_if(data, str_detect(df$case_type,"US"),
                       ~select(.,-c("UID", "iso2", "iso3", "code3", "FIPS",
                                    "Combined_Key"))),
         data = map_if(data, str_detect(df$case_type,"global"),
                       ~ mutate(., County = "None" )),
         data = map_if(data, !str_detect(df$case_type,"deaths_US"),
                       ~ mutate(., Population = 0)),
         data = map(data, ~unite(.,"Country_State",
                                 c("Country_Region","Province_State"),
                                 na.rm = TRUE, remove = FALSE))
  )->df

df$vars <- map(df$data,  names)

# for ( i in seq_along(file_names)){
#    names(df$data[[i]]) <- str_replace_all(names(df$data[[i]]),"([ey])/","\\1_")
#    names(df$data[[i]]) <- str_replace_all(names(df$data[[i]]),"Admin2","County")
#    names(df$data[[i]]) <- str_replace_all(names(df$data[[i]]),"Long_","Long")
#    if(str_detect(df$case_type[i],"US")){
#     df$data[[i]] <- df$data[[i]] <- select(df$data[[i]], -c("UID", "iso2", "iso3", "code3", "FIPS", "Combined_Key"))
#    }
#    if(!str_detect(names(df$data[[i]]),"Population")){
#      df$data[[i]]$Population <- 0
#    }
#    if(!str_detect(names(df$data[[i]]),"County")){
#      df$data[[i]]$County <- "None"
#    }
#    #df$data[[i]]$Type <- df$case_type[i]
#    df$data[[i]] <- unite(df$data[[i]], "Country_State",
#                                     c("Country_Region","Province_State"),
#                                      na.rm = TRUE, remove = FALSE)
#    
#   }

#Tidy each dataframe
df %>% 
  mutate(data = map(data, ~ pivot_longer(data = ., cols = contains("/"), 
                                         names_to = "Date", 
                                         values_to = "Daily_Total") ))-> 
  df

#helper function for Date Change
fix_date_mdy <- function(df,var_name){
  stopifnot(is.data.frame(df), is.character(var_name))
  df[[var_name]]<- mdy(df[[var_name]])
  return(df)
}

df %>%
  mutate(data = map(data, ~fix_date_mdy(., "Date")))-> 
  df

# df$vars <- map(df$data,  names)
# for ( i in seq_along(file_names)){
# df$data[[i]] <- 
#     pivot_longer(df$data[[i]], cols = contains("/"), names_to = "Date", values_to = "Daily_Total") 
# df$data[[i]]$Date <- mdy(df$data[[i]]$Date)
# }

#Add Continents
# add continents to dataset
df %>% 
  mutate(data = map(data, ~mutate(., Continent = countrycode(Country_Region,
                                                             origin = "country.name",
                                                             destination = "continent")))
  )->df

# for ( i in seq_along(file_names)){
# df$data[[i]]$continent = countrycode(df$data[[i]]$Country_Region, origin = "country.name",
#                     destination = "continent") 
# print(unique(df$data[[i]]$continent))
# print(unique(df$data[[i]]$Country_Region[is.na(df$data[[i]]$continent)]))
# }

# replace NAs with a continent
df %>% 
  mutate(data = map(data, ~mutate(.,Continent = case_when(
    Country_Region == "Diamond Princess" ~ "Asia",
    Country_Region == "MS Zaandam" ~ "Americas",
    Country_Region == "Kosovo" ~ "Europe",
    TRUE ~ Continent)))
  )-> df

map(df$data, ~unique(.$Continent))

# for ( i in seq_along(file_names)){
#     df$data[[i]]$Continent <-  case_when(
#     df$data[[i]]$Country_Region == "Diamond Princess" ~ "Asia",
#     df$data[[i]]$Country_Region == "MS Zaandam" ~ "Americas",
#     df$data[[i]]$Country_Region == "Kosovo" ~ "Europe",
#     TRUE ~ df$data[[i]]$Continent)
#   print(unique(df$data[[i]]$Continent))
# }

#Unnest the Data Frames
df_all <- unnest(df, cols=data) %>% 
  ungroup()
rm(df)
df_all <- select(df_all, -vars)

#Get World Population Data
df_pop <- read_csv("C:/Users/mthng/OneDrive/Documents/Personal/School/3 Spring 2020/Data Science (STAT 613-001)/Lectures/Lectures/10_many_models/WPP2019_TotalPopulation.csv")
head(df_pop)

#Add Population Data to df
df_all %>% 
  left_join(df_pop, by = c("Country_Region"= "Location"))  -> 
  df_all

df_all %>%
  filter(case_type == "confirmed_global") %>%
  pivot_wider(names_from = Date, values_from = Daily_Total) %>%
  select(-case_type, -Country_State, -Province_State, -Lat, -Long, -County, -Population, 
         -Continent, -LocID) %>%
  group_by(Country_Region) %>%
  summarize_all(sum) %>%
  pivot_longer(cols = contains("-"),
               names_to = "Date",
               values_to = "Daily_Total") %>%
  rename(Pop_Total = PopTotal,
         Pop_Density = PopDensity)  %>%
  mutate(Case_Type = "Confirmed") %>%
  select(Date, Case_Type, Country_Region, Pop_Total, Pop_Density, Daily_Total) -> df_confirmed

df_all %>%
  filter(case_type == "deaths_global") %>%
  pivot_wider(names_from = Date, values_from = Daily_Total) %>%
  select(-case_type, -Country_State, -Province_State, -Lat, -Long, -County, -Population, 
         -Continent, -LocID) %>%
  group_by(Country_Region) %>%
  summarize_all(sum) %>%
  pivot_longer(cols = contains("-"),
               names_to = "Date",
               values_to = "Daily_Total") %>%
  rename(Pop_Total = PopTotal,
         Pop_Density = PopDensity) %>%
  mutate(Case_Type = "Deaths") %>%
  select(Date, Case_Type, Country_Region, Pop_Total, Pop_Density, Daily_Total) -> df_deaths

rbind(df_confirmed, df_deaths) -> df_all

df_all %>% 
  mutate(Country_Region = case_when(str_detect(Country_Region, "\\bChina\\b") ~ "China",
                                    str_detect(Country_Region, "\\bItaly\\b") ~ "Italy",
                                    str_detect(Country_Region, "\\bUS\\b") ~ "US",
                       TRUE ~ "Others")) %>% 
  group_by(Date, Case_Type, Country_Region) %>%
  summarize(Daily_Total = sum(Daily_Total)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(Case_Type,Country_Region), values_from = Daily_Total) %>%
  mutate(Date = as.Date(Date)) ->  
  epi_total

###EXTRA ANALYSIS###

#Analyze Data
df_all %>% 
  group_by(Country_Region, Continent, case_type) %>%
  summarize(total = sum(max(Daily_Total)), per_pop = total/max(1000*PopTotal)) %>%
  ungroup() -> df_country_state

#What are the 25 Countries with the most confirmed cases and what is the percentage of their total population affected?
df_country_state %>%
  filter(case_type %in% c("confirmed_global")) %>% 
  arrange(desc(total)) %>%
  slice(1:25) ->
  top_25_cases

top_25_cases

#What are the 25 Countries with the most deaths and what is the percentage of their total population affected?
df_country_state %>%
  filter(case_type %in% c("deaths_global")) %>%
  arrange(desc(total)) %>%
  slice(1:25) ->
  top_25_deaths

top_25_deaths

#Which countries in the top 25 for percentage of population affected are Not in the top 25s for the absolute number of cases and deaths?
df_country_state %>%
  filter(case_type %in% c("confirmed_global")) %>% 
  arrange(desc(per_pop)) %>%
  slice(1:25) ->
  top_25_cases_per_pop

anti_join(top_25_cases_per_pop, top_25_cases)


df_country_state %>%
  filter(case_type %in% c("deaths_global"))  %>% 
  arrange(desc(per_pop)) %>%
  slice(1:25) ->
  top_25_deaths_per_pop

anti_join(top_25_deaths_per_pop, top_25_deaths)

#Plot the number of cases/deaths over time for the top 25 country/states faceting by continent. Use appropriate scales for the axes.
df_all %>% 
  group_by(case_type, Country_Region, Continent, Date) %>% 
  summarize(country_total = sum(Daily_Total)) ->
  df_summary_country

plot_data_c <- right_join(df_summary_country, top_25_cases)

plot_data_c %>% 
  ggplot(aes(x = Date, y = country_total, 
             group = Country_Region, 
             color = Country_Region)) +
  geom_line() +
  facet_wrap(~Continent, scales = "free") +
  scale_y_log10() +
  guides(color = guide_legend(ncol = 5)) +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90))

plot_data_d <- right_join(df_summary_country, top_25_deaths)
plot_data_d %>% 
  ggplot(aes(x = Date, y = country_total, 
             group = Country_Region, 
             color = Country_Region)) +
  geom_line() +
  facet_wrap(~Continent, scales = "free") +
  scale_y_log10() +
  guides(color = guide_legend(ncol = 5)) +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90))
