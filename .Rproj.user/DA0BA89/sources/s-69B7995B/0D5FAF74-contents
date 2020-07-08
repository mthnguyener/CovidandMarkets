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
covidcounts <- unnest(df, cols=data) %>% 
  ungroup()
rm(df)
covidcounts <- select(covidcounts, -vars)

#Cleaning columns
df <- covidcounts %>%
  rename(c("timestamp"="Date", "country"="Country_Region", "total"="Daily_Total")) %>%
  select(case_type, timestamp, country, total, -Country_State, -Province_State, -Lat, -Long, -County, -Population, -Continent)

df$country = replace(x = df$country, 
                             list =  !df$country %in% c('China', 'US', 'Italy'),
                             values =  'Others')

df <- df %>%
  filter(case_type == "confirmed_global" | case_type == "deaths_global") %>%
  mutate(type = case_when(str_detect(case_type, "confirmed_global") ~ "confirmed",
                          str_detect(case_type, "deaths_global") ~ "deaths",
                          TRUE ~ "other")) %>%
  select(-case_type)
  
df <- df %>%
  group_by(type, timestamp, country) %>%
  summarize_all(sum)

df <- df %>%
  pivot_wider(names_from = c(type,country),  names_sep = ".", values_from = total)

df <- df %>%
  mutate(confirmed.total = sum(confirmed.China, confirmed.Italy, 
                               confirmed.Others, confirmed.US),
         deaths.total =  sum(deaths.China, deaths.Italy, 
                             deaths.Others, deaths.US)) %>%
  select(timestamp, confirmed.total, deaths.total)

## export data 
write.csv(df, "covidtotal.csv", row.names = FALSE)
