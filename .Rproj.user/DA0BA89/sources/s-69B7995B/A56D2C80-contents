library(tidyverse)
library(tibble)
library(purrr)
library(stringr)
library(lubridate)
library(tree)
library(dplyr)
library(countrycode)
library(tidytext)
library(readr)
library(scales)
library(ggplot2)
library(readxl)
library(shiny)
library(readxl)
library(gridExtra) 
library(grid)
library(png) 
library(downloader) 
library(grDevices)
library(broom)
library(reshape)
library(leaflet)
library(plotly)

library(devtools)
library(usethis)
library(keyring)
library(alphavantager)
library(twitteR)


# John Hopkins COVID19 DATA:
# https://github.com/CSSEGISandData/COVID-19
# https://github.com/CSSEGISandData/COVID-19.git

key_set("key")
key_set("secret")
key_set("atoken")
key_set("asecret")

key <- key_get("key")
secret <- key_get("secret")
atoken <- key_get("atoken")
asecret <- key_get("asecret")

setup_twitter_oauth(key, secret, atoken, asecret)
