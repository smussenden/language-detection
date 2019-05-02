#############################################
####### Language Detection Project ##########
#############################################

# This project is part of an effort to use publicly available information on the web -- including information on social networks like Twitter -- to determine what languages are spoken in a given geographic area.

#############################################
####### Load Required Packages ##############
#############################################

# For connecting directly to Twitter API to get supported languages, then  parsing output
library(httr)
library(httpuv)
library(jsonlite)
library(stringi)

# For easier connection to Twitter search API
library(rtweet)

# For general data wrangling
library(tidyverse)

# For connecting to langscape tool to spot-check Twitter language results
library(httr)
library(openxlsx)
library(qdapRegex)
library(janitor)

library(gridExtra)

rm(list=ls())

#############################################
###### Connect to Twitter API with HTTR #####
#############################################

# Modified version of directions here: https://github.com/r-lib/httr/blob/master/demo/oauth1-twitter.r. As those directions note, you need to register an app at https://apps.twitter.com/ for use with HTTR package.

# One note, if you follow directions linked above. The directions say to make sure to set the app's callback url to "http://127.0.0.1:1410/".  That didn't work for us, kept getting 403 errors (forbidden), which was a sign that there was a problem with callback URL.  It was only after we added every possible itteration of that URL, including localhost/1410, that it actually worked! You can add up to 10 callback URLs, so add as many as you want. This is what we put. https://localhost:1410, http://127.0.0.1:1410/, https://127.0.0.1:1410, http://localhost:1410, https://localhost:1410/, http://localhost:1410/, https://127.0.0.1:1410/, http://127.0.0.1:1410.  This step is necessary, because it will launch a browser window and have you authenticate with twitter.

# In the folder twitter_keys, create a new file called httr-keys.R.  In that file, paste in a function with your app name, consumer API key and consumer secret key. The function should look like this, subsitituting your app name, key and secret.

# myapp <- oauth_app("app_name",
#                   key = "pastekeyinhere",
#                   secret = "pastesecretinhere"

# Load the file httr-keys.R that contains your authentication keys to connect to Twitter. It will load an object called myapp, which contains your keys.
source("twitter_keys/httr-keys.R")
source("functions/get_twitter_supported_languages.r")


#############################################
###### Connect to RTWEET Package ############
#############################################

# In the folder twitter_keys, create a new file called rtweet-keys.R.  In that file, paste in a function with your app name, consumer key, consumer secret key, access token and access secret. The function should look like this, subsitituting your info.

# create_token(
#  app = "yourappname",
#  consumer_key = "pastekeyinhere",
#  consumer_secret = "pastesecretinhere",
#  access_token = "pastetokeninhere",
#  access_secret = "pastesecretinhere"
#)

# Load the file rtweet-keys.R that contains your authentication keys to connect to Twitter. It will create a token with your information.

source("twitter_keys/rtweet-keys.R")
source("functions/language_search.R")
source("functions/rate_check.R")

#############################################
#### Search for Languages Using Function ####
#############################################

# language_search(city_name, country_name, geocode) takes three arguments. Each argument must be in quotes. city_name and country_name aren't actually used in the search, they're just there for the output dataframe. geocode must be in a specific format. Latitude and Longitude of specific point, followed by radius around that point in miles."51.50,0.15,20mi". This is finicky. No spaces.

# To find lat and long and a radius, use this tool
# https://www.mapdevelopers.com/draw-circle-tool.php

# London
language_search("London", "UK", "51.50,0.15,20mi")

# Before moving on, check and see how many tweets we can continue collecting before hitting rate limit, and time remaining before rate limit resets (every 15 minutes, 180 tweets max). Build a function called rate_check, then run rate_check.  In the future, could write a function to check if there's time left before rate limit expires, and, if so, move on.

rate_check()

# New York
language_search("New York", "NY", "40.71,-74.01,20mi")
rate_check()

# Bolivia
language_search("LaPaz", "Bolivia", "-16.50,-68.15,20mi")
rate_check()

# Tajikistan
language_search("Dushanbe", "Tajikistan", "38.55,68.80,20mi")
rate_check()

#############################################
#### Read in Language on the Web Data #######
#############################################

bolivia_web <- read_csv("data/web-research/bolivia-web.csv")
tajikistan_web <- read_csv("data/web-research/tajikistan-web.csv")
london_web <- read_csv("data/web-research/london-web.csv")
nyc_web <- read_csv("data/web-research/nyc-web.csv")

#############################################
#### Read in Census Data ####################
#############################################

bolivia_census <- read_csv("data/census/bolivia-census.csv")
tajikistan_census <- read_csv("data/census/tajikistan-census.csv")
london_census <- read_csv("data/census/london-census.csv")
nyc_census <- read_csv("data/census/nyc-census.csv")

#############################################
#### Check with Twitter + Web ###############
#############################################
####THIS NEEDS WORK, NOT ACCURATELY CATCHIGN BOLIVIA
prediction <- function(location, languages_by_location_df, web, census) {
  
  temp_1 <<- languages_by_location_df %>%
    mutate(code = language_code_2_twitter, present = presence_pre_langscape, source = "twitter") %>%
    select(name, code, present, source) %>%
    na.omit()
  
  temp_2 <<- web %>%
    mutate(source = "web", present = "present") %>%
    select(name, code, present, source) %>%
    na.omit()
  
  temp_3 <<- bind_rows(temp_1, temp_2)
  
  temp_4 <<- temp_3 %>%
    filter(present == "present") %>%
    distinct(name) %>%
    mutate(predicted = "predicted to exist")
  
  temp_5 <<- census %>%
    select(name) %>%
    mutate(actual = "actually exists")
  
  temp_6 <<- full_join(temp_5, temp_4, by="name") %>%
    select(name, predicted, actual) %>%
    mutate(result_type_tf = case_when(
      !is.na(predicted) & !is.na(actual) ~ "true",
      is.na(predicted) & is.na(actual) ~ "true",
      is.na(predicted) & !is.na(actual) ~ "false",
      !is.na(predicted) & is.na(actual) ~ "false")) %>%
    mutate(result_type_pn = case_when(
      !is.na(predicted) & !is.na(actual) ~ "positive",
      is.na(predicted) & is.na(actual) ~ "negative",
      is.na(predicted) & !is.na(actual) ~ "negative",
      !is.na(predicted) & is.na(actual) ~ "positive"))
  
  temp_7 <<- temp_6 %>%
    group_by(result_type_tf, result_type_pn) %>%
    summarise(count = n()) %>%
    arrange(desc(result_type_pn))
  
  temp_8 <<- temp_7 %>%
    filter((result_type_tf == "true" & result_type_pn == "positive") | (result_type_tf == "false" & result_type_pn == "negative")) %>%
    ungroup() %>% 
    mutate(percent_accuracy = round(count/sum(count)*100, 2)) %>%
    mutate(type = paste0(result_type_tf,"-",result_type_pn)) %>%
    select(type, percent_accuracy)
  
  assign(paste0(location, "_accuracy"), temp_8, envir = .GlobalEnv)
  assign(paste0(location, "_matrix"), temp_6, envir = .GlobalEnv)
  
  
  
}

prediction("UK", languages_by_location_UK, london_web, london_census)
prediction("NYC", languages_by_location_NY, nyc_web, nyc_census)
prediction("Bolivia", languages_by_location_Bolivia, bolivia_web, bolivia_census)
prediction("UK", languages_by_location_UK, london_web, london_census)

