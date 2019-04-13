#############################################
####### Language Detection Project ##########
#############################################

# This project is part of an effort to use publicly available information on the web -- including information on social networks like Twitter -- to determine what languages are spoken in a given area.  

#############################################
####### Install Required Packages ###########
#############################################

## install.packages("rtweet")
## install.packages("glue")
## install.packages("data.table")
## install.packages("httr")
## install.packages("httpuv")
## install.packages("jsonlite")
## install.packages("tidyverse")
## install.packages("rtweet")
## install.packages("stringi")

#############################################
####### Load Required Packages ##############
#############################################

# For connecting directly to Twitter API and parsing output
library(httr)
library(httpuv)
library(jsonlite)
library(stringi)

# For easier connection to search API
library(rtweet)

# Data wrangling
library(tidyverse)

rm(list=ls())

#############################################
###### Connect to Twitter API with HTTR #####
#############################################

# Modified version of directions here: https://github.com/r-lib/httr/blob/master/demo/oauth1-twitter.r. As those directions note, you need to register an app at https://apps.twitter.com/ for use with httr package.

# One note, if you follow directions linked above. The directions say to make sure to set the app's callback url to "http://127.0.0.1:1410/".  That didn't work for me, kept getting 403 errors (forbidden), which was a sign that there was a problem with callback URL.  It was only after I added every possible itteration of that URL, including localhost/1410, that it actually worked! You can add up to 10 callback URLs, so add as many as you want. This is what I put. https://localhost:1410, http://127.0.0.1:1410/, https://127.0.0.1:1410, http://localhost:1410, https://localhost:1410/, http://localhost:1410/, https://127.0.0.1:1410/, http://127.0.0.1:1410.  This step is necessary, because it will launch a browser window and have you authenticate with twitter.

# In the folder twitter_keys, create a new file called httr-keys.R.  In that file, paste in a function with your app name, consumer API key and consumer secret key. The function should look like this, subsitituting your app name, key and secret. 

# myapp <- oauth_app("app_name",
#                   key = "pastekeyinhere",
#                   secret = "pastesecretinhere"

# Load the file httr-keys.R that contains your authentication keys to connect to Twitter. It will load an object called myapp, which contains your keys.
source("twitter_keys/httr-keys.R")

# Get the necessary OAuth credentials by passing in your keys, and storing as twitter token
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

######################################################
## Pull Twitter Suported languages into data frame ###
######################################################

# Pull down the list of of languages used on Twitter, passing in authentication credentials. 

twitter_languages <- GET(
  "https://api.twitter.com/1.1/help/languages.json",
  config(token = twitter_token)
)

# Coerce the lists of lists that's returned into a dataframe by grabbing the content of the GET request, converting it to json, then passing it from json into a dataframe. 

twitter_languages <- twitter_languages %>%
  content() %>%
  toJSON() %>%
  fromJSON()

# Turn it into a dataframe. 

twitter_languages <- as_tibble(stri_list2matrix(twitter_languages, byrow=FALSE))

# Rename the columns that got stripped when we used the stringi list2matrix function. 

twitter_languages <- twitter_languages %>%
  rename(code = V1, name = V2, local_name = V3, status = V4, debug = V5)

# Load in list of additional languages Twitter seems to support that aren't listed in the API.  A CSV was compiled from the twitter search api documentation. https://developer.twitter.com/en/docs/tweets/rules-and-filtering/overview/premium-operators.

twitter_supported <- read_csv('data/twitter-supported-languages/input/search-api-languages.csv')

# Join the two dataframes of Twitter languages together

twitter_languages <- twitter_languages %>%
  full_join(twitter_supported, by = c("code" = "Code")) %>%
  mutate(name = if_else(is.na(name), Language, name)) %>%
  select(code, name)

# Write it out to a CSV. We can load it directly in the future, without connecting to the API.

write.csv(twitter_languages, "data/twitter-supported-languages/output/twitter_languages.csv")

# Remove the twitter token and twitter app.  It will create conflicts when using the RTweet package. Can also remove twitter_supported.

rm(list=setdiff(ls(), "twitter_languages"))


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


#############################################
###### Create Language Search Function ######
#############################################

## Create an empty dataframe, which will store our results. 

languages_by_location <- tibble(
  language_code = "",
  geocode = "",
  present = "",
  city = "",
  country = ""
)



# Define a function called language search that allows you to pass the name of the city, the name of a country, and a circle centered on an latitutde, longitude pair.  The name of the city and country don't actually matter, they're just for inclusion in the dataframe.  What really matters is the geocode. 

language_search <- function(city_name, country_name, geocode) {
  # loop through each language code in our languages dataframe
  for (language_code in twitter_languages$code) {
    # create a dataframe called rt_working that searches the target geographic area for one tweet with any text that uses the language in question.
    rt_working <- as_tibble(search_tweets("", n = 1, include_rts = FALSE, lang = language_code, geocode = geocode))
    # The dataframe will have either 1 row, if it detects a tweet in a language, or 0 rows, if it fails to find a tweet in that language.   
    # If it has 1 row, modify rt_working to indicate the language is present.
    if(nrow(rt_working) > 0) {
      rt_working <<- rt_working 
      rt_working <- rt_working %>%
        mutate(language_code = language_code) %>%
        mutate(geocode = geocode) %>%
        mutate(present = "present") %>%
        mutate(country = country_name) %>%
        mutate(city = city_name) %>%
        select(language_code, geocode, present, country, city)
      # Bind the results to our master dataframe
      languages_by_location <- bind_rows(languages_by_location, rt_working)
    } else {
      # If it has 0 rows, modify rt_working to indicate the language is not present.
      rt_working <- tibble(
        language_code = language_code,
        geocode = geocode,
        present = "not present",
        country = country_name,
        city = city_name
      )
      # Bind the results to our master dataframe
      languages_by_location <- bind_rows(languages_by_location, rt_working)
    }
  }
  # After looping through all the languages, this last step is necessary so that the languages_by_location we've modified inside the function (essentially a copy) overwrites the global languages_by_location that exists outside the function. The double angle bracket <<- instead of normal <- for assignment does that. 
  languages_by_location <<- languages_by_location
}


#############################################
#### Search for Languages Using Function ####
#############################################

# language_search(city_name, country_name, geocode) takes three arguments. Each argument must be in quotes. city_name and country_name aren't actually used in the search, they're just there for the output dataframe. geocode must be in a specific format. Latitude and Longitude of specific point, followed by radius around that point in miles."51.50,0.15,20mi". This is finicky. No spaces. 

# To find lat and long and a radius, use this tool 
# https://www.mapdevelopers.com/draw-circle-tool.php

# Wait 10 minutes or so between runs, to avoid rate limit. 

# London
language_search("London", "UK", "51.50,0.15,20mi")

# New York
language_search("New York", "NY", "40.71,-74.01,20mi")

# Bolivia
language_search("Whole Country", "Bolivia", "-16.23,-64.73,455mi")

# Bolivia
language_search("Oruro", "Bolivia", "-17.95,-67.10,20mi")

-17.947987,-67.092493

# Tajikistan
language_search("Western", "Tajikistan", "38.67,69.23,69mi")


languages_by_location <- languages_by_location %>%
  filter(city != "Western")

# After for loop finished running, bind it back to twitter_languages dataframe. Unnest it first
languages_by_location2 <- twitter_languages %>%
  right_join(languages_by_location, by = c("code" = "language_code")) %>%
  select(country, city, geocode, name, code, present) %>%
  arrange(country, city, geocode, name)


languages_by_location2 <- twitter_languages %>%
  unnest() %>%
  right_join(lang_loc, by = c("code" = "language_code")) %>%
  select(country, city, geocode, name, code, present) %>%
  arrange(country, city, geocode, name)



## Now, loop through all of the languages in the twitter supported languages.
## Adding for loop. 
for (geocode in geocodes$geostring) {
  for (language_code in twitter_languages_df$code) {
    print(geocode)
    city_name <- "London"
    country_name <- "UK"
    rt_working <- as_tibble(search_tweets("", n = 1, include_rts = FALSE, lang = language_code, geocode = geocode))
    if(nrow(rt_working) > 0) {
      ## assign(language_code, as_tibble(rt_working))
      rt_working <- rt_working %>%
        mutate(language_code = language_code) %>%
        mutate(geocode = geocode) %>%
        mutate(present = "present") %>%
        mutate(country = country_name) %>%
        mutate(city = city_name) %>%
        select(language_code, geocode, present, country, city)
      lang_loc <- bind_rows(lang_loc, rt_working)
    } else {
      ## assign(paste0(language_code, "empty"), as_tibble(rt_working))
      rt_working <- tibble(
        language_code = language_code,
        geocode = geocode,
        present = "not present",
        country = country_name,
        city = city_name
      )
      lang_loc <- bind_rows(lang_loc, rt_working)
    }
  }
  ## print("sleep for five minutes to avoid getting rate limited")
  ## Sys.sleep(20)
}



df <- enframe(twitter_languages_df)

df <- twitter_languages_df %>%
  unnest()
glimpse(df)

twitter_languages_tibble <- data.frame(twitter_languages_df)
df <- data.frame(matrix(unlist(twitter_languages_df), nrow=length(twitter_languages_df), byrow=T))
glimpse(twitter_languages_tibble)
lang_loc_verbose <- lang_loc %>%
  left_join(twitter_languages_tibble, by = c("language_code" = "code"))

glimpse(lang_loc)
glimpse(twitter_languages_df)
assign(language_code, as_tibble(rt_working))

language_code <- language_code %>%
  group_by(language_code$lang) %>%
  summarise(count=n())

## Actual one
languages <- langs
## Filter out languages that don't have an alpha value
languages <- langs %>%
  filter(!is.na(alpha)) %>%
  head(20)

### For loop through languages.  I'm getting rate limited, so need to do this in chunks. 
for (language_code in languages$alpha) {
  rt_working <- as_tibble(search_tweets("", n = 1, include_rts = FALSE, lang = language_code, geocode = "51.50,0.15,20mi"))
  assign(language_code, as_tibble(rt_working))
}

### Bind them all together.  I cannot for the life of me figure out how to automate this shit, none of the rowbind functions are working, so fuck it, let's do it manually.
language_code_list <- languages %>%
  mutate(code_list = paste0(alpha,",")) %>%
  select(code_list)

language_code_list <- (noquote(language_code_list$code_list))
View(language_code_list)  
master_data_frame <-bind_rows(list(aa,	ab,	af,	ak,	sq,	am,	ar,	an,	hy,	as,	av,	ae,	ay,	az,	ba,	bm,	eu, be,	bn,	bh,	bi,	bs,	br,	bg,	my,	ca,	ch,	ce,	zh,	cu,	cv,	kw,	co,	cr, cs,	da,	dv,	nl,	dz,	en,	eo,	et,	ee,	fo,	fj,	fi,	fr,	fy,	ff,	ka,	de, gd,	ga,	gl,	gv,	el,	gn,	gu,	ht,	ha,	he,	hz,	hi,	ho,	hr,	hu,	ig,	is, io,	ii,	iu,	ie,	ia,	id,	ik,	it,	jv,	ja,	kl,	kn,	ks,	kr,	kk,	km,	ki, rw,	ky,	kv,	kg,	ko,	kj,	ku,	lo,	la,	lv,	li,	ln,	lt,	lb,	lu,	lg,	mk, mh,	ml,	mi,	mr,	ms,	mg,	mt,	mn,	na,	nv,	nr,	nd,	ng,	ne,	nn,	nb,	no, ny,	oc,	oj,	or,	om,	os,	pa,	fa,	pi,	pl,	pt,	ps,	qu,	rm,	ro,	rn,	ru, sg,	sa,	si,	sk,	sl,	se,	sm,	sn,	sd,	so,	st,	es,	sc,	sr,	ss,	su,	sw, sv,	ty,	ta,	tt,	te,	tg,	tl,	th,	bo,	ti,	to,	tn,	ts,	tk,	tr,	tw,	ug, uk,	ur,	uz,	ve,	vi,	vo,	cy,	wa,	wo,	xh,	yi,	yo,	za,	zu))


rt_working_2 <- rt
rt_working_3 <- rt
for (language_code in languages$alpha) {
  ### Get tweets and save as tibble code here
  ### Paste into data frames
  assign(language_code, as_tibble(rt_working_2))
  ### Paste in
  ###new <- bind_rows(rt_working_3, data.frame(language_code), .id="id")
}

new<-bind_rows(noquote(dataframe_list))


new <- bind_rows(dataframe_list)

### Create a list of dataframes
dataframe_list <- list(languages$alpha)
bind_rows(dataframe_list)
big_old_frame <- rbindlist(dataframe_list)
big_old_frame <- do.call(rbind, dataframe_list)

library(data.table)
> 
  > head(rbindlist(data)


for (language_code in languages$alpha) {
    rt_working <- as_tibble(search_tweets("", n = 100, include_rts = FALSE, lang = language_code, geocode = "51.50,0.15,20mi"))
}

# Save working news URLs to csv files in groups of 100
write_csv(compile_urls("https://www.rt.com/news/", 1, 100), "data/rt_newsURLs-1.csv") # first 100
for(i in 1:5){ # 200-600
  filename <- paste0("data/rt_newsURLs-", i+1, ".csv", "")
  start <- i*100 +1
  end <- (i+1)*100
  
  write_csv(compile_urls("https://www.rt.com/news/", start, end), filename)
}


language_code <- "en"
search_tweet_function <- function(language_code) {
  search_tweets("", n = 100, include_rts = FALSE, lang = glue(' {language_code} '), geocode = "51.50,0.15,20mi")
}


## London

## Test connection
alpha <- "en"
rt <- search_tweets(
  "", n = 100, include_rts = FALSE, lang = alpha, geocode = "51.50,0.15,20mi"
)
rt_map <- map(lang_vector, alpha)
row_bind

## Define my search tweet function for London
language_code <- "en"
search_tweet_function <- function(language_code) {
  search_tweets("", n = 100, include_rts = FALSE, lang = glue(' {language_code} '), geocode = "51.50,0.15,20mi")
}

search_tweet_function(en)

name <- "Fred"
age <- 50
anniversary <- as.Date("1991-10-12")
glue('
     \"My name is {name}
     my age next year is {age + 1}
     my anniversary is {format(anniversary, "%A, %B %d, %Y")}
     ')
  
eval(quote(sean))

## Other
for (i in lang_vector) {
  rt <- search_tweets(
    "", n = 100, include_rts = FALSE, lang = lang_vector, geocode = "51.50,0.15,20mi"
  )
}





-- Idea is to use the search API to take a given location and loop through all of the languages Twitter recognizes on its platform.  If a tweet is returned in that language for a given location, then we create a column that classifies that language as existing in that location.  Ultimately we want four data frames or CSVs of each location, with one row per language that Twitter includes in its list of supported languages and a classification of Y or N whether tweets were found in that location. 

### To Start
Connect to Twitter APi






https://github.com/TheSiddy/INST737Capstone

"""
Created on Mon Mar  4 02:37:41 2019
@author: TheSiddy
"""

import tweepy
from tweepy import Stream
from tweepy import OAuthHandler
import json

consumer_key="Nv0UJjhuhJXAnDNuUl3Rr0fwA"
consumer_secret="m4vHuXoh3YQ3rFJt51bKg3N4DncByrKxFVt3nwMyijRrfkQQZF"
access_token="877892050124976128-E8Fws8Yxn1g4cwQWX7efJAcdv3iYWoJ"
access_secret="CQWKjZZXsIXMQ9pU4tgafNJUAb5ILxMBC0Jk4nUn3SMAi"

auth = OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_secret)

api = tweepy.API(auth)
@classmethod
def parse(cls, api, raw):
  status = cls.first_parse(api, raw)
setattr(status, 'json', json.dumps(raw))
return status

# Status() is the data model for a tweet
tweepy.models.Status.first_parse = tweepy.models.Status.parse
tweepy.models.Status.parse = parse
class MyListener(tweepy.StreamListener):
  
  def on_data(self, data):
  try:
  with open('FILENAME.json', 'a') as f:
  f.write(data)
return True
except BaseException as e:
  print("Error on_data: %s" % str(e))
return True

def on_error(self, status):
  print(status)
return True

#Set the hashtag to be searched
twitter_stream = Stream(auth, MyListener())
twitter_stream.filter(track=['dog'])