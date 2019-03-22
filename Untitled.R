#### Project to use Twitter to detect languages in a given area

## install.packages("rtweet")
install.packages("glue")

## load packages
library(rtweet)
library(tidyverse)
library(glue)

## Create token
create_token(
  app = "smussenden_twitter_app",
  consumer_key = "ckIXDdBqR7IPkLy35jUkwUM7W",
  consumer_secret = "51uCttObRrpjfEsnoCVRx4VBm1T07yotOeVZO3rmqc3uCbmpRH",
  access_token = "4166101-SRVESlFuv14ghbHcFoUdelDdnJQKArZp9iF3Ut35GU",
  access_secret = "Qo6hEh1ugFMvLZQUD2JB08nJRzv7ERYPjUxpXuGmjBdKX")

## Create a list of language codes to loop through
## Pull list of languages from RTWEET package
languages <- langs
## Filter out languages that don't have an alpha value
languages <- langs %>%
  filter(!is.na(alpha)) %>%
  ### Comment this out to get full list
  head()


for (language_code in c("en","ja")) {
  print(paste("The year is", language_code))
}

for (language_code in c("en","ja")) {
  print(search_tweets("", n = 100, include_rts = FALSE, lang = language_code, geocode = "51.50,0.15,20mi") )
}

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