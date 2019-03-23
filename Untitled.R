#### Project to use Twitter to detect languages in a given area

## install.packages("rtweet")
## install.packages("glue")
## install.packages("data.table")
## install.packages("httr")
## install.packages("httpuv")
## install.packages("jsonlite")
## install.packages("tidyverse")
## install.packages("rtweet")

#############################################
############### load packages ###############
#############################################

# For connecting directly to Twitter API
library(httr)
library(httpuv)
library(jsonlite)

# Rtweet package for easier connection to search API
library(rtweet)

# Data wrangling
library(tidyverse)

#############################################
###### Connect to Twitter API with HTTR #####
#############################################

# Modified version of directions here: 
# https://github.com/r-lib/httr/blob/master/demo/oauth1-twitter.r

# First, need to register an app at https://apps.twitter.com/ for use with httr.
# The directions say, in the app, to make sure to set callback url to "http://127.0.0.1:1410/".  That didn't work for me, kept getting 403 errors (forbidden), which was a sign that there was a problem with callback URL.  It was only after I added every possible itteration of that URL, including localhost/1410, that it actually worked! You can add up to 10 callback URLs, so add as many as you want. This is what I put. https://localhost:1410, http://127.0.0.1:1410/, https://127.0.0.1:1410, http://localhost:1410, https://localhost:1410/, http://localhost:1410/, https://127.0.0.1:1410/, http://127.0.0.1:1410

# Add in app name, consumer API key and consumer secret key, and store as myapp.
myapp <- oauth_app("smussenden_twitter_app",
                   key = "ckIXDdBqR7IPkLy35jUkwUM7W",
                   secret = "51uCttObRrpjfEsnoCVRx4VBm1T07yotOeVZO3rmqc3uCbmpRH"
)

# Get the necessary OAuth credentials by passing in my keys, and storing as twitter token
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

#############################################
## Pull Twitter languages into data frame ###
#############################################

# Pull down the list of of languages used on Twitter, passing in my authentication credentials. 
twitter_languages <- GET(
  "https://api.twitter.com/1.1/help/languages.json",
  config(token = twitter_token)
)

# Coerce the lists of lists that's returned into a dataframe by grabbing the content of the GET request, converting it to json, then from json into a dataframe. There may be a way to conver the list of lists to a dataframe directly, but this works fine!
twitter_languages_df <- twitter_languages %>%
  content() %>%
  toJSON() %>%
  fromJSON()

# Write it out to a CSV. We can load it directly next time we run it, without connecting to the API
write_csv(twitter_languages_df, "twitter_supported_languages.csv")

# Once I'm finished, remove the twitter_token from the environment, cause I'm going to create a new one below. Otherwise there are conflicts. 

rm(twitter_token)
rm(myapp)

# If I want it for later, for comparison purposes, the RTWEET package has a list of languages from the UN.  
languages <- langs

#############################################
###### Connect to RTWEET Package ############
#############################################

## Create token to authenticate access to Twitter API via TWEETR. Note I'm using a different app this time.
create_token(
  app = "language_loop",
  consumer_key = "Bi9EEczj73nAjX6Ak0U60Obna",
  consumer_secret = "e62MjCYMaF6oPdrtbbZBExOOzksPtPDKreIhWbr13kQxuFskti",
  access_token = "4166101-o6ZgayHu0rb7mrjrkT6fyarnjRGLmA6iAaZ3pVrqqH",
  access_secret = "0ADqQbxcfYLa0TvNjbJoq85VXqORopz5MQwJDiTt2M8o4"
)

## Loop through 

english <- twitter_languages_df %>%
  filter(code == "en")

for (language_code in english$code) {
  rt_working <- as_tibble(search_tweets("", n = 10, include_rts = FALSE, lang = language_code, geocode = "51.50,0.15,20mi"))
}

## Create empty dataframe

lang_loc <- tibble(
  language_code = "",
  geocode = "",
  present = ""
)
language_code = "en"
geocode = "51.50,0.15,20mi"
hr_test <- hr %>%
  mutate(language_code = language_code) %>%
  mutate(geocode = geocode) %>%
  mutate(present = "not present") %>%
  select(language_code, geocode, present)


## Now, loop through all of the languages in the twitter supported languages. 
for (language_code in twitter_languages_df$code) {
  geocode = "51.50,0.15,20mi"
  rt_working <- as_tibble(search_tweets("", n = 1, include_rts = FALSE, lang = language_code, geocode = geocode))
  if(nrow(rt_working) > 0) {
    ## assign(language_code, as_tibble(rt_working))
    rt_working <- rt_working %>%
      mutate(language_code = language_code) %>%
      mutate(geocode = geocode) %>%
      mutate(present = "present") %>%
      select(language_code, geocode, present)
    lang_loc <- bind_rows(lang_loc, rt_working)
    print(paste0(language_code, ", present"))
  } else {
    ## assign(paste0(language_code, "empty"), as_tibble(rt_working))
    rt_working <- rt_working %>%
      mutate(language_code = language_code) %>%
      mutate(geocode = geocode) %>%
      mutate(present = "not present") %>%
      select(language_code, geocode, present)
    lang_loc <- bind_rows(lang_loc, rt_working)
    print(paste0(language_code, ", blank"))
  }
  
  
}
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