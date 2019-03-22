install.packages('httpuv')
library(httpuv)
library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at
#    https://github.com/settings/developers. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("github",
                   key = "78064c657f967fdfcc42",
                   secret = "1193f5878ddb2c1112cf00c2c072fb1082379a5a"
)

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)

# OR:
req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
stop_for_status(req)
content(req)

#### Project to use Twitter to detect languages in a given area

## install.packages("rtweet")
install.packages("glue")
install.packages("data.table")
## load packages
library(rtweet)
library(tidyverse)
library(glue)
library(data.table)
library(httr)

## Create token
create_token(
  app = "smussenden_twitter_app",
  consumer_key = "ckIXDdBqR7IPkLy35jUkwUM7W",
  consumer_secret = "51uCttObRrpjfEsnoCVRx4VBm1T07yotOeVZO3rmqc3uCbmpRH",
  access_token = "4166101-SRVESlFuv14ghbHcFoUdelDdnJQKArZp9iF3Ut35GU",
  access_secret = "Qo6hEh1ugFMvLZQUD2JB08nJRzv7ERYPjUxpXuGmjBdKX")


library(httr)

# 1. Find OAuth settings for twitter:
#    https://dev.twitter.com/docs/auth/oauth
oauth_endpoints("twitter")

# 2. Register an application at https://apps.twitter.com/
#    Make sure to set callback url to "http://127.0.0.1:1410/"
#
#    Replace key and secret below
myapp <- oauth_app("twitter",
                   key = "fLJr0HHU9NCOKSPsozDI3j17W",
                   secret = "cizBpyVkfkbOaor5rFrqTT1k8eKicZ9N9RZLyCqdxuwxhzEoRk"
)

# 3. Get OAuth credentials
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp, as_header = TRUE)

# 4. Use API
req <- GET(
  "https://api.twitter.com/1.1/statuses/home_timeline.json",
  config(token = twitter_token)
)
stop_for_status(req)
content(req)


## Create a list of language codes to loop through
## Pull list of languages from RTWEET package
languages <- langs

## Create a test data frame just for english, so I don't get rate limited while debugging.
english <- languages %>%
  filter(alpha == "en")

for (language_code in english$alpha) {
  rt_working <- as_tibble(search_tweets("", n = 10, include_rts = FALSE, lang = language_code, geocode = "51.50,0.15,20mi"))
}

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