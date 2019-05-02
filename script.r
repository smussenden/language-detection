#############################################
####### Language Detection Project ##########
#############################################

# This project is part of an effort to use publicly available information on the web -- including information on social networks like Twitter -- to determine what languages are spoken in a given geographic area.


#############################################
####### Load Required Packages ##############
#############################################
source("functions/load_packages.r")


rm(list=ls())

#############################################
###### Connect to Twitter API  ##############
#############################################

# Load the file twitter_keys/httr-keys.R that contains your authentication keys to connect to Twitter. This file has not been included in the repo, so you'll need to make your own. 

# In the folder twitter_keys, create a new file called httr-keys.R.  In that file, paste in a function with your app name, consumer API key and consumer secret key. The function should look like this, subsitituting your app name, key and secret.

# myapp <- oauth_app("app_name",
#                   key = "pastekeyinhere",
#                   secret = "pastesecretinhere")

# Directions on how to make an app and get the key and secret are here: https://github.com/r-lib/httr/blob/master/demo/oauth1-twitter.r. As those directions note, you need to register an app at https://apps.twitter.com/ for use with HTTR package. One note, if you follow directions linked above. The directions say to make sure to set the app's callback url to "http://127.0.0.1:1410/".  That didn't work for us, kept getting 403 errors (forbidden), which was a sign that there was a problem with callback URL.  It was only after we added every possible itteration of that URL, including localhost/1410, that it actually worked! You can add up to 10 callback URLs, so add as many as you want. This is what we put. https://localhost:1410, http://127.0.0.1:1410/, https://127.0.0.1:1410, http://localhost:1410, https://localhost:1410/, http://localhost:1410/, https://127.0.0.1:1410/, http://127.0.0.1:1410.  This step is necessary, because it will launch a browser window and have you authenticate with twitter.

# Load the Twitter keys file you created
source("twitter_keys/httr-keys.R")

##############################################################
###### Get List of Twitter Supported Languages  ##############
##############################################################

# The function below will return two dataframes.  twitter_lanugages (all of the languages twitter supports, with two letter code), and crosswalk_twitter (which will allow us to compare three letter language code from langscape output to two letter twitter language codes)

source("functions/get_twitter_supported_languages.r")


#############################################
###### Connect to RTWEET Package ############
#############################################

# In the folder twitter_keys, create a new file called rtweet-keys.R. In that file, paste in a function with your app name, consumer key, consumer secret key, access token and access secret. Note: you'll need to create a separate app from the one you used to get Twitter supported languages above, but the directions are the same. The function should look like this, subsitituting your info. 

# create_token(
#  app = "yourappname",
#  consumer_key = "pastekeyinhere",
#  consumer_secret = "pastesecretinhere",
#  access_token = "pastetokeninhere",
#  access_secret = "pastesecretinhere"
#)

# Load the file rtweet-keys.R that contains your authentication keys to connect to Twitter. It will create a token with your information.

source("twitter_keys/rtweet-keys.R")

#################################################################
###### Load Twitter Search for Languages Functions ##############
#################################################################

# The language search function will take lat long coordinates you feed it and a radius around those coordinates and loop through all of the languages Twitter supports searching for a tweet in each language.  If it finds a tweet for a given language, it will make a note that Twitter considered the language to be "present" in that area.  If it doesn't find a tweet, it will consider the language to be "not present".  As a second verification step, it will store the text of the tweet and run it through the Lanugage Science Center's Langscape tool to see what language it thinks the Tweet is.  If it matches what Twitter thinks the language is, it will note that.  

# When executed, this function returns two dataframes.  Languages_by_location_LOCATIONNAME shows for each language Twitter supports whether or not the language was found in that location and whether or not it was confirmed by Langscape.  Language_tweets_LOCATION shows what the text of the tweet was for each language identified by Twitter, and for tweets confirmed by Langscape, what the correspondening language code was. 

source("functions/language_search.R")

# The rate check function is necessary to run after you run the language_search function for each city. With the lowest level Twitter API access, you only get 180 searches every 15 minutes.  Running this function will tell you how many searches you have remaining in each 15 minute block.  You can essentially run two areas in each 15 minute block.

source("functions/rate_check.R")

#############################################
#### Search for Languages Using Function ####
#############################################

# language_search(city_name, country_name, geocode) takes three arguments. Each argument must be in quotes. city_name and country_name aren't actually used in the search, they're just there for the output dataframe. geocode must be in a specific format. Latitude and Longitude of specific point, followed by radius around that point in miles."51.50,0.15,20mi". This is finicky. No spaces.

# To find lat and long and a radius, use this tool
# https://www.mapdevelopers.com/draw-circle-tool.php

# London
language_search("London", "UK", "51.50,0.15,20mi")
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

# These tables contain information on language use in each target area found through means other than analyzing Twitter, including meetup groups, court translation and other sources. They're used to buttress Twitter data.

bolivia_web <- read_csv("data/web-research/bolivia-web.csv")
tajikistan_web <- read_csv("data/web-research/tajikistan-web.csv")
london_web <- read_csv("data/web-research/london-web.csv")
nyc_web <- read_csv("data/web-research/nyc-web.csv")

#############################################
#### Read in Census Data ####################
#############################################

# This is the ground truth data on language use, which we'll use to test accuracy of our predictions about language use

bolivia_census <- read_csv("data/census/bolivia-census.csv")
tajikistan_census <- read_csv("data/census/tajikistan-census.csv")
london_census <- read_csv("data/census/london-census.csv")
nyc_census <- read_csv("data/census/nyc-census.csv")

#############################################
#### Load Prediction Function ###############
#############################################
# This function takes the results of our Twitter search (and Langscape varification), adds the "signals of language use on the web" for each area to the Twitter data and makes a prediction for each language on whether or not it should exist. It then compares the results to the ground truth data and for each language determines whether or not the prediction was correct.  It produces two data frames: a matrix indicating whether true positive, false positive or false negative. There are no true negatives in this data. And then in a separate dataframe it computes an accuracy score, based on false negative and true positive. True positive is accuracy.
# We compute for with langscape check and without.


####THIS NEEDS WORK, NOT ACCURATELY CATCHIGN BOLIVIA
source("functions/prediction.R")

# Without langscape confirmation
prediction_without_langscape("UK", languages_by_location_UK, london_web, london_census)
prediction_without_langscape("NYC", languages_by_location_NY, nyc_web, nyc_census)
prediction_without_langscape("Bolivia", languages_by_location_Bolivia, bolivia_web, bolivia_census)
prediction_without_langscape("Tajikistan", languages_by_location_Tajikistan, tajikistan_web, tajikistan_census)

# With langscape confirmation
prediction_with_langscape("UK", languages_by_location_UK, london_web, london_census)
prediction_with_langscape("NYC", languages_by_location_NY, nyc_web, nyc_census)
prediction_with_langscape("Bolivia", languages_by_location_Bolivia, bolivia_web, bolivia_census)
prediction_with_langscape("Tajikistan", languages_by_location_Tajikistan, tajikistan_web, tajikistan_census)
