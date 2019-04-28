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
## may not need install.packages("curl")

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

# Connecting to langscape tool
# may not needlibrary(curl)
library(httr)
library(openxlsx)
# install.packages('qdapRegex')
library(qdapRegex)


# For Debugging rm(list=ls())

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

## Create an empty dataframe, which will store our presence of language results.

languages_by_location <- tibble(
  language_code = "",
  geocode = "",
  present = "",
  city = "",
  country = ""
)

languages_by_location <- languages_by_location[-1,]

## Create an empty dataframe, which will store the tweets that flagged the presence of a given language. Pull down one tweet, then remove the values, preserving the columns.

language_tweets <- as_tibble(search_tweets("", n=1, include_rts=FALSE, geocode="40.71,-74.01,20mi"))

language_tweets <- language_tweets[-1,]

language_tweets <- language_tweets %>%
  select(screen_name, text, lang, name, location, description, account_lang, place_name, place_full_name, place_type, country, country_code, everything())

# Define a function called language search that allows you to pass the name of the city, the name of a country, and a circle centered on an latitutde, longitude pair.  The name of the city and country don't actually matter, they're just for inclusion in the dataframe.  What really matters is the geocode.

language_search <- function(city_name, country_name, geocode) {
  # loop through each language code in our languages dataframe
  for (language_code in twitter_languages$code) {
    # create a dataframe called rt_working that searches the target geographic area for one tweet with any text that uses the language in question.
    rt_working <- as_tibble(search_tweets("", n = 1, include_rts = FALSE, lang = language_code, geocode = geocode))
    # The dataframe will have either 1 row, if it detects a tweet in a language, or 0 rows, if it fails to find a tweet in that language.
    # If it has 1 row, save the tweet in a separate dataframe, and modify rt_working to indicate the language is present.
    if(nrow(rt_working) > 0) {
      # Bind the tweet content to a dataframe
      rt_working <- rt_working %>%
      select(screen_name, text, lang, name, location, description, account_lang, place_name, place_full_name, place_type, country, country_code)
      language_tweets <- bind_rows(language_tweets, rt_working)
      # Modify the rt_wroking dataframe to indicate presence of language
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
  language_tweets <<- language_tweets

}

#############################################
#### Search for Languages Using Function ####
#############################################



# language_search(city_name, country_name, geocode) takes three arguments. Each argument must be in quotes. city_name and country_name aren't actually used in the search, they're just there for the output dataframe. geocode must be in a specific format. Latitude and Longitude of specific point, followed by radius around that point in miles."51.50,0.15,20mi". This is finicky. No spaces.

# To find lat and long and a radius, use this tool
# https://www.mapdevelopers.com/draw-circle-tool.php

# London
language_search("London", "UK", "51.50,0.15,20mi")

# Before moving on, check and see how many tweets we can continue collecting before hitting rate limit, and time remaining before rate limit resets (every 15 minutes, 180 tweets max). Build a function called rate_check, then run rate_check.  In the future, could write a function to check if there's time left before rate limit expires, and, if so, move on.

rate_check <- function() {
  token <- get_tokens()
  rate <- rate_limit(token, "search_tweets")
  View(rate)
}

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


######################################
#### Write out DF of Language Tweets##
######################################

# Remove List Columns before piping to Excel
language_tweets_df <- language_tweets %>%
  select_if(~!is.list(.))

# Create empty Excel workbook
wb <- createWorkbook("workbook")

# Add an empty worksheet
addWorksheet(wb, "tweets")

# Write dataframe to worksheet
writeData(wb, sheet = 1, language_tweets_df)

# Save Workbook
saveWorkbook(wb, "tweets.xlsx", overwrite = TRUE)

#####################################
### Check results of language Tweets#
#####################################

# After list of tweets returned by location functions finished running, bind it back to twitter_langugages dataframe to get names.

language_tweets_copy <- language_tweets %>%
  full_join(twitter_languages, by = c("lang" = "code")) %>%
  rename(name = name.x, language_name = name.y) %>%
  select(language_name, lang, text, everything()) %>%
  filter(!is.na(text))

#### Parse text
language_tweets_copy <- language_tweets_copy %>%
  mutate(text = str_replace_all(text,
                                pattern=regex("(www|https?[^\\s]+)"),
                                replacement = "")) %>%
  mutate(text = rm_hash(text)) %>%
  mutate(text = rm_url(text)) %>%
  mutate(text = rm_twitter_url(text)) %>%
  mutate(unique_id = rownames(language_tweets_copy)) %>%
  select(unique_id, everything())

#### Pipe out to Langscape ID Tool

# Select only columns we need
language_tweets_simple <- language_tweets_copy %>%
  select(unique_id, language_name, lang, text)
# For now, select one row
sample <- language_tweets_simple %>%
  filter(unique_id == 1)
# Create a copy of tweet text with %20 instead of spaces, for passing through URL string.
sample <- sample %>%
  mutate(encoded_text = str_replace_all(sample$text, "\\ ", "%20"))
# Define variables to build url with query string for connecting to langscape tool langscape <- "http://langscape.umd.edu/php/LID_Exec.php?sampleText="
post_url <- paste0(langscape, sample$encoded_text)

# Using HTTR, post the url to the langscape tool
post_langscape <- POST(post_url)

# Convert the response from a list to dataframe
response_langscape <- enframe(content(post_langscape, as = "text"))

# split the langscape language id response scores into a dataframe, and just keep the language code for the top response.

response_langscape <- response_langscape %>%
  select(-name) %>%
  separate(value, sep="\\^", into=c("a","b","c","d","e")) %>%
  gather() %>%
  separate(value, sep="\\|", into=c("source1","source2","score")) %>%
  mutate(language_code = str_sub(source1, 1,3)) %>%
  select(language_code, score) %>%
  arrange(desc(score)) %>%
  slice(1) %>%
  select(language_code)

# bind it back to the dataframe in question



mutate(value = str_replace(value,"\\^","&"))

test6 <- list(test2)
str_split(test6,"^")
print(test2)
test3 <- enframe(test2)
test4 <- test3 %>%
  separate(value, sep="/^", into = c("a", "b", "c", "d", "e"))
test3$value
test4 <- test3 %>%


  row
str_replace(test3$value,'^', '!') %>%
  str_split(test3$value, '!')
separate(value, c("a","b","c","d","e"), sep="|")


# After all location functions run and added to present v not present dataframe, bind it back to twitter_languages dataframe to get language names.
languages_by_location_2 <- twitter_languages %>%
  right_join(languages_by_location, by = c("code" = "language_code")) %>%
  select(country, city, geocode, name, code, present) %>%
  arrange(country, city, geocode, name)







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
#### Filter out Bolivia Data ################
#############################################

bolivia_tweet_languages <- languages_by_location_2 %>%
  filter(country == "Bolivia") %>%
  filter(city == "LaPaz") %>%
  select(name, code, present) %>%
  mutate(source = "twitter")

bolivia_web <- bolivia_web %>%
  mutate(source = "web", present = "present") %>%
  select(name, code, present, source)

bolivia_merged <- bind_rows(bolivia_tweet_languages, bolivia_web)

# Check against merged data

bolivia_present_languages <- bolivia_merged %>%
  filter(present == "present") %>%
  distinct(name) %>%
  mutate(predicted = "predicted to exist")

bolivia_census_languages <- bolivia_census %>%
  select(name) %>%
  mutate(actual = "actually exists")

check <- full_join(bolivia_census_languages, bolivia_present_languages, by="name") %>%
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

check_count <- check %>%
  group_by(result_type_tf, result_type_pn) %>%
  summarise(count = n()) %>%
  order_by(desc(result_type_pn))

#############################################
#### Filter out NEW YORK Data ################
#############################################

nyc_tweet_languages <- languages_by_location_2 %>%
  filter(country == "NY") %>%
  select(name, code, present) %>%
  mutate(source = "twitter")

nyc_web <- nyc_web %>%
  filter(source_type != "public library catalog") %>%
  mutate(source = "web", present = "present") %>%
  select(name, code, present, source)

# Filter out library catalog data

nyc_merged <- bind_rows(nyc_tweet_languages, nyc_web)

# Check against merged data

nyc_present_languages <- nyc_merged %>%
  filter(present == "present") %>%
  distinct(name) %>%
  mutate(predicted = "predicted to exist")

nyc_census_languages <- nyc_census %>%
  select(name) %>%
  mutate(actual = "actually exists")

check <- full_join(nyc_census_languages, nyc_present_languages, by="name") %>%
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

check_count <- check %>%
  group_by(result_type_tf, result_type_pn) %>%
  summarise(count = n()) %>%
  order_by(desc(result_type_pn))


#############################################
#### Filter out TAJIKISTAN Data ################
#############################################

tajikistan_tweet_languages <- languages_by_location_2 %>%
  filter(country == "Tajikistan") %>%
  select(name, code, present) %>%
  mutate(source = "twitter")

tajikistan_web <- tajikistan_web %>%
  mutate(source = "web", present = "present") %>%
  select(name, code, present, source)

tajikistan_merged <- bind_rows(tajikistan_tweet_languages, tajikistan_web)

# Check against merged data

tajikistan_present_languages <- tajikistan_merged %>%
  filter(present == "present") %>%
  distinct(name) %>%
  mutate(predicted = "predicted to exist")

tajikistan_census_languages <- tajikistan_census %>%
  select(name) %>%
  mutate(actual = "actually exists")

check <- full_join(tajikistan_census_languages, tajikistan_present_languages, by="name") %>%
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

check_count <- check %>%
  group_by(result_type_tf, result_type_pn) %>%
  summarise(count = n()) %>%
  order_by(desc(result_type_pn))



check_count <- check %>%
  group_by(result_type_tf, result_type_pn) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  add_row(check_count, result_type_tf = "true", result_type_pn = "negative", count="")

check_count <- check_count %>%
  add_row(check_count, result_type_tf = "true", result_type_pn = "negative", count="")


check_count_spread <- check_count %>%
  spread(result_type_pn, result_type_tf)

check_matrix <- tribble(
  ~x, ~true, ~false,
  "positive", NA, NA,
  "negative", NA, NA
)

check_matrix <- check_matrix %>%
  mutate(true = case_when(
    x == "positive" ~ (filter(check_count$result_type = "true positive") %>% check_count$count))
  )

mutate(result_type = case_when(
  !is.na(predicted) & !is.na(actual) ~ "true positive",
  is.na(predicted) & is.na(actual) ~ "true negative",
  is.na(predicted) & !is.na(actual) ~ "false negative",
  !is.na(predicted) & is.na(actual) ~ "false positive (may exist as other in census data)",
))

  check %>%
  mutate(x = "", true = "", false = "") %>%
  select(x, true, false) %>%






assignments %>%
  count(title, consensus, wt = count) %>%
  spread(consensus, n, fill = 0)



#### CURL

sample <- language_tweets_2 %>%
  filter(language_name == 'French', screen_name == "sarahemnt")

sample_text <- " يم ڪيو ايم: متان ڀائو وارن جي پٺِيان لڳا آه ي"

sample_text <- str_replace_all(sample$text, "\\ ", "%20")

يم ڪيو ايم: متان ڀائو وارن جي پٺِيان لڳا آهيو!

sample_text <- "When%20examining%20the%20scores,%20the%20difference%20in%20score%20between%20languages%20is%20a%20guide%20to%20the%20certainty%20of%20the%20match.%20For%20example,%20if%20the%20top%20score%20is%20twice%20as%20high%20as%20the%20second%20score,%20the%20first%20language%20identified%20is%20probably%20correct.%20If%20the%20scores%20for%20the%20suggested%20languages%20are%20similar,%20this%20may%20be%20because%20the%20languages%20are%20closely%20related,%20but%20more%20commonly%20suggests%20that%20the%20algorithm%20did%20not%20find%20a%20good%20match"
langscape <- "http://langscape.umd.edu/php/LID_Exec.php?sampleText="
post_url <- paste0(langscape, sample_text)
test <- POST(post_url)
test2 <- content(test, as = "text")
test3 <- enframe(test2)
test3 <- test3 %>%
  separate(value, sep="\\^", into=c("a","b","c","d","e"))
test4 <- test3 %>%
  gather()
test5 <- test4 %>%
  filter(key != "name")
test6 <- test5 %>%
  separate(value, sep="\\|", into=c("source1","source2","score")) %>%
  mutate(language_code = str_sub(source1, 1,3)) %>%
  select(key, language_code, score, source2, -source1)

 mutate(value = str_replace(value,"\\^","&"))

test6 <- list(test2)
str_split(test6,"^")
print(test2)
test3 <- enframe(test2)
test4 <- test3 %>%
  separate(value, sep="/^", into = c("a", "b", "c", "d", "e"))
test3$value
test4 <- test3 %>%


row
  str_replace(test3$value,'^', '!') %>%
  str_split(test3$value, '!')
  separate(value, c("a","b","c","d","e"), sep="|")





test <- POST("http://langscape.umd.edu/php/LID_Exec.php", body="sampleText=When examining the scores, the difference in score between languages is a guide to the certainty of the match. For example, if the top score is twice as high as the second score, the first language identified is probably correct. If the scores for the suggested languages are similar, this may be because the languages are closely related, but more commonly suggests that the algorithm did not find a good match", encode = "form")



test_2 <- as_tibble(content(test, as="parsed"))


GET("http://langscape.umd.edu/php/LID_Exec.php", verbose())
library(httr)
login <- list(
  email = "login",
  password = "password"
  submit = "Login!"
)
res <- POST("http://kenpom.com/handlers/login_handler.php", body = login, encode = "form", verbose())
team <- GET("http://kenpom.com/team.php?team=Rice", verbose())

con <- curl("http://langscape.umd.edu/php/LID_Exec.php")
readLines(con)
curl_options()


# Posting multipart
h <- new_handle()
handle_setform(h,
               foo = "blabla",
               bar = charToRaw("boeboe"),
               iris = form_data(serialize(iris, NULL), "application/rda"),
               description = form_file(system.file("DESCRIPTION")),
               logo = form_file(file.path(R.home('doc'), "html/logo.jpg"), "image/jpeg")
)
req <- curl_fetch_memory("https://eu.httpbin.org/post", handle = h)

  mutate(false_positive, false_negative, true_positive, true_negative)

  library(dplyr) # >= 0.7.0
  mtcars %>%
    mutate(cg = case_when(carb <= 2 ~ "low",
                          carb > 2  ~ "high"))

  full_join(bolivia_census, bolivia_merged, by = "name")

  mutate(gradebook, letter = ifelse(grade %in% 60:69, "D",
                                    ifelse(grade %in% 70:79, "C",
                                           ifelse(grade %in% 80:89, "B",
                                                  ifelse(grade %in% 90:99, "A", "F")))))
