
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

# Load ISO609-1 and ISO609-2 Crosswalk data

crosswalk <- read_csv("data/twitter-supported-languages/input/iso-code-crosswalk.csv")

# Select first three columns
crosswalk <- crosswalk %>%
  select(1:3)

# Clean column names 
crosswalk <- clean_names(crosswalk)

# Inner join with twitter languages to get only twitter languages on list
crosswalk_twitter <- crosswalk %>%
  inner_join(twitter_languages, by=c("iso639_1code" = "code"))

# Write it out to a CSV. We can load it directly in the future, without connecting to the API.

write.csv(twitter_languages, "data/twitter-supported-languages/output/twitter_languages.csv")
write.csv(crosswalk_twitter, "data/twitter-supported-languages/output/crosswalk_twitter.csv")

# Remove the twitter token and twitter app.  It will create conflicts when using the RTweet package. Can also remove twitter_supported.

rm(list=setdiff(ls(), c("twitter_languages","crosswalk_twitter")))