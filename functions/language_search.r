

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

language_tweets <- as_tibble(search_tweets("", n=1, include_rts=FALSE, geocode="40.54,-74.01,20mi"))

language_tweets <- language_tweets[-1,]

language_tweets <- language_tweets %>%
  select(screen_name, text, lang, name, location, description, account_lang, place_name, place_full_name, place_type, country, country_code, everything())

# twitter_languages <- head(twitter_languages, n=20)

language_search <- function(city_name, country_name, geocode) {
  
  # Reset empty dataframes
  languages_by_location <- languages_by_location %>%
    top_n(0)
  language_tweets <- language_tweets %>%
    top_n(0)
  
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
  ######################################
  ### Check results of language Tweets #
  ######################################
  
  # After list of tweets returned by location functions finished running, bind it back to twitter_langugages dataframe to get names.
  
  language_tweets <- language_tweets %>%
    full_join(twitter_languages, by = c("lang" = "code")) %>%
    rename(username = name.x, full_language_name = name.y)  %>%
    select(lang, full_language_name, text, everything()) %>%
    filter(!is.na(text))
  
  #### Parse text
  language_tweets <- language_tweets %>%
    mutate(text = str_replace_all(text,
                                  pattern=regex("(www|https?[^\\s]+)"),
                                  replacement = "")) %>%
    mutate(text = rm_hash(text)) %>%
    mutate(text = rm_url(text)) %>%
    mutate(text = rm_twitter_url(text)) %>%
    mutate(unique_id = as.numeric(rownames(language_tweets))) %>%
    select(unique_id, everything())
  
  # Select only columns we need, then create a column with %20 instead of spaces in tweet text. 
  language_tweets <- language_tweets %>%
    select(unique_id, full_language_name, lang, text) %>%
    mutate(encoded_text = str_replace_all(language_tweets$text, "\\ ", "%20"))
  
  # make empty dataframe 
  language_tweets_empty <- language_tweets %>%
    top_n(0)
  
  for (i in language_tweets$encoded_text) {
    # Define variables to build url with query string for connecting to langscape tool
    langscape <- "http://langscape.umd.edu/php/LID_Exec.php?sampleText="
    post_url <- paste0(langscape, i)
    
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
    
    
    # Filter just the one row 
    language_tweets_row <- language_tweets %>%
      filter(encoded_text == i)  
    # Bind the code to the crosswalk table
    crosswalk_convert <- response_langscape %>%
      left_join(crosswalk_twitter, by = c("language_code"  = "iso639_2code"))
    # Bind the result to the language_tweets_row   
    language_tweets_row <- language_tweets_row %>%
      left_join(crosswalk_convert, by = c("lang" = "iso639_1code"))
    # Bind to empty dataframe 
    language_tweets_empty <- rbind(language_tweets_empty, language_tweets_row)
  }
  
  # Remove duplicates
  language_tweets <- language_tweets_empty %>%
    distinct()  
  
  # Join the langscape coded tweets back to languages_by_location
  languages_by_location <- languages_by_location %>%
    left_join(language_tweets, by = c("language_code" = "lang"))
  
  languages_by_location_langscape <- languages_by_location %>%
    mutate(langscape_present = case_when(
      is.na(name) ~ "not present",
      TRUE ~ "present")) %>%
    select(-unique_id) %>%
    distinct() %>%
    mutate(presence_pre_langscape = present, presence_post_langscape = langscape_present, tweet_text = text, language_code_3_langscape = language_code.y, language_code_2_twitter = language_code) %>%
    select(language_code_2_twitter, language_code_3_langscape, full_language_name, geocode, city, country, presence_pre_langscape, presence_post_langscape)
  
  languages_by_location_langscape <- languages_by_location_langscape %>%
    left_join(twitter_languages, by = c("language_code_2_twitter" = "code")) %>%
    select(name, language_code_2_twitter, geocode, city, country, presence_pre_langscape, presence_post_langscape)
  
  
  # After looping through all the languages, this last step is necessary so that the languages_by_location we've modified inside the function (essentially a copy) overwrites the global languages_by_location that exists outside the function. The double angle bracket <<- instead of normal <- for assignment does that.
  assign(paste0("languages_by_location_", country_name), languages_by_location_langscape, envir = .GlobalEnv)
  assign(paste0("language_tweets_", country_name), language_tweets, envir = .GlobalEnv)
  
}
