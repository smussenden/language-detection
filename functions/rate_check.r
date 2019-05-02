# Function to check how many calls to Twitter Search API you have remaining. 
rate_check <- function() {
  token <- get_tokens()
  rate <- rate_limit(token, "search_tweets")
  View(rate)
}