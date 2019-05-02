# Functions to build prediction matrix and accuracy score table. Two versions.  One uses Twitter WITHOUT langscape confirmation.  Other uses Twitter WITH langscape confirmation. 


# without langscape
prediction_without_langscape <- function(location, languages_by_location_df, web, census) {
  
  temp_1 <- languages_by_location_df %>%
    mutate(code = language_code_2_twitter, present = presence_pre_langscape, source = "twitter") %>%
    select(name, code, present, source)
  
  temp_2 <- web %>%
    mutate(source = "web", present = "present") %>%
    select(name, code, present, source) 
  
  
  temp_3 <- bind_rows(temp_1, temp_2)
  
  temp_4 <- temp_3 %>%
    filter(present == "present") %>%
    distinct(name) %>%
    mutate(predicted = "predicted to exist")
  
  temp_5 <- census %>%
    select(name) %>%
    mutate(actual = "actually exists")
  
  temp_6 <- full_join(temp_5, temp_4, by="name") %>%
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
      !is.na(predicted) & is.na(actual) ~ "positive")) %>%
    filter(!is.na(name))
  
  temp_7 <- temp_6 %>%
    group_by(result_type_tf, result_type_pn) %>%
    summarise(count = n()) %>%
    arrange(desc(result_type_pn))
  
  temp_8 <- temp_7 %>%
    filter((result_type_tf == "true" & result_type_pn == "positive") | (result_type_tf == "false" & result_type_pn == "negative")) %>%
    ungroup() %>% 
    mutate(percent_accuracy = round(count/sum(count)*100, 2)) %>%
    mutate(type = paste0(result_type_tf,"-",result_type_pn)) %>%
    select(type, percent_accuracy)
  
  assign(paste0(location, "_accuracy_no_langscape"), temp_8, envir = .GlobalEnv)
  assign(paste0(location, "_matrix_no_langscape"), temp_6, envir = .GlobalEnv)
  
  
  
}

# with langscape

prediction_with_langscape <- function(location, languages_by_location_df, web, census) {
  
  temp_1 <- languages_by_location_df %>%
    mutate(code = language_code_2_twitter, present = presence_post_langscape, source = "twitter") %>%
    select(name, code, present, source)
  
  temp_2 <- web %>%
    mutate(source = "web", present = "present") %>%
    select(name, code, present, source) 
  
  
  temp_3 <- bind_rows(temp_1, temp_2)
  
  temp_4 <- temp_3 %>%
    filter(present == "present") %>%
    distinct(name) %>%
    mutate(predicted = "predicted to exist")
  
  temp_5 <- census %>%
    select(name) %>%
    mutate(actual = "actually exists")
  
  temp_6 <- full_join(temp_5, temp_4, by="name") %>%
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
      !is.na(predicted) & is.na(actual) ~ "positive")) %>%
    filter(!is.na(name))
  
  temp_7 <- temp_6 %>%
    group_by(result_type_tf, result_type_pn) %>%
    summarise(count = n()) %>%
    arrange(desc(result_type_pn))
  
  temp_8 <- temp_7 %>%
    filter((result_type_tf == "true" & result_type_pn == "positive") | (result_type_tf == "false" & result_type_pn == "negative")) %>%
    ungroup() %>% 
    mutate(percent_accuracy = round(count/sum(count)*100, 2)) %>%
    mutate(type = paste0(result_type_tf,"-",result_type_pn)) %>%
    select(type, percent_accuracy)
  
  assign(paste0(location, "_accuracy_with_langscape"), temp_8, envir = .GlobalEnv)
  assign(paste0(location, "_matrix_with_langscape"), temp_6, envir = .GlobalEnv)
  
  
  
}