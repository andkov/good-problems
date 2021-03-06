rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default unless overwritten

# ---- load-sources ------------------------------------------------------------

source("./scripts/common-functions.R")

# ---- load-packages -----------------------------------------------------------
library(magrittr)  # pipes
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates

# ---- declare-globals ---------------------------------------------------------

# ---- declare-functions -------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
# consider a scenario in which a group of respondents asked a survey question
# "Did you have lunch today?" (item `b4`)
b4_responses <- c("1" = "yes", "2" = "no", "-7" = "Q not asked")
# If "yes", then asked "What fruit did you eat with your lunch?"
# recording responses into two items (`b4_response1`,`b4_response2`)
# each with the following possible values:
fruit <- c("1" = "apple", "2" = "banana", "3" = "kiwi", "97" = "other")
# Each time a fruit was named, follow up with question
# "How much did you like it?"
b4_ratings <- c("1"="Very Bad", "2"="Bad","3"="Neutral", "4"="Good","5"="Very Good")
# the following data set stores a sample of responses
ds0 <-
  tibble::tribble(
    ~id, ~b4, ~b4_response1, ~b4_response2, ~b4_rating1, ~b4_rating2,
    1, 1 ,  1,  2   , 1 , 4 ,      
    2, 2 , NA, NA   ,NA ,NA ,      
    3, 1 ,  2, 3    , 3 , 4 ,      
    4, 1 ,  97, -7  ,1 ,  NA,      
    5, 1 ,  3, 97   , 3 , 5 ,      
    6, 1 ,  NA, 2   ,NA , 4 ,      
    7, -7, -7, -7   ,NA , NA,  
    8, 1 ,  97, 97  , 2 , 4 ,      
  ) %>%
  mutate_all(as.integer)
ds0
valid_responses <- c(1,2,3,97)

# ---- solution-1-basic --------------------------------------------------------
# of people who had lunch and provided at least one valid response
# what percent of people had apple/banana/kiwi for lunch?
# Step 1: Isolate variables to be evaluated
(ds1 <- ds0 %>% select(id, b4_response1, b4_response2, b4_rating1, b4_rating2))

ds2 <-
  ds1 %>%
  tidyr::pivot_longer(-id ) %>%
  mutate(
    item_type = case_when(
      name %in% c("b4_response1","b4_response2") ~ "response"
      ,name %in% c("b4_rating1","b4_rating2") ~ "rating"
      )
    ,item_order = case_when(
      name %in% c("b4_response1","b4_rating1") ~ 1
      ,name %in% c("b4_response2","b4_rating2") ~ 2
      # TODO: figure out a way to automate for list of N pairs
    )
  ) %>%
  select(-name) %>%
  pivot_wider(names_from = "item_type", values_from = "value")
ds1
ds2
# browser()
ds3 <-
  ds2 %>%
  mutate(
    response = case_when(response %in% c(1,2,3,97) ~ response)
    ,rating  =  case_when(rating %in% c(1,2,3,4,5) ~ rating)
    # ,response_valid = case_when(response %in% valid_responses ~ TRUE,TRUE ~ FALSE)
  ) %>%
  # remove if provided save value for all responses
  group_by(id) %>%
  mutate(
    same_responses = n_distinct(response)==1L
  ) %>%
  ungroup() %>%
  filter(!same_responses) %>%  # b/c responses from two slots should be different
  filter(!is.na(response)) %>% # so that pivoting produced proper results
  mutate(
    item_response = paste0("b4_",response)
  )
ds3

ds3_response <-
  ds3 %>%
  select(id, item_response, response) %>%
  mutate(
    # name = paste0(item_response, "_response") # to make self-explanatory
    name = paste0(item_response, "_a")          # to help view in console
  ) %>%
  select(-item_response) %>%
  pivot_wider(names_from = "name", values_from = "response",
              values_fn=is.integer, values_fill = FALSE)
ds3_response 
ds3_rating <-
  ds3 %>%
  select(id, item_response, rating) %>%
  mutate(
    # name = paste0(item_response, "_rating") # to make self-explanatory
    name = paste0(item_response, "_b")        # to help view in console      
  ) %>%
  select(-item_response) %>%
  pivot_wider(names_from = "name", values_from = "rating", values_fill = -99)
ds3_rating

ds4 <-
  ds1 %>%
  left_join(
    ds3_response
    ,by = "id"
  ) %>%
  left_join(
    ds3_rating
    ,by = "id"
  )
ds4

# ----- solution-1-functional --------------------------------------------------
rm(ds1,ds2,ds3,ds4)
make_indicator_responses_ratings <- function(
  d
  ,item_names
  ,valid_responses
  ,valid_ratings
  ,id_name
  ,item_stem
  ,separator = "_"
  ,keep_responses = TRUE
  ,keep_ratings = TRUE
){
  # d <- ds0
  # d <- ds1
  # item_names = list(c("b4a_resp1","b4b1"),c("b4a_resp2","b4b2"))
  # valid_responses = c(1,2,3,97)
  # valid_ratings   = c(1,2,3,4,5)
  # id_name = "survey_oid"
  # item_stem = "b4"
  # separator = "_"
  # keep_responses = TRUE
  # keep_ratings = TRUE
  
  # compute values to be used in operations
  all_relevant_vars <- c(id_name, unlist(item_names))
  d1 <- d %>% select(all_relevant_vars)
  names(item_names) <- paste0("pair",seq_along(item_names))
  item_names
  response_names <- c(); rating_names <- c()
  for(i in seq_along(item_names)){
    response_names[i] <- item_names[[i]][1]#  first position in the pair
    rating_names[i] <- item_names[[i]][2]  # second position in the pair
  }
  response_names
  rating_names
  
  (d1 <- d %>% select( all_relevant_vars))
  # browser()
  d2 <-
    d1 %>%
    mutate_at( all_relevant_vars, as.integer) %>% # all incoming vars must be integers!!!
    tidyr::pivot_longer(-c(id_name) ) %>%
    mutate(
      item_type = case_when(
        # name %in% c("b4_response1","b4_response2") ~ "response"
        name %in% response_names ~ "response"
        # ,name %in% c("b4_rating1","b4_rating2") ~ "rating"
        ,name %in% rating_names ~ "rating"
      )
      ,item_order = case_when(
        # name %in% c("b4_response1","b4_rating1") ~ 1
        name %in% item_names[["pair1"]] ~ 1
        # ,name %in% c("b4_response2","b4_rating2") ~ 2
        ,name %in% item_names[["pair2"]] ~ 2
        # add more lines if more that two pairs
        # TODO: figure out a way to automate for list of N pairs
      )
    ) %>% print() %>% 
    select(-name) %>%
    pivot_wider(names_from = "item_type", values_from = "value")
  d1
  d2
  # browser()
  d3 <-
    d2 %>%
    mutate(
      response = case_when(response %in% valid_responses ~ response)
      ,rating  =  case_when(rating %in% valid_ratings ~ rating)
      # ,response_valid = case_when(response %in% valid_responses ~ TRUE,TRUE ~ FALSE)
    ) %>%
    # remove if provided save value for all responses
    group_by(!!rlang::sym(id_name)) %>%
    mutate(
      same_responses = n_distinct(response)==1L
    ) %>%
    ungroup() %>%
    filter(!same_responses) %>%  # the order of filer is important!
    filter(!is.na(response)) %>% # the order of filer is important!
    mutate(
      # rating_binary = case_when(
      #   rating %in% c(1,2) ~ TRUE
      #   ,rating %in% c(3,4,5) ~ FALSE
      #   # ,TRUE ~ NA_integer_
      # )
      # ,item_response = paste0(item_stem,separator,response_valid)
      item_response = paste0(item_stem,separator,response)
    )
  d3
  d3_response <-
    d3 %>%
    select(c(id_name, item_response, response)) %>%
    mutate(
      name = paste0(item_response, "_response")
      # name = paste0(item_response, "_a")
    ) %>%
    select(-item_response) %>%
    arrange(name) %>%
    pivot_wider(names_from = "name", values_from = "response",
                values_fn=is.integer, values_fill = FALSE)
  d3_response
  d3_rating <-
    d3 %>%
    select(c(id_name, item_response, rating)) %>%
    mutate(
      name = paste0(item_response, "_rating")
      # name = paste0(item_response, "_b")
    ) %>%
    select(-item_response) %>%
    # select(-item_names) %>%
    arrange(name) %>%
    pivot_wider(names_from = "name", values_from = "rating", values_fill = -99)
  d3_rating
  
  # browser()
  
  d4 <-
    d %>%
    left_join(
      d3_response
      , by = id_name
    ) %>%
    left_join(
      d3_rating
      , by = id_name
    )
  # d4 %>% glimpse()
  
  response_names <- setdiff( setdiff(names(d3_response),unlist(item_names)), id_name )
  rating_names <-setdiff( setdiff(names(d3_rating),unlist(item_names)), id_name )
  
  if(keep_responses == FALSE){
    d4 <- d4 %>% select(-response_names) #%>% glimpse()
  }
  if(keep_ratings == FALSE){
    d4 <- d4 %>% select(-rating_names)# %>% glimpse()
  }
  return(d4)
}

# how to use
ds0
ds0 %>%
  make_indicator_responses_ratings(
    item_names = list(c("b4_response1","b4_rating1"),c("b4_response2","b4_rating2"))
    ,valid_responses = c(1,2,3,97)
    ,valid_ratings   = c(1,2,3,4,5)
    ,id_name = "id"
    ,item_stem = "b4"
    ,separator = "_"
    ,keep_responses = TRUE
    ,keep_ratings = TRUE
  )



# ---- testing-2 ------------------------------------------------------------
# testing with an alternative data
# this data set has been constructed from a real-life scenario:
ds1 <- structure(list(survey_oid = 1:53
                      , b4a_resp1 = c(-7L, 97L, 1L, 
                                      1L, 1L, 99L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 88L, 3L, 3L, 
                                      2L, 2L, 1L, 2L, 2L, 1L, 1L, 3L, 1L, 3L, 3L, 1L, 1L, 1L, 2L, 3L, 
                                      2L, 1L, 1L, 1L, 1L, 3L, 1L, 2L, 2L, 1L, 1L, 2L, 3L, 1L, 1L, 2L, 
                                      3L, 2L, 1L)
                      , b4a_resp2 = c(-7L, -7L, -7L, -7L, 3L, -7L, -7L, 
                                      97L, -7L, -7L, 2L, -7L, 3L, 3L, 97L, -7L, -7L, -7L, -7L, -7L, 
                                      3L, -7L, 3L, -7L, 97L, 97L, -7L, 97L, -7L, 2L, 2L, 3L, 3L, -7L, 
                                      -7L, 3L, 3L, 3L, 97L, -7L, 3L, 97L, -7L, 3L, 3L, 97L, 97L, 2L, 
                                      2L, -7L, -7L, 97L, 97L)
                      , b4b1 = c(-7L, -7L, 99L, 4L, 2L, -7L, 
                                 2L, 2L, 2L, 3L, 1L, 1L, 2L, 3L, 2L, -7L, 3L, 99L, 99L, 2L, 1L, 
                                 3L, 2L, 5L, 1L, 1L, 88L, 3L, 1L, 2L, 3L, 4L, 1L, 5L, 1L, 1L, 
                                 2L, 3L, 3L, 4L, 88L, 2L, 88L, 2L, 99L, 4L, 99L, 1L, 2L, 4L, 88L, 
                                 1L, 4L)
                      , b4b2 = c(-7L, -7L, -7L, -7L, 99L, -7L, -7L, -7L, -7L, 
                                 -7L, 1L, -7L, 2L, 2L, -7L, -7L, -7L, -7L, -7L, -7L, 1L, -7L, 
                                 2L, -7L, -7L, -7L, -7L, -7L, -7L, 2L, 4L, 4L, 1L, -7L, -7L, 2L, 
                                 3L, 3L, -7L, -7L, 88L, -7L, -7L, 1L, 2L, -7L, -7L, 3L, 1L, -7L, 
                                 -7L, -7L, -7L))
                 , row.names = c(NA, -53L), class = c("tbl_df","tbl", "data.frame"))

ds1 %>% print(n=nrow(.))

ds1 %>%
  make_indicator_responses_ratings(
    item_names = list(c("b4a_resp1","b4b1"),c("b4a_resp2","b4b2"))
    ,valid_responses = c(1,2,3,97)
    ,valid_ratings   = c(1,2,3,4,5)
    ,id_name = "survey_oid"
    ,item_stem = "b4"
    ,separator = "_"
    ,keep_responses = TRUE
    ,keep_ratings = TRUE
  )

# ---- tweak-data --------------------------------------------------------------


# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
path <- "./analysis/.../report-isolated.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
