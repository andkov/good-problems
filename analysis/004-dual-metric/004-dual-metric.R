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
# define the pool of valid values (the rest  will be converted to NA)

(item_names <- list(c("b4_response1","b4_rating1"),c("b4_response2","b4_rating2")))
names(item_names) <- c("pair1","pair2")
names(item_names) <- paste0("pair",seq_along(item_names))
item_names
response_names <- c(); rating_names <- c()
for(i in seq_along(item_names)){
  response_names[i] <- item_names[[i]][1]
  rating_names[i] <- item_names[[i]][2]
}
response_names
rating_names

ds2 <-
  ds1 %>%
  tidyr::pivot_longer(-id) %>% 
  mutate(
    item_type = case_when(
      # name %in% c("b4_response1","b4_response2") ~ "response"
      name %in% response_names ~ "response"
      # ,name %in% c("b4_rating1","b4_rating2") ~ "rating"
      ,name %in% rating_names ~ "rating"
    )
   ,item_order = case_when(
      # name %in% c("b4_response1","b4_rating1") ~ 1
      name %in% item_names[["pair1"]]  ~ 1
      # ,name %in% c("b4_response2","b4_rating2") ~ 2
      ,name %in% item_names[["pair2"]] ~ 2
      # add more lines if more that two pairs
      # TODO: figure out a way to automate for list of N pairs
    )
  ) %>% 
  select(-name) %>% 
  pivot_wider(names_from = "item_type", values_from = "value")
ds2
# compare it to the original
ds1


# ----- solution-1-functional --------------------------------------------------
rm(ds1,ds2)
transform_response_rating <- function(
  d
  ,item_names # list of the form: list(c("response1","rating1"),c("response2","rating2"),...)
  ,id_name
){
  d <- ds0
  (item_names <- list(c("b4_response1","b4_rating1"),c("b4_response2","b4_rating2")))
  id_name = "id"
  
  
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
  
  d2 <-
    d1 %>%
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
    ) %>% 
    select(-name) %>% 
    pivot_wider(names_from = "item_type", values_from = "value")
  d1
  d2
  
  return(d2)
  
}

# how to use
ds0
ds0 %>% 
  transform_response_rating(
  (item_names <- list(c("b4_response1","b4_rating1"),c("b4_response2","b4_rating2")))
  ,id_name = "id"
)

# ---- inspect-data ------------------------------------------------------------


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
