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
valid_responses <- c(1,2,3,97)
item_names <- c("b4_response1","b4_response2","b4_rating1", "b4_rating2")
ds2 <-
  ds1 %>%
  tidyr::pivot_longer(cols = item_names) %>%
  mutate(
    value_valid = value %in% valid_responses # TRUE if target, FALSE if valid, but not target
    ,item_response = paste0("b4","_",value)
  ) %>%
  group_by(id, item_response) %>%
  ungroup()
valid_responses
ds2

# if any of the items contain a valid response -> case is valid
# d_valid <- 
#   ds2 %>% 
#   # mutate(name = "b4") %>% 
#   select(id, value_valid) %>% 
#   group_by(id) %>% 
#   summarize(
#     value_valid = sum(value_valid, na.rm = T)>0L
#   ) %>% 
#   mutate(
#     name = "b4_valid"
#   ) %>% 
#   pivot_wider(names_from = "name", values_from = "value_valid")
# d_valid

# make wide ds with indicators
ds3 <-
  ds2 %>%
  mutate(
    value = ifelse(value %in% valid_responses, value, NA)
  ) %>% 
  filter(!is.na(value)) %>%
  select(id, item_response, value_valid) %>%
  pivot_wider(names_from = item_response, values_from = value_valid, values_fn=max) %>%
  mutate_at(
    .vars = setdiff(names(.),"id")
    ,.funs = as.logical
  )
ds3
# join to the ds with raw responses

ds4 <-
  dplyr::left_join(
    ds1
    ,d_valid
    ,by = "id"
  ) %>% 
  dplyr::left_join(
    ds3
    ,by = "id"
  )
ds4

# ----- solution-1-functional --------------------------------------------------
rm(ds1,ds2,ds3,ds4)
make_indicator_responses_ratings <- function(
  d
  ,item_names
  ,valid_responses
  ,id_name
  ,item_stem
  ,separator = "_"
){
  # d <- ds0
  # (item_names <- list(c("b4_response1","b4_rating1"),c("b4_response2","b4_rating2")))
  # valid_responses = c(1,2,3,97)
  # valid_ratings   = c(1,2,3,4,5)
  # id_name = "id"
  # item_stem = "b4"
  # separator = "_"
  
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
      # name = paste0(item_response, "_response")
      name = paste0(item_response, "_a")
    ) %>%
    select(-item_response) %>%
    pivot_wider(names_from = "name", values_from = "response",
                values_fn=is.integer, values_fill = FALSE)
 d3_response #%>% View()
 d3_rating <- 
   d3 %>% 
   select(c(id_name, item_response, rating)) %>% 
   mutate(
     # name = paste0(item_response, "_rating")
     name = paste0(item_response, "_b")
   ) %>% 
    select(-item_response) %>% 
   pivot_wider(names_from = "name", values_from = "rating", values_fill = -99)
 d3_rating 
 
 d4 <- 
   d1 %>% 
   left_join(
     d3_response
     ,by = id_name
   ) %>% 
   left_join(
     d3_rating
     ,by = id_name
   )
  d4
  
  # make wide ds with indicators
  d3 <-
    d2 %>%
    mutate(
      value = ifelse(value %in% valid_responses, value, NA)
    ) %>% 
    filter(!is.na(value)) %>%
    select(!!!rlang::syms(c(id_name, "item_response", "value_valid"))) %>% 
    pivot_wider(names_from = item_response, values_from = value_valid, values_fn=max,values_fill = FALSE) %>%
    mutate_at(
      .vars = setdiff(names(.),id_name)
      ,.funs = as.logical
    )
  d3
  
  
  
  # # join to the ds with raw responses
  
  d4 <-
    dplyr::left_join(
      d
      ,d3
      ,by = id_name
    ) 
  d4
  
  return(d4)
  
}

# how to use
ds0
ds0 %>% 
  make_indicator_responses_ratings(
    item_names = c("b4_response1", "b4_response2")
    ,valid_responses = c(1,2,3,97)
    ,id_name = "id"
    ,item_stem = "b4"
    ,separator = "_"
  ) %>% 
  make_indicator_responses_ratings(
    item_names = c("b4_rating1", "b4_rating2")
    ,valid_responses = c(1,2,3,4,5)
    ,id_name = "id"
    ,item_stem = "b4r"
    ,separator = "_"
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
