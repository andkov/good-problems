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
# the following data set stores a sample of responses
ds0 <-
  tibble::tribble(
    ~id, ~b4, ~b4_response1, ~b4_response2,
    1, 1 ,  1,  2,
    2, 2 , NA, NA,
    3, 1 ,  2, 3,
    4, 1 ,  97, -7,
    5, 1 ,  3, 97,
    6, 1 ,  NA, 2,
    7, -7, -7, -7,
    8, 1 ,  97, 97,
  ) %>%
  mutate_all(as.integer)
ds0

# ---- solution-1-basic --------------------------------------------------------
# of people who had lunch and provided at least one valid response
# what percent of people had apple/banana/kiwi for lunch?
# Step 1: Isolate variables to be evaluated
(ds1 <- ds0 %>% select(id, b4_response1, b4_response2))
# define the pool of valid values (the rest  will be converted to NA)
valid_responses <- c(1,2,3,97)
item_names <- c("b4_response1","b4_response2")
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
d_valid <- 
  ds2 %>% 
  # mutate(name = "b4") %>% 
  select(id, value_valid) %>% 
  group_by(id) %>% 
  summarize(
    value_valid = sum(value_valid, na.rm = T)>0L
  ) %>% 
  mutate(
    name = "b4_valid"
  ) %>% 
  pivot_wider(names_from = "name", values_from = "value_valid")
d_valid

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
rm(ds1,ds2,ds3,ds4, ds_valid)
augment_with_indicators <- function(
  d
  ,item_names
  ,valid_responses
  ,id_name
  ,item_stem
  ,separator = "_"
){
  d <- ds0
  item_names = c("b4_response1", "b4_response2")
  valid_responses = c(1,2,3,97)
  id_name = "id"
  item_stem = "b4"
  separator = "_"
  
  (d1 <- d %>% select( c(id_name, item_names) ) )
  # define the pool of valid values (the rest  will be converted to NA)
  # valids <- c(1,2,3,97)
  # item_names <- c("b4_response1","b4_response2")
  d2 <-
    d1 %>%
    tidyr::pivot_longer(cols = item_names) %>%
    mutate(
      value_valid = value %in% valid_responses # TRUE if target, FALSE if valid, but not target
      ,item_response = paste0(item_stem,separator,value)
    ) 
  d2
  
  
  # if any of the items contain a valid response -> case is valid
  d_valid <-
    d2 %>%
    # mutate(name = "b4") %>%
    select(!!!rlang::syms(c(id_name, "value_valid"))) %>%
    group_by(!!rlang::sym(id_name)) %>%
    summarize(
      value_valid = sum(value_valid, na.rm = T)>0L
    ) %>%
    mutate(
      name = paste0(item_stem,separator,"valid")
    ) %>%
    pivot_wider(names_from = "name", values_from = "value_valid")
  d_valid
  
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
  augment_with_indicators(
    item_names = c("b4_response1", "b4_response2")
    ,valid_responses = c(1,2,3,97)
    ,id_name = "id"
    ,item_stem = "b4"
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
