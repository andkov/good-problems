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

prints_folder <- paste0("./analysis/003-either-or/prints/")
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}

# ---- load-data ---------------------------------------------------------------
# Scenario: 6 individuals asked about fruit consumption the day before
# Research Question 1: Wht

# Q1 - Did you have lunch? (1 - yes, 2 - no, -7 - question no asked) 
# Q1_resp1(2) - If you had lunch, what fruit did you eat? (select up to two)
# Q2 - Did you have dinner?
# Q2_resp1(2) - If you had dinner, what fruit did you eat? (select up to two)

responses <- c("1" = "yes", "2" = "no", "-7" = "Q not asked")
fruit <- c("1" = "apple", "2" = "banana", "3" = "kiwi", "97" = "other")
ds0 <-
  tibble::tribble(
  ~id, ~lunch, ~lunch1, ~lunch2,
  1, 1 ,  1,  2,
  2, 2 , NA, NA,
  3, 1 ,  2, 1, 
  4, 1 ,  97, NA,
  5, 1 ,  3, NA, 
  6, -7, NA, NA,
  7, 2 ,  NA, NA,
) %>% 
  mutate_all(as.integer)

# of people who had lunch and provided at least one valid response
# what percent of people had apple/banana/kiwi for lunch? 

ds1 <-
  ds0 %>% 
  mutate(
    # apple = case_when( (lunch1==1 | lunch2 == 1) ~ TRUE)
    # apple = case_when( any( c(lunch1, lunch2) %in% c(1,2,3,97)) ~ "valid fruit") 
    apple = case_when( any( c(lunch1, lunch2)==-7 ) ~ "valid fruit") 
  )
ds1

a <- "A"
b <- "B"
v <- c(a,b)
valid_set <- c("D","C")
any(v %in% valid_set)

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
