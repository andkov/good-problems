rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default unless overwritten

# ---- load-sources ------------------------------------------------------------

source("./scripts/common-functions.R")
source("./analysis/006-define-outliers/define-outliers-function.R")

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
# Goal: create a simulated data set storing reported hours of work per week
# relevant thresholds:
# tier 3 -  168 = total hours in a week
# tier 2 -  112 =  7 * 16
# tier 1 -  84  =  7 * 12
# 

# ---- tweak-data --------------------------------------------------------------


# ---- table-1 -----------------------------------------------------------------

# ---- graph-1 -----------------------------------------------------------------
# Goal: define a function that defines the thresholds for outlier cutoffs
# plots the graph and create a factor storing 

# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------

