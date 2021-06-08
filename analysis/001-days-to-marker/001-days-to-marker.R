library(magrittr)  # pipes
library(dplyr)     # data wrangling

# Subject:
d <- tribble(
  ~id, ~date, ~marker,
   1, "2011-01-01", FALSE
  ,1, "2011-01-03", FALSE 
  ,1, "2011-01-05", TRUE 
  ,1, "2011-01-07", FALSE 
  ,1, "2011-01-09", FALSE
  
  ,2, "2012-02-02", TRUE 
  ,2, "2012-02-04", FALSE
  
  ,3, "2013-03-01", FALSE
  ,3, "2013-03-05", FALSE
  ,3, "2013-03-10", TRUE 
  
  ,4, "2014-04-20", FALSE
  ,4, "2014-04-30", FALSE
  
  ,5, "2015-05-15", NA
) %>% 
  mutate(
  id = as.integer(id)
  ,date = as.Date(date)
) %>% 
  print(n=nrow(.))

# Challenge:
# Create a variable "days_to_marker" that show the number of days from the
# current date to the date of the marker
# Strategy suggestion: consider developing solutions step by step:
# Step 1 for ids 1:3, Step 2 for ids 1:4, and Step 3 for ids 1:5


# Solution 1
d1 <- d %>% 
  filter(id %in% 1:3) %>% 
  group_by(id) %>% 
  mutate(
    date_marker = min(case_when(marker==T ~ date), na.rm = T)
  )
d1
