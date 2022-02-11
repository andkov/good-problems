make_indicator_responses_ratings <- function(
  d
  ,item_names
  ,valid_responses
  ,valid_ratings
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
  
  #
  
  return(d4)
  
}