# functions to assist with exploratory data analysis

# ---- outliers ----------------------------------------------------------------
# ds <- 
#   d1 %>% 
#   left_join(WORP_SURVEY %>% select(survey_oid, survey_year)) %>%  
#   select(survey_oid, pre_prog_hours, pre_prog_wage, pre_prog_monthly_earnings, 
#          survey_year)
# 
# ds

library("rlang")

detect_outliers <- function(
  d # 
  ,yvar = NULL #"pre_prog_hours" # continuous variable to be examined
  ,xvar = NULL #"survey_year" # categorical variable used on x-axis
  ,outvar # name of the new variable containing outlier flags
  ,tier3 = Inf # values that are clearly impossible (e.g. 200 hours per week)
  ,tier2 = Inf # liberal rule (give benefit of the doubt to an odd observation )
  ,tier1 = Inf # conservative rule (only include defensible values), tier0 is not an outlier
  ,width = 12
  ,height = 10
){
  # browser()
  outvar <- paste0(yvar,"_out")
  
  ls_out  <- list()
  d1 <-
    d %>%
    rename(
      ymeasure = yvar
    ) %>% 
    mutate(
      # {{"pre_prog_hours_out"}} := case_when(
      !!rlang::sym(outvar) := case_when(
        ymeasure < tier1                           ~ "tier 0"
        ,(ymeasure >= tier1) & (ymeasure <= tier2) ~ "tier 1"
        ,(ymeasure > tier2) & (ymeasure <= tier3)  ~ "tier 2"
        ,ymeasure > tier3                          ~ "tier 3"
      )
    ) %>% 
    rename(
      !!rlang::sym(yvar) := ymeasure
    ) %>% 
    mutate(
      !!rlang::sym(xvar) := as_factor(!!rlang::sym(xvar))
    ) 
  
  # browser()
  d2 <- 
    d1 %>% 
    group_by(!!!rlang::syms(c(xvar,outvar))) %>%
    # group_by(!!!rlang::syms(list(c(xvar,outvar)))) %>% 
    summarize(
      case_count = n()
    ) %>% 
    ungroup() %>% 
    mutate(
      value_exists = !is.na(!!rlang::sym(outvar))
    ) %>% 
    group_by(!!!rlang::syms(c(xvar, "value_exists"))) %>%
    # group_by(!!!rlang::syms(list(c(xvar, value_exists)))) %>% 
    mutate(
      case_pct = case_count/sum(case_count, na.rm = T)
      ,case_pct = case_when(
        value_exists ~ case_pct, TRUE ~ NA_real_
      )
    ) %>% 
    ungroup() %>% 
    mutate(
      case_pct_pretty = scales::percent(case_pct, accuracy = 1)
      ,case_count_pretty = scales::comma(case_count)
      ,text_display = paste0(
        # case_pretty = case_count_pretty,"\n",purrr::discard(case_pct_pretty, is.na)
        # case_pretty = case_count_pretty,"\n",na.omit(case_pct_pretty)
        # case_pretty = case_count_pretty,"\n",case_pct_pretty
        # ,case_pretty = str_remove(case_pretty,"NA$")
        # case_count_pretty,"\n",na.omit(case_pct_pretty)
        case_count_pretty,"\n",case_pct_pretty
      )
      ,text_display = str_remove(text_display, "\nNA$")
    )
  d2
  #1b9e77 - green
  #d95f02 - orange
  #7570b3 - purprle
  tier_colors <- c(
    "tier 0" =  "#1b9e77" # green
    ,"tier 1" = "#d95f02" # orange
    ,"tier 2" = "#7570b3" # purple
    ,"tier 3" = "red"
  )
  
  tier_levels <- c(
    "tier 0" =  "main\nbody"
    ,"tier 1" = paste0("cut at ",tier1)
    ,"tier 2" = paste0("cut at ",tier2)
    ,"tier 3" = paste0("cant be over ",tier3)
  )
  # browser()
  outvar2 <- paste0(outvar,"2")
  g00 <- 
    d2 %>% 
    mutate(
      !!rlang::sym(outvar2) := factor(!!rlang::sym(outvar),
                                      levels = names(tier_levels),
                                      labels = tier_levels
      )
      ,!!rlang::sym(outvar) := factor(!!rlang::sym(outvar)) %>%   fct_rev()
    ) %>%
    {
      ggplot(., aes(x=survey_year, y = !!rlang::sym(outvar2)))+
        geom_text(aes(label = text_display, color = !!rlang::sym(outvar)))+
        scale_color_manual(values = tier_colors)+
        guides(color = guide_legend(reverse = TRUE))
    }
  
  plot_outliers <- function(d,filter_exp=NULL){
    d %>%
      filter(!!enquo(filter_exp)) %>% 
      {
        ggplot(., aes(x = !!rlang::sym(xvar), y = !!rlang::sym(yvar))) +
          geom_boxplot(
            outlier.colour = tier_colors["tier 0"]
            ,outlier.fill=NA
            ,outlier.shape=21
            # ,outlier.shape=NA
            # ,position = position_jitter(width = .1, height =0)
          )+
          geom_point(
            aes(color = !!rlang::sym(outvar))
            ,  shape = 21, size = 2
            , data = . %>% filter(!!rlang::sym(outvar) != "tier 0")
            , position = position_jitter(width = .1)
          )+
          scale_color_manual(values = tier_colors)
        
      }  
  }
  
  title_gg <- ggplot() + 
    labs(title = paste0("Suspected outliers in (",yvar,") by levels of (",xvar, ")")
         # , subtitle = "By census tract, 2016"
    )+theme_void()
  
  title_g3 <- paste0("All observation shown")
  title_g1 <- paste0("Obserervations removed above (", tier2,")")
  title_g0 <- paste0("Obserervations removed above (", tier1,")")
  title_g00 <- paste0("Counts of ligitimate values and suspected outliers")
  
  shared_labs <- labs(y = NULL, x = NULL)
  # browser()
  g3 <- d1 %>% plot_outliers(!!rlang::sym(outvar) %in% c("tier 0", "tier 1", "tier 2", "tier 3")) + labs(title = title_g3) + shared_labs
  # g2 <- d1 %>% plot_outliers(!!rlang::sym(outvar) %in% c("tier 0", "tier 1", "tier2")) + labs(title = title_g1)
  g1 <- d1 %>% plot_outliers(!!rlang::sym(outvar) %in% c("tier 0", "tier 1")) + labs(title = title_g1)+ shared_labs
  g0 <- d1 %>% plot_outliers(!!rlang::sym(outvar) %in% c("tier 0")) + labs(title = title_g0)+shared_labs
  g00 <- g00 + labs(title = title_g00)+shared_labs
  
  g3 <- g3 +theme(legend.position = "bottom")
  library("cowplot")
  g <- 
    cowplot::plot_grid(
      title_gg, NULL
      ,g3   + theme(legend.position = "none")
      ,g1  + theme(legend.position = "none")
      ,cowplot::get_legend(g3)
      ,NULL
      ,g00 + theme(legend.position = "none")
      ,g0  + theme(legend.position = "none")
      # ,labels = c("Tier 2+3", "Tier 1","", "Counts", "Main Body")
      ,ncol = 2
      , nrow = 4
      ,rel_heights = c(1,7,1,7)
    )+
    theme(plot.background =  element_rect(fill = "white", color = "white"))
  # g
  if(!exists("prints_folder")) stop("Prints folder has not been defined")
  if(!fs::dir_exists(prints_folder)) stop("Prints folder does not exist")
  g %>% quick_save(outvar, w=12, h=8)
  
  ls_out[["data"]] <- d1
  ls_out[["graph"]] <- g
  return(d1)
}
# How to use

# lstemp <-
#   ds %>% 
#   detect_outliers(
#     yvar  = "pre_prog_hours"
#     ,xvar  = "survey_year"
#     ,tier3 = 150
#     ,tier2 = 112
#     ,tier1 = 84
#   )  
# lstemp$data
# lstemp$graph

detect_overall_outliers <- function(outlier_vector){
  num_flags <- length(outlier_vector)
  overall_level <- "tier 0"
  for(i in 1:num_flags){
    current_flag <- outlier_vector[i]
    if(is.na(current_flag))current_flag <- "tier 0"
    if(current_flag != "tier 0")overall_level <- "tier 1"
    if(current_flag %in% c("tier 2", "tier 3"))overall_level <- "tier 2"
    if(current_flag == "teir 3")overall_level <- "tier 3"
  }
  return(overall_level)
}
# How to use

# group_by(survey_oid)
# pass list of outlier flags as vector
# ungroup()