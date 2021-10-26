rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default unless overwritten

# ---- load-sources ------------------------------------------------------------

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
prints_folder <- paste0("./analysis/002-placing-plots/prints/")
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}

ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey95", color = NA)
    )
)
quick_save <- function(g,name,...){
  ggplot2::ggsave(
    # filename = paste0(name,".jpg"),
    filename = paste0(name,".png"),
    plot     = g,
    # device   = "jpg",
    device   = "png",
    path     = prints_folder,
    # width    = width,
    # height   = height,
    # units = "cm",
    # dpi      = 'retina',
    limitsize = FALSE,
    ...
  )
}

# ---- load-data ---------------------------------------------------------------

# ---- inspect-data ------------------------------------------------------------


# ---- tweak-data --------------------------------------------------------------


# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------
g <-
  mtcars %>%
  ggplot(aes(x=mpg,y=disp))+
  geom_point()
g

# ---- graph-2 -----------------------------------------------------------------
file_name <- "plot-1"
# g %>% quick_save("plot-1",width = 12, height = 5)
g %>% quick_save(file_name,width = 12, height =5, dpi =1000)
file_path <- paste0(prints_folder,file_name,".png")
# file_path %>% jpeg::readJPEG() %>% grid::grid.raster()
file_path %>% png::readPNG() %>% grid::grid.raster()


# ---- graph-3 -----------------------------------------------------------------
# TODO
# Design the function that would
# 1) save the plot to disk with specified physical dimension
# 2) place the plot from disk into the chunk preserving the dimensions
# g %>% print_and_place() # 1) saves to disk 2) prints to chunk

# ---- save-to-disk ------------------------------------------------------------
path <- "./analysis/study-placing-plots/002-placing-plots.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
