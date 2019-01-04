# rm(list = ls())
options(stringsAsFactors = FALSE)

script_name <- ""
project_directory <- getwd()
working_directory <- gsub("/PROJECT/", "/DATATMP/", project_directory)
output_directory <- paste0(working_directory, "/", script_name)

# setwd(working_directory)

params <- list(
  n_cores = parallel::detectCores(),
  gg_fontsize = 12
)


### Load packages
library(parallel)
library(tidyverse)
library(scales)
library(grid)
library(broom)

tidyverse_conflicts()


### Define n_cores
n_cores <- params$n_cores


### Define theme
devtools::source_url('https://github.com/mcanouil/DEV/raw/master/R/theme_black.R') 
theme_set(theme_black(base_size = params$gg_fontsize))


###
