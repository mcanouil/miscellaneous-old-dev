# rm(list = ls())
options(stringsAsFactors = FALSE)

script_name <- ""
project_directory <- gsub("Scripts", "", getwd())
working_directory <- gsub("/PROJECT/", "/DATATMP/", project_directory)
output_directory <- paste0(working_directory, "/", script_name)

dir.create(paste0(output_directory, "/KnitrFiles/"), recursive = TRUE, showWarnings = FALSE, mode = '0777')

setwd(working_directory)

params <- list(
  n_cores = parallel::detectCores(),
  gg_fontsize = 12
)


### Load packages
library(tidyverse)
library(broom)
library(scales)
library(parallel)
library(grid)

tidyverse_conflicts()


### Define n_cores
n_cores <- params$n_cores


### Define theme
theme_dark <- params$theme_dark
if (theme_dark) {
  source("/disks/PROJECT/Rscripts/MC/theme_black.R")
  theme_set(theme_black(base_size = params$gg_fontsize))
} else {
  theme_set(theme_light(base_size = params$gg_fontsize))
} 


###
