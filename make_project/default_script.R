options(stringsAsFactors = FALSE)

script_name <- ''
project_directory <- '~/PRJCT'
working_directory <- paste0(project_directory, '/Data')
output_directory <- paste0(working_directory, script_name, sep = '/')


params <- list(
  n_cores = parallel::detectCores(),
  theme_dark = TRUE,
  gg_fontsize = 12
)


### Load packages
library(parallel)
library(grid)
library(tidyverse)
library(scales)

devtools::source_url('https://github.com/mcanouil/DEV/raw/master/R/theme_black.R')


### Define n_cores
n_cores <- params$n_cores


### Define theme
if (params$theme_dark) {
  theme_set(theme_black(base_size = params$gg_fontsize))
} else {
  theme_set(theme_light(base_size = params$gg_fontsize))
}


###
