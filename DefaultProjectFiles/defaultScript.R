# rm(list = ls())
options(stringsAsFactors = FALSE)

working_directory <- "/disks/DATATMP/PRJCT/"
setwd(working_directory)
# ggsave(file = "Pictures/.png", plot = p, width = 6.3, height = 4.7, units = "in", dpi = 300)

params <- list(
  n_cores = parallel::detectCores(),
  theme_dark = TRUE,
  gg_fontsize = 12
)

### Load packages
library(tidyverse)
library(broom)
library(scales)
library(parallel)
library(grid)

tidyverse_conflicts()

source("/disks/PROJECT/Rscripts/MC/format_pval.R")
source("/disks/PROJECT/Rscripts/MC/ggheatmap.R")
source("/disks/PROJECT/Rscripts/MC/ggmanhattan.R")
source("/disks/PROJECT/Rscripts/MC/ggqqplot.R")

source("/disks/PROJECT/Rscripts/MC/theme_black.R")

### Define n_cores
n_cores <- params$n_cores


### Define theme
theme_dark <- params$theme_dark
if (theme_dark) {
  theme_set(theme_black(base_size = params$gg_fontsize))
} else {
  theme_set(theme_light(base_size = params$gg_fontsize))
} 


###
