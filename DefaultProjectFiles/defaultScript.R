# rm(list = ls())
options(stringsAsFactors = FALSE)

setwd("/disks/DATATMP/PRJCT/")
# ggsave(file = "Pictures/.png", plot = p, width = 6.3, height = 4.7, units = "in", dpi = 300)


### Load packages
library(tidyverse)
library(broom)
library(scales)
library(parallel)
library(grid)

tidyverse_conflicts()

source("/disks/PROJECT/Mickael/DEV/Rfunctions/format_pval.R")
source("/disks/PROJECT/Mickael/DEV/Rfunctions/ggheatmap.R")
source("/disks/PROJECT/Mickael/DEV/Rfunctions/ggmanhattan.R")
source("/disks/PROJECT/Mickael/DEV/Rfunctions/ggqqplot.R")
source("/disks/PROJECT/Mickael/DEV/Rfunctions/pretty_kable.R")
source("/disks/PROJECT/Mickael/DEV/Rfunctions/theme_black.R")

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
