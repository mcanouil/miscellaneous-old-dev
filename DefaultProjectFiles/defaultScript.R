rm(list = ls())
options(stringsAsFactors = FALSE)

setwd("/disks/DATA/PRJCT/")
# ggsave(file = "Pictures/.png", plot = p, width = 7.5, height = 6, units = "in", dpi = 300)
# ggsave(file = "Pictures/.png", plot = p, width = 12, height = 6, units = "in", dpi = 300)


### Define nCores
    nCores <- parallel::detectCores()


### Load packages
    library(parallel)
    library(grid)
    library(scales)
    library(tidyverse)
    library(broom)
    library(viridis)
    library(readxl)
    library(writexl)
    library(cowplot)
    library(ggrepel)
    library(flashpcaR)
    
    null <- sapply(list.files(path = "/disks/PROJECT/Mickael/DEV/Rfunctions/", full.names = TRUE), source)
    
    theme_dark <- TRUE
    if (theme_dark) {
        ## Theme Black ON ##
        theme_set(theme_black(base_size = 14))
        plot_grid <- hijack(plot_grid, theme_dark = theme_dark)
        scale_colour_viridis <- hijack(scale_colour_viridis, option = "viridis", begin = 2/5, end = 1, direction = -1)
        scale_fill_viridis <- hijack(scale_fill_viridis, option = "viridis", begin = 2/5, end = 1, direction = -1)
    } else {
        theme_set(theme_light(base_size = 14))
        scale_colour_viridis <- hijack(scale_colour_viridis, option = "viridis", begin = 0, end = 4/5)
        scale_fill_viridis <- hijack(scale_fill_viridis, option = "viridis", begin = 0, end = 4/5)
    }    


###
