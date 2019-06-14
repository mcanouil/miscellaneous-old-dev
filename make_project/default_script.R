options(stringsAsFactors = FALSE)

script_name <- "default_script"
project_directory <- "~/PRJCT"
working_directory <- paste0(project_directory, "/Data")
output_directory <- paste(working_directory, script_name, sep = "/")

dir.create(path = output_directory, recursive = TRUE, showWarnings = FALSE, mode = "0777")

# params <- list(
#   n_cores = parallel::detectCores(),
#   gg_fontsize = 12
# )


### Load packages
library(parallel)
library(tidyverse)
library(scales)
library(grid)

devtools::source_url('https://github.com/mcanouil/DEV/raw/master/R/theme_black.R')


### Define theme
theme_set(theme_black(base_size = 12))


### GIT parameters
current_date <- format(Sys.Date(), format = "%Y%m%d")
user <- "Mickaël Canouil"
email <- "mickael.canouil@cnrs.fr"
git <- paste0('git -c "user.name=', user, '" -c "user.email=', email, '" -C ', project_directory)
commit <- paste0('-m "ANALYSIS done ', current_date, '"')
fexsend <- "/disks/DATA/Softwares/FEX/fexsend"


### Analyses
rmarkdown::render(
  input = paste0(project_directory, "/Scripts/", script_name, ".Rmd"), 
  output_file = paste0(file_name, ".html"), 
  output_dir = paste(working_directory, script_name, sep = "/"),
  encoding = "UTF-8", 
  params = list(
    script_name = script_name,
    project_directory = project_directory,
    working_directory = working_directory
  )
)


### Output
file_name <- script_name
archive_name <- paste(output_directory, paste0(current_date, "_", file_name, ".tar.gz"), sep = "/")
system(paste(
  "tar zcvf", archive_name, "-C", output_directory, paste0(file_name, ".html")
), intern = TRUE)
fex_out <- system(paste(fexsend, archive_name, email), intern = TRUE)
system(paste(git, "tag", paste(current_date, file_name, sep = "-"), commit), intern = TRUE)
