options(stringsAsFactors = FALSE)

script_name <- "default_script"
project_directory <- "~/PRJCT"
working_directory <- paste0(project_directory, "/Data")
output_directory <- paste(working_directory, script_name, sep = "/")

dir.create(path = output_directory, recursive = TRUE, showWarnings = FALSE, mode = "0777")


### Params
fexsend <- "/disks/DATA/Softwares/FEX/fexsend"
user <- "Mickaël Canouil"
email <- "mickael.canouil@cnrs.fr"


### Load packages
library(tidyverse)


### Define theme
theme_set(theme_light(base_size = 12))


### Analyses
rmarkdown::render(
  input = paste0(project_directory, "/Scripts/", script_name, ".Rmd"), 
  output_file = paste0(script_name, ".html"), 
  output_dir = paste(working_directory, script_name, sep = "/"),
  encoding = "UTF-8", 
  params = list(
    script_name = script_name,
    project_directory = project_directory,
    working_directory = working_directory
  )
)


### Output
archive_name <- file.path(
  output_directory,
  paste0(format(Sys.Date(), format = "%Y%m%d"), "_", basename(project_directory), ".tar.gz")
)
tar(tarfile = archive_name, files = paste0(c(paste0(script_name, ".html")), ".html"), compression = "gzip")
fex_out <- system(paste(fexsend, archive_name, email), intern = TRUE)
