options(stringsAsFactors = FALSE)

### Load packages
library(here)
library(tidyverse)


### Working directory

script_name <- "default_script"
dir.create(path = here("Data", script_name), recursive = TRUE, showWarnings = FALSE, mode = "0777")


### Params
fexsend <- "/disks/DATA/Softwares/FEX/fexsend"
user <- "Mickaël Canouil"
email <- "mickael.canouil@cnrs.fr"


### Define theme
theme_set(theme_light(base_size = 12))


### Analyses
rmarkdown::render(
  input = here("Scripts", paste0(script_name, ".Rmd")), 
  output_file = paste0(script_name, ".html"), 
  output_dir = here("Data", script_name),
  encoding = "UTF-8", 
  params = list(
    script_name = script_name,
    project_directory = here(),
    working_directory = here("Data")
  )
)


### Output
archive_name <- file.path(
  here("Data", script_name),
  paste0(format(Sys.Date(), format = "%Y%m%d"), "_", basename(here()), ".tar.gz")
)
tar(tarfile = archive_name, files = paste0(script_name, ".html"), compression = "gzip")
fex_out <- system(paste(fexsend, archive_name, email), intern = TRUE)
