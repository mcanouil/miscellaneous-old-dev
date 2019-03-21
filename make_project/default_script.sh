#!/bin/sh

R --slave --silent <<RSCRIPT
user <- "Mickaël Canouil"
email <- "mickael.canouil@cnrs.fr"
# fexsend <- "/fexsend"

project_directory <- "~/PRJCT"
working_directory <- gsub("/PROJECT/", "/DATATMP/", project_directory)

script_name <- "00-main"

rmarkdown::render(
  input = paste0(project_directory, "/Scripts/", script_name, ".Rmd"), 
  output_file = paste0(script_name, ".html"), 
  # output_format = "html_document2", 
  output_dir = paste(working_directory, script_name, sep = "/"),
  encoding = 'UTF-8'
)

system(
  command = paste(
    "tar zcvf", 
    paste0(paste(working_directory, script_name, sep = "/"), "/", Sys.Date(), "_", script_name, ".tar.gz"), 
    "-C", paste(working_directory, script_name, sep = "/"), 
    paste0(script_name, ".html")
  ),
  intern = TRUE
)

# fex_out <- system(
#   command = paste(
#     fexsend,
#     paste0(output_directory, "/",  Sys.Date(), "_", script_name, ".tar.gz"),
#     email
#   ),
#   intern = TRUE
# )

git <- paste0('git -c "user.name=', user, '" -c "user.email=', email, '" -C ', project_directory)
system(paste(git, "add", paste0("Scripts/", script_name, ".Rmd")))
system(paste(
  git, "commit", paste0("Scripts/analyses/", script_name, ".Rmd"),
  paste0('-m "Analysis done ', as.Date(Sys.Date(), "%Y%m%d"), '"')
))
system(paste(
  git, "tag -a", script_name,
  paste0("-m\"", "Analysis done ", as.Date(Sys.Date(), "%Y%m%d"), "\"")
))
RSCRIPT
