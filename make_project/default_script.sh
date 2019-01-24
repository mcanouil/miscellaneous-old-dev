#!/bin/sh

R --slave --silent <<RSCRIPT
project_directory <- '~/PRJCT'
report_directory <- paste0(project_directory, '/Report')

dir.create(path = report_directory, recursive = TRUE, showWarnings = FALSE, mode = '0777')

rmarkdown::render(
  input = paste0(project_directory, '/Scripts/00-main.Rmd'), 
  output_file = '00-main.html', 
  output_format = 'html_document', 
  output_dir = report_directory,
  encoding = 'UTF-8'
)
RSCRIPT

git -C ~/PRJCT/Report add 00-main.html
git -C ~/PRJCT/Report commit -a 00-main.html -m 'Update report PRJCT'
