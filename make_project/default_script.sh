#!/bin/sh

R --slave --silent <<RSCRIPT
project_directory <- '/disks/PROJECT/PRJCT'
report_directory <- '/disks/PROJECT/Report/PRJCT'

dir.create(path = report_directory, recursive = TRUE, showWarnings = FALSE, mode = '0777')

rmarkdown::render(
  input = paste0(project_directory, '/Scripts/00-main.Rmd'), 
  output_file = '00-main.html', 
  output_format = 'html_document', 
  output_dir = report_directory,
  encoding = 'UTF-8'
)
RSCRIPT

git -C /disks/PROJECT/Report/PRJCT/ add 00-main.html
git -C /disks/PROJECT/Report/PRJCT/ commit -a 00-main.html -m 'Update report PRJCT'

