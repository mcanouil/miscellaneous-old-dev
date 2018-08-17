#!/bin/sh


R --slave --silent <<RSCRIPT
dir.create(path = '/disks/PROJECT/Report/PRJCT', recursive = TRUE)
rmarkdown::render(
  input = '/disks/PROJECT/PRJCT/Scripts/00-main.Rmd', 
  output_file = '00-main.html', 
  output_format = 'html_document', 
  output_dir = '/disks/PROJECT/Report/PRJCT',
  encoding = 'UTF-8'
)
RSCRIPT

git -C /disks/PROJECT/Report/ add PRJCT/00-main.html
git -C /disks/PROJECT/Report/ commit -a PRJCT/00-main.html -m 'Update report PRJCT'

