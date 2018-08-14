#!/bin/sh


R --slave --silent <<RSCRIPT
dir.create(path = '/disks/PROJECT/Report/PRJCT', recursive = TRUE)
rmarkdown::render(
  input = '/disks/PROJECT/PRJCT/Scripts/00-PRJCT.Rmd', 
  output_file = '00-PRJCT.html', 
  output_format = 'html_document', 
  output_dir = '/disks/PROJECT/Report/PRJCT',
  encoding = 'UTF-8'
)
RSCRIPT

git -C /disks/PROJECT/Report/ commit \
  -a PRJCT/00-PRJCT.html \
  -m 'Update report PRJCT'

