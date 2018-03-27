#!/bin/sh

cd /disks/PROJECT/intranet-result/
mkdir PRJCT
# git pull origin master

R --slave --silent <<RSCRIPT
rmarkdown::render(
    input = '/disks/PROJECT/PRJCT/Scripts/00-PRJCT.Rmd', 
    output_file = '00-PRJCT.html', 
    output_format = "bookdown::html_document2", 
    output_dir = './PRJCT',
    encoding = 'UTF-8'
)

RSCRIPT

git add PRJCT/00-PRJCT.html
git commit -m 'Update report PRJCT'
git push origin master
