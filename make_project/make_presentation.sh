#!/bin/bash

project_name=$1

mkdir -p -m 755 $project_name

echo "# $project_name
<!-- badges: start -->
[![Travis build status](https://travis-ci.org/mcanouil/$project_name.svg?branch=master)](https://travis-ci.org/mcanouil/$project_name)
<!-- badges: end -->

This is the work-in-progress repo for the slides about $project_name.
" > $project_name/README.md

printf "Version: 1.0

RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: No

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: knitr
LaTeX: pdfLaTeX

AutoAppendNewline: Yes

QuitChildProcessesOnExit: Yes
" > $project_name/$project_name.Rproj

printf '.Rproj.user
.Rhistory
.RData
.Ruserdata
' > $project_name/.gitignore

printf '^.*\.Rproj$
^\.Rproj\.user$
' > $project_name/.Rbuildignore

echo "Package: $project_name
Title: $project_name
Version: 0.0.1
Imports: 
    rmarkdown
" > $project_name/DESCRIPTION

echo "language: R
cache: packages

pandoc_version: 2.3.1

script: Rscript -e 'rmarkdown::render(input = \"index.Rmd\")'

before_deploy: Rscript -e 'remotes::install_cran(\"pkgdown\")'

deploy:
  provider: script
  script: Rscript -e 'source(\"https://raw.githubusercontent.com/mcanouil/DEV/master/R/deploy.R\"); deploy_site(\"index.Rmd\", commit_message = \"$project_name\")'
  skip_cleanup: true
" > $project_name/.travis.yml

chmod -R 755  $project_name

git -C $project_name/ init
git -C $project_name/ add --all
git -C $project_name/ commit -am 'create project'
curl -u "mcanouil" https://api.github.com/user/repos -d "{\"name\":\"$project_name\"}"
git -C $project_name/ remote add origin  https://github.com/mcanouil/$project_name.git
git -C $project_name/ checkout --orphan gh-pages
git -C $project_name/ rm -rf .
git -C $project_name/ commit --allow-empty -m 'Initial gh-pages commit'
git -C $project_name/ checkout master
git -C $project_name/ push origin master
git -C $project_name/ push origin gh-pages

