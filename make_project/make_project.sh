#!/bin/bash

project_name=$1

mkdir -p -m 777 ~/$project_name

echo "Engineer: Mickaël Canouil" >> ~/$project_name/README.md

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

BuildType: Custom
CustomScriptPath: Scripts/_build.sh

QuitChildProcessesOnExit: Yes
" > ~/$project_name/$project_name.Rproj

for ifile in "Docs" "Report" "Scripts" "Data"
do
  mkdir -p -m 777 ~/$project_name/$ifile
done

cat ~/DEV/make_project/default_script.R \
  | sed -e "s/PRJCT/$project_name/g" > ~/$project_name/Scripts/00-main.R

cat ~/DEV/make_project/default_script.Rmd \
  | sed -e "s/PRJCT/$project_name/g" > ~/$project_name/Scripts/00-main.Rmd

cat ~/DEV/make_project/default_script.sh \
  | sed -e "s/PRJCT/$project_name/g" > ~/$project_name/Scripts/_build.sh

echo '/*

**.Rproj.user
**.Rhistory
**.RData
**.Rdata
**.Ruserdata
**.rdb
**.rdx
**.rds

**.glo
**.ist
**.out
**.nav
**.log
**.bbl
**.blg
**.aux
**.toc
**.snm
**.html
**.pdf

!/Scripts/
!/Docs/*
!/Report/*
!README.md
' > ~/$project_name/.gitignore

git -C ~/$project_name/ init
git -C ~/$project_name/ remote add origin git@github.com:mcanouil/$project_name.git
git -C ~/$project_name/ add --all
git -C ~/$project_name/ commit -am 'create project'
# chmod 777 -R ~/$project_name/.git
# git push -u origin master

# ~/DEV/make_project/make_project.sh ""
