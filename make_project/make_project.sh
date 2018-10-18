#!/bin/bash

project_name=$1

mkdir -p -m 777 /disks/PROJECT/$project_name

echo "Engineer: Mickaël Canouil" >> /disks/PROJECT/$project_name/README.md

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
" > /disks/PROJECT/$project_name/$project_name.Rproj

for ifile in "Docs" "Report" "Scripts"
do
  mkdir /disks/PROJECT/$project_name/$ifile
done

mkdir -p -m 777 /disks/DATATMP/$project_name
ln -s /disks/DATATMP/$project_name /disks/PROJECT/$project_name/Data

cat /disks/PROJECT/BIOSTAT_TEAM/make_project/default_script.R \
  | sed -e "s/PRJCT/$project_name/g" > /disks/PROJECT/$project_name/Scripts/00-main.R

cat /disks/PROJECT/BIOSTAT_TEAM/make_project/default_script.Rmd \
  | sed -e "s/PRJCT/$project_name/g" > /disks/PROJECT/$project_name/Scripts/00-main.Rmd

cat /disks/PROJECT/BIOSTAT_TEAM/make_project/default_script.sh \
  | sed -e "s/PRJCT/$project_name/g" > /disks/PROJECT/$project_name/Scripts/_build.sh

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
!README.md
' > /disks/PROJECT/$project_name/.gitignore

git -C /disks/PROJECT/$project_name/ init
git -C /disks/PROJECT/$project_name/ remote add origin git@gitlab.egid.local:BioStat/$project_name.git
git -C /disks/PROJECT/$project_name/ add --all
git -C /disks/PROJECT/$project_name/ commit -am 'create project'
chmod 777 -R /disks/PROJECT/$project_name/.git
# git push -u origin master

# /disks/PROJECT/BIOSTAT_TEAM/make_project/make_project.sh ""
