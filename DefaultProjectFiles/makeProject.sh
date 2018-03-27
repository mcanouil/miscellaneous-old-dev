#!/bin/bash

projectName=$1
mkdir /disks/PROJECT/$projectName

currentdir=`pwd`

printf "Engineer: Mickaël Canouil  " >> /disks/PROJECT/$projectName/README.md

printf "Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 4
Encoding: UTF-8

RnwWeave: knitr
LaTeX: pdfLaTeX

AutoAppendNewline: Yes
" >> /disks/PROJECT/$projectName/$projectName.Rproj

for iFile in "Docs" "Report" "Scripts"
do
   mkdir /disks/PROJECT/$projectName/$iFile
done

mkdir /disks/DATA/$projectName/
ln -s /disks/DATA/$projectName/ /disks/PROJECT/$projectName/Data

cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/defaultScript.R | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Scripts/00-${projectName}.R"
cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/defaultScript.Rmd | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Scripts/00-${projectName}.Rmd"
cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/defaultScript.sh | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Scripts/00-${projectName}.sh"

cp /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/gitignore.txt /disks/PROJECT/$projectName/.gitignore

cd /disks/PROJECT/$projectName/
git init
git remote add origin git@gitlab.egid.local:BioStat/$projectName.git
git add --all
git commit -m 'Initial commit'
git push -u origin master

cd $currentdir
unset currentdir

# /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/makeProject.sh ""