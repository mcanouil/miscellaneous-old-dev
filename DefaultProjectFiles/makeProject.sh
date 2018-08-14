#!/bin/bash

projectName=$1
mkdir /disks/PROJECT/$projectName

currentdir=`pwd`

echo "Engineer: Mickaël Canouil  \n" >> /disks/PROJECT/$projectName/README.md

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
" >> /disks/PROJECT/$projectName/$projectName.Rproj

for iFile in "Docs" "Report" "Scripts"
do
  mkdir /disks/PROJECT/$projectName/$iFile
done

mkdir /disks/DATATMP/$projectName/
ln -s /disks/DATATMP/$projectName/ /disks/PROJECT/$projectName/Data

cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/defaultScript.R | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Scripts/00-${projectName}.R"
cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/defaultScript.Rmd | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Scripts/00-${projectName}.Rmd"
cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/defaultScript.sh | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Scripts/_build.sh"

cp /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/gitignore.txt /disks/PROJECT/$projectName/.gitignore

cd /disks/PROJECT/$projectName/
git init
git remote add origin git@gitlab.egid.local:BioStat/$projectName.git
git add --all
git commit -m 'Initial commit'
# git push -u origin master

cd $currentdir
unset currentdir

# /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/makeProject.sh ""
