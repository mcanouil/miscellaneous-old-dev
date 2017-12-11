#!/bin/bash

projectName=$1
mkdir /disks/PROJECT/$projectName

currentdir=`pwd`

printf "Engineer: Mickaël Canouil  " >> /disks/PROJECT/$projectName/README.md

for iFile in "Docs" "Report" "Scripts"
do
   mkdir /disks/PROJECT/$projectName/$iFile
done

mkdir /disks/DATA/$projectName/
ln -s /disks/DATA/$projectName/ /disks/PROJECT/$projectName/Data

cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/defaultScript.R | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Scripts/00-${projectName}.R"
cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/defaultScript.Rmd | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Scripts/00-${projectName}.Rmd"

cp /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/gitignore.txt /disks/PROJECT/$projectName/.gitignore

cd /disks/PROJECT/$projectName/
git init
git add .
git commit -m 'Initialise'
git remote add origin git@gitlab.egid.local:BioStat/$projectName.git

cd $currentdir
unset currentdir

# /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/makeProject.sh ""