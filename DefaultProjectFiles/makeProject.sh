#!/bin/bash

projectName=$1
mkdir /disks/PROJECT/$projectName

printf "Engineer: Mickael Canouil" >> /disks/PROJECT/$projectName/README.txt

for iFile in "Docs" "Report" "Scripts"
do
   mkdir /disks/PROJECT/$projectName/$iFile
done

mkdir /disks/DATA/$projectName/
ln -s /disks/DATA/$projectName/ /disks/PROJECT/$projectName/Data

# mkdir /disks/PROJECT/$projectName/Report/Headers/
# cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/Headers/Header_Article.tex | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Report/Headers/Header_Article.tex"
# cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/Body_Article.tex | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Report/Report_${projectName}.tex"
# cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/Headers/Header_Beamer.tex | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Report/Headers/Header_Beamer.tex"
# cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/Body_Beamer.tex | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Report/Beamer_${projectName}.tex"
# cp -r /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/Logos/ /disks/PROJECT/$projectName/Report/
# cp -r /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/Background/ /disks/PROJECT/$projectName/Report/
# cp -r /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/template.docx /disks/PROJECT/$projectName/Report/Headers/

cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/defaultScript.R | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/Scripts/00-${projectName}_main.R"

# mkdir /disks/PROJECT/$projectName/ShinyApp/
# mkdir /disks/PROJECT/$projectName/ShinyApp/$projectName/
# cat /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/ShinyApp/app.R | sed -e "s/PRJCT/$projectName/g" > "/disks/PROJECT/$projectName/ShinyApp/$projectName/app.R"
# cp -r /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/ShinyApp/www/ /disks/PROJECT/$projectName/ShinyApp/$projectName/

# /disks/PROJECT/Mickael/DEV/DefaultProjectFiles/makeProject.sh ""