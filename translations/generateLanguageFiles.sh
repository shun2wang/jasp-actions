#!/bin/bash

MODULE_NAMES=${1:-"jaspTestModule"}
MODULE_NAMES=$(echo $MODULE_NAMES | cut -d'/' -f 2)
PATH_TO_R_FILE=$2
LANGUAGE_CODES=${3:-"nl de es pt ja tr gl zh_Hans zh_Hant id fr ru pl it sr"}

create_file_if_it_doesnt_exist() {
	if [ ! -f "$1" ]; then
		touch "$1"
	fi
}

clean_po_whitespace_msgstrs() {
	sed -i -E 's/^msgstr[[:space:]]+"[[:space:]]*"$/msgstr ""/' "$1"
}

do_lupdate()	{	lupdate -locations none -extensions cpp,h,qml,cpp.in,h.in -recursive $1 -ts $2;	}
do_msgattrib()	{	msgattrib --no-obsolete --no-location ${1} -o $1;	}

# in case people try this at home
validate_module()
{
	if [ ! -d "$1/inst/qml" ] | [ ! -d "$1/R" ]; then
		echo "either $1/inst/qml or $1/R doesn't exist, so $1 is probably not a JASP module."
	fi
}

for moduleName in ${MODULE_NAMES[@]}
do
	echo Module: ${moduleName}
 	
 	cd ${moduleName}
 	git pull
  	git status
   	cd ..

	if [ ${moduleName} = "jasp-desktop" ]
	then
		FILE=${moduleName}/Desktop/po/jaspDesktop.pot
	else
		validate_module ${moduleName}

		# ensure we don't crash if no translation infrastructure exists
		mkdir -p "${moduleName}/po"
		mkdir -p "${moduleName}/inst/qml/translations"

		FILE=${moduleName}/po/QML-${moduleName}.pot
		create_file_if_it_doesnt_exist $FILE
	fi

	do_lupdate   $moduleName $FILE
	do_msgattrib $FILE

	for languageCode in ${LANGUAGE_CODES[@]}
	do
		echo Generating language File: ${languageCode}
		
		if [ ${moduleName} = "jasp-desktop" ]
		then
			LANGUAGEFILE=${moduleName}/Desktop/po/jaspDesktop-${languageCode}.po
			QMFILE=${moduleName}/Resources/Translations/jaspDesktop-${languageCode}.qm
		else
			LANGUAGEFILE=${moduleName}/po/QML-${languageCode}.po
			QMFILE=${moduleName}/inst/qml/translations/${moduleName}-${languageCode}.qm
		fi
		
		clean_po_whitespace_msgstrs "$LANGUAGEFILE"
		do_lupdate   $moduleName $LANGUAGEFILE
		do_msgattrib $LANGUAGEFILE
		lrelease $LANGUAGEFILE -qm $QMFILE

	done
	
	if [ ${moduleName} != "jasp-desktop" ]
	then
		echo Rscript $PATH_TO_R_FILE/translate.R ${moduleName}
		Rscript $PATH_TO_R_FILE/translate.R ${moduleName};
	fi
done
