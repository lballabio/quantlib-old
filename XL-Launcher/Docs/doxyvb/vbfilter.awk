#----------------------------------------------------------------------------
# vbfilter.awk - doxygen VB .NET filter script - v2.4.1
#
# Creation:     26.05.2010  Vsevolod Kukol
# Last Update:  09.10.2011  Vsevolod Kukol
#
# Copyright (c) 2010-2011 Vsevolod Kukol, sevo(at)sevo(dot)org
#
# Inspired by the Visual Basic convertion script written by
# Mathias Henze. Rewritten from scratch with VB.NET support by
# Vsevolod Kukol.
#
# requirements: doxygen, gawk
#
# usage:
#    1. create a wrapper shell script:
#        #!/bin/sh
#        gawk -f /path/to/vbfilter.awk "$1"
#        EOF
#    2. define wrapper script as INPUT_FILTER in your Doxyfile:
#        INPUT_FILTER = /path/to/vbfilter.sh
#    3. take a look on the configuration options in the Configuration
#       section of this file (inside BEGIN function)
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#---------------------------------------------------------------------------- 


BEGIN{
#############################################################################
# Configuration
#############################################################################
	# unix line breaks
	# set to 1 if using doxygen on unix with
	# windows formatted sources
	UnixLineBreaks=1;
	
	# leading shift inside classes/namespaces/etc.
	# default is "\t" (tab)
	ShiftRight="\t";
	#ShiftRight="    ";
	
	# add namespace definition at the beginning using project directory name
	# should be enabled, if no explicit namespaces are used in the sources
	# but doxygen should recognize package names.
	# in C# unlike in VB .NET a namespace must always be defined
	leadingNamespace=1;
	
	# per default the parser converts all keywords to their C# equivalents:
	# Function -> function
	# Sub -> void
	# ....
	# Set csharpStyledOutput=0 to keep the VB style in the resulting 
	# documentation.
	csharpStyledOutput=1;
	
#############################################################################
# helper variables, don't change
#############################################################################
	printedFilename=0;
	fileHeader=0;
	fullLine=1;
	classNestCounter=0;
	className[1]="";
	insideVB6Class=0;
	insideVB6ClassName="";
	insideVB6Header=0;
	insideNamespace=0;
	insideComment=0;
	insideImports=0;
	instideVB6Property=0;
	isInherited=0;
	lastLine="";
	appShift="";
	
}

#############################################################################
# shifter functions
#############################################################################
function AddShift() {
	appShift=appShift ShiftRight;
}

function ReduceShift() {
	appShift=substr(appShift,0,length(appShift)-length(ShiftRight));
}

#############################################################################
# apply dos2unix
#############################################################################
UnixLineBreaks==1{
	sub(/\r$/,"")
}

#############################################################################
# merge multiline statements into one line
#############################################################################
fullLine==0{
	fullLine=1;
	$0= lastLine$0;
	lastLine="";
}
/_$/{
	fullLine=0;
 	sub(/_$/,"");
 	lastLine=$0;
 	next;

}
#############################################################################
# remove leading whitespaces and tabs
#############################################################################
/^[ \t]/{
	sub(/^[ \t]*/, "")
}

#############################################################################
# remove Option and Region statements
#############################################################################
(/^#Region[[:blank:]]+/ ||
/.*Option[[:blank:]]+/) && insideComment!=1 {
	next;
}


#############################################################################
# VB6 file headers including class definitions
#############################################################################

# if file begins with a class definition, swith to VB6 mode
/.*[[:blank:]]+CLASS/ ||
/.*[[:blank:]]+VB\.Form[[:blank:]]+/ ||
/.*[[:blank:]]+VB\.UserControl[[:blank:]]+/ {
	insideVB6Class=1;
	next;
}

# ignore first line in VB6 forms
/.*VERSION[[:blank:]]+[0-9]+/ {
	next;
}

# get VB6 class name
/^Attribute[[:blank:]]+VB_Name.*/ {
	insideVB6ClassName=gensub(".*VB_Name[[:blank:]]+[=][[:blank:]]+\"(.*)\"","\\1","g",$0);
	insideVB6Header=1
}

# detect when class attributes begin, to recognize the end of VB6 header
/^Attribute[[:blank:]]+.*/ {
	insideVB6Header=1
	next;
}

# detect the end of VB6 header
(!(/^Attribute[[:blank:]]+.*/)) && insideVB6Class==1 && insideVB6Header<=1{
	if (insideVB6Header==0) {
		next;
	} else {
		insideVB6Header=2
	}
}


#############################################################################
# parse file header comment
#############################################################################

/^[[:blank:]]*'/ && fileHeader!=2 {

	# check if header already processed
	if (fileHeader==0) {
		fileHeader=1;
		printedFilename=1
		# print @file line at the beginning
		file=gensub(/\\/, "/", "G", FILENAME)
		print "/**\n * @file "basename[split(file, basename , "/")];
		# if inside VB6 class module, then the file header describes the
		# class itself and should be printed after 
		if (insideVB6Class==1) {
			print " * \\brief Single VB6 class module, defining " insideVB6ClassName;
			print " */";
			if (leadingNamespace==1) {	# leading namespace enabled?
				# get project name from the file path
				print "namespace "basename[split(file, basename , "/")-1]" {";
				AddShift()
			}
			print appShift " /**";
		}
	}
	sub("^[ \t]*'+"," * ");		# replace leading "'"
	print appShift $0;
	next;
}

# if .*' didn't match but header was processed, then
# the header ends here
fileHeader!=2 {
	if (fileHeader!=0) {
		print appShift " */";
	}
	fileHeader=2;
}

#############################################################################
# print simply @file, if no file header found
#############################################################################
printedFilename==0 {
	printedFilename=1;
	file=gensub(/\\/, "/", "G", FILENAME)
		if (insideVB6Class!=1) {
			print "/// @file \n";
		} else {
			print "/**\n * @file \n";
			print " * \\brief Single VB6 class module, defining " insideVB6ClassName;
			print " */";
			if (leadingNamespace==1) {	# leading namespace enabled?
				# get project name from the file path
				print "namespace "basename[split(file, basename , "/")-1]" {";
				AddShift()
			}
		}
}


#############################################################################
# skip empty lines
#############################################################################
/^$/ { next; }

#############################################################################
# convert Imports to C# style
#
# remark: doxygen seems not to recognize
#         c# using directives so converting Imports is maybe useless?
#############################################################################
/.*Imports[[:blank:]]+/ {
	sub("Imports","using");
	print $0";";
	insideImports=1;
	next;
}

#############################################################################
# print leading namespace after the using section (if present)
# or after the file header.
# namespace name is extracted from file path. the last directory name in
# the path, usually the project folder, is used.
#
# can be disabled by leadingNamespace=0;
#############################################################################
(!/^Imports[[:blank:]]+/) && leadingNamespace<=1 && fileHeader==2{
	if (leadingNamespace==1) {	# leading namespace enabled?
		# if inside VB6 file, then namespace was already printed
		if (insideVB6Class!=1) {
			file=gensub(/\\/, "/", "G", FILENAME)
			# get project name from the file path
			print "namespace "gensub(/ /,"_","G",basename[split(file, basename , "/")-1])" {";
			AddShift()
		}
		leadingNamespace=2;	# is checked by the END function to print corresponding "}"
	} else {
		# reduce leading shift
		leadingNamespace=3;
	}
	insideImports=0;
	if (insideVB6Class==1) {
		isInherited=1;
		print appShift "class " insideVB6ClassName;
	}
}



#############################################################################
# handle comments
#############################################################################

## beginning of comment
(/^[[:blank:]]*'''[[:blank:]]*/ || /^[[:blank:]]*'[[:blank:]]*[\\<][^ ].+/) && insideComment!=1 {
	if (insideEnum==1){	
		# if enum is being processed, add comment to enumComment
		# instead of printing it
		if (enumComment!="") {
			enumComment = enumComment "\n" appShift "/**";
		} else {
			enumComment = appShift "/**";
		}
		
	} else {
	
		# if inheritance is being processed, then add comment to lastLine
		# instead of printing it and process the end of
		# class/interface declaration
		
		if (isInherited==1){
			endOfInheritance();
		}
		print appShift "/**"
	}
	insideComment=1;
}

## strip leading '''
/^[[:blank:]]*'/ {
	if(insideComment==1){
		commentString=gensub("^[ \t]*[']+"," * ",1,$0);
		# if enum is being processed, add comment to enumComment
		# instead of printing it
		if (insideEnum==1){
			enumComment = enumComment "\n" appShift commentString;
		} else {
			print appShift commentString;
		}
		next;
	}
}

## end of comment
(!(/^[[:blank:]]*'/)) && insideComment==1 {
	# if enum is being processed, add comment to enumComment
	# instead of printing it
	if (insideEnum==1){	
		enumComment = enumComment "\n" appShift " */";
	} else {
		print appShift " */";
	}
	insideComment=0;
}

#############################################################################
# inline comments in c# style /** ... */
#############################################################################
# strip all commented lines, if not part of a comment block
/^'+/ && insideComment!=1 {
	next;
}
/.+'+/ && insideComment!=1 {
	sub("[[:blank:]]*'"," /**< \\brief ");
	$0 = $0" */"
}

#############################################################################
# strip compiler options
#############################################################################
/.*<.*>.*/ {
	gsub("<.*>[ ]+","");
}

#############################################################################
# simple rewrites
# vb -> c# style
# 
# keywords used by doxygen must be rewritten. All other rewrites
# are optional and depend on the csharpStyledOutput setting.
#############################################################################
/^.*Private[[:blank:]]+/ {
	sub("Private[[:blank:]]+","private ");
}
/^.*Public[[:blank:]]+/ {
	sub("Public[[:blank:]]+","public ");
}
# friend is the same as internal in c#, but Doxygen doesn't support internal,
# so make it private to get it recognized by Doxygen) and Friend appear
# in Documentation
/^.*Friend[[:blank:]]+/ {
	if (csharpStyledOutput==1)
		sub("Friend[[:blank:]]+","private Friend ");
	else {
		print appShift"/// \\remark declared as Friend in the VB original source"
		sub("Friend[[:blank:]]+","private ");
	}
}

/^.*Protected[[:blank:]]+/ {
	sub("Protected[[:blank:]]+","protected ");
}

/^.*Shared[[:blank:]]+/ {
	if (csharpStyledOutput==1)
		sub("Shared", "static");
	else 
		sub("Shared", "static Shared");
}
# Replace "Partial" by "partial" and swap order of "partial" and "public" or "private"
/^.*Partial[[:blank:]]+/ {
	sub("Partial","partial");
	if($1 == "partial" && $2 ~ /public|private/) {
		$1 = $2;
		$2 = "partial";
	}	
}

# Const -> const
/\<Const\>/ {
	gsub(/\<Const\>/,"const");
}

# No WithEvents in C# - let's treat it like variables
/^.*WithEvents[[:blank:]]+/ && (csharpStyledOutput==1) {
	sub("WithEvents","");
}

# Overrides -> override
/[[:blank:]]Overrides[[:blank:]]/ && (csharpStyledOutput==1) {
	sub("Overrides","override");
}

# Overridable -> virtual
/[[:blank:]]Overridable[[:blank:]]/ && (csharpStyledOutput==1) {
	sub("Overridable","virtual");
}

# Optional has to be removed for c# style
/[[:blank:]]Optional[[:blank:]]/ && (csharpStyledOutput==1) {
	gsub("Optional"," ");
}

/\<String\>/ && (csharpStyledOutput==1) {
	gsub(/\<String\>/,"string");
}

/\<Boolean\>/ && (csharpStyledOutput==1) {
	gsub(/\<Boolean\>/,"bool");
}

/\<Char\>/ && (csharpStyledOutput==1) {
	gsub(/\<Char\>/,"char");
}

/\<Byte\>/ && (csharpStyledOutput==1) {
	gsub(/\<Byte\>/,"byte");
}

/\<Short\>/ && (csharpStyledOutput==1) {
	gsub(/\<Short\>/,"short");
}

/\<Integer\>/ && (csharpStyledOutput==1) {
	gsub(/\<Integer\>/,"int");
}

/\<Long\>/ && (csharpStyledOutput==1) {
	gsub(/\<Long\>/,"long");
}

/\<Single\>/ && (csharpStyledOutput==1) {
	gsub(/\<Single\>/,"float");
}

/\<Double\>/ && (csharpStyledOutput==1) {
	gsub(/\<Double\>/,"double");
}

/\<Decimal\>/ && (csharpStyledOutput==1) {
	gsub(/\<Decimal\>/,"decimal");
}

/\<Date\>/ && (csharpStyledOutput==1) {
	gsub(/\<Date\>/,"DateTime");
}

/\<Object\>/ && (csharpStyledOutput==1) {
	gsub(/\<Object\>/,"object");
}

/\<Delegate\>/ && (csharpStyledOutput==1) {
	gsub(/\<Delegate\>/,"delegate");
}

/\<AddressOf\>/ && (csharpStyledOutput==1) {
	gsub(/\<AddressOf\>/,"\\&");
}

#############################################################################
# Enums
#############################################################################
/^Enum[[:blank:]]+/ || /[[:blank:]]+Enum[[:blank:]]+/ {
	sub("Enum","enum");
	sub("+*[[:blank:]]As.*",""); # enums shouldn't have type definitions
	print appShift $0"\n"appShift"{";
	insideEnum=1;
	lastEnumLine="";
	AddShift()
	next;
}

/^[ \t]*End[[:blank:]]+Enum/ && insideEnum==1{
	print appShift lastEnumLine;
	ReduceShift()
	print appShift "}"
	insideEnum=0;
	lastEnumLine="";
	enumComment="";
	next;
}

insideEnum==1 {
	if ( lastEnumLine == "" ) {
		lastEnumLine = $0;
		if (enumComment!="") print enumComment;
		enumComment="";
	} else {
		commentPart=substr(lastEnumLine,match(lastEnumLine,"[/][*][*]<"));
		definitionPart=substr(lastEnumLine,0,match(lastEnumLine,"[/][*][*]<")-2);
		if (definitionPart=="") print appShift commentPart ",";
		else {
			print appShift definitionPart ", " commentPart
		}
		lastEnumLine = $0;
		# print leading comment of next element, if present
		if (enumComment!="") print enumComment;
		enumComment="";
	}
	next;
}

#############################################################################
# Declares
#############################################################################

/.*Declare[[:blank:]]+/ {
	libName=gensub(".+Lib[[:blank:]]+\"([^ ]*)\"[[:blank:]].*","\\1","g");
	if (match($0,"Alias")>0) aliasName=gensub(".+Alias[[:blank:]]+\"([^ ]*)\"[[:blank:]].*"," (Alias: \\1)","g");
	print appShift "/** Is imported from extern library: " libName aliasName " */";
	if (csharpStyledOutput==1)
		sub(/Declare[[:blank:]]+/,"extern ");
	libName="";
	aliasName="";
}

# remove lib and alias from declares
/.*Lib[[:blank:]]+/ {
	sub("Lib[[:blank:]]+[^[:blank:]]+","");
	sub("Alias[[:blank:]]+[^[:blank:]]+","");
}



#############################################################################
# types (handle As and Of)
#############################################################################

/.*[(]Of[ ][^ ]+[)].*/ {
	$0=gensub("[(]Of[ ]([^ )]+)[)]", "<\\1>","g",$0);
}

## converts a single type definition to c#
##  "Var As Type" -> "Type Var"
##  "Var As New Type" -> "Type Var = new Type()"
function convertSimpleType(Param)
{
	l=split(Param, aParam, " ")
	newParam="";
	for (j = 1; j <= l; j++) {
		if (aParam[j] == "As") {			
			typeIndex = 1;
			if (aParam[j+1] == "New") {
				typeIndex = 2;
				aParam[j+1] = "";
			}
			aParam[j]=aParam[j-1];
			aParam[j-1]=aParam[j+typeIndex];
			aParam[j+typeIndex]="";
		}
	}
	for (j = 1; j <= l; j++) {
		if (aParam[j]!="") {
			if (j == 1) {
				newParam=aParam[j];
			} else {
				newParam=newParam " " aParam[j];
			}
		}
	}
	l="";
	delete aParam;
	return newParam;
}

function rindex(string, find) {
	ns=length(string);
	nf=length(find);
	for (r = ns + 1 - nf; r>=1; r--)
		if (substr(string, r, nf) == find)
			return r;
	return 0;
}

function findEndArgs(string) {
	ns=length(string);
	nf=length(")");
	for (r = ns + 1 - nf; r>=1; r--) {
		if ((substr(string, r, nf) == ")") && (substr(string, r - 1, nf) != "(")) {
			return r;
		}
	}
	return 0;
}

#(/.*Function[[:blank:]]+/ ||
#/.*Sub[[:blank:]]+/ ||
#/.*Property[[:blank:]]+/ ||
#/.*Event[[:blank:]]+/ ||
#/.*Operator[[:blank:]]+/) &&
/.*As[[:blank:]]+/ {
	gsub("ByVal","");
	# keep ByRef to make pointers differ from others
	# gsub("ByRef","");
	if (csharpStyledOutput==1)
		gsub("ByRef","ref");
	
	# simple member definition without brackets
	if (index($0,"(") == 0) {
		$0=convertSimpleType($0);
	}
	else if (match($0, ".*Sub[[:blank:]].+") ||
	    match($0, ".*Function[[:blank:]].+") ||
	    match($0, ".*Property[[:blank:]].+") ||
	    match($0, ".*Event[[:blank:]].+") ||
	    match($0, ".*Operator[[:blank:]].+")) {
		# parametrized member
		preParams=substr($0,0,index($0,"(")-1) 
		lpreParams=split(preParams, apreParams , " ")
		
		Params=substr($0,index($0,"(")+1,findEndArgs($0)-index($0,"(")-1)
		
		lParams=split(Params, aParams, ",")
		Params="";
		# loop over params and convert them
		if (lParams > 0) {
			for (i = 1; i <= lParams; i++) {
			
				
				if(match(aParams[i],/.+[(][)].*/)) {
					lParam=split(aParams[i], aParam , " ")
					for (j = 1; j <= lParam; j++) {
						if (aParam[j] == "As") {
							aParam[j-1]=gensub("[(][)]","","g",aParam[j-1]);
							aParam[j+1]=gensub("[(][)]","","g",aParam[j+1]);
							aParam[j+1]=aParam[j+1]"[]";
						}
					}
					for (j = 1; j <= lParam; j++) {
						if (j == 1) {
							aParams[i]=aParam[j];
						} else {
							aParams[i]=aParams[i] " " aParam[j];
						}
					}
				}
			
				if (i == 1) {
					Params=convertSimpleType(aParams[i]);
				} else {
					Params=Params ", " convertSimpleType(aParams[i]);
				}
			}
			postParams=substr($0,findEndArgs($0)+1)
		} else { 
			postParams=substr($0,rindex($0, ")")+1)
		}
		#postParams=substr($0,findEndArgs($0)+1) 
		# handle type def of functions and properties
		lpostParams=split(postParams, apostParams , " ")
		if (lpostParams > 0) {
			if (apostParams[1] == "As") {
				## functions with array as result
				if (match(apostParams[2], ".*[(].*[)].*")) {
					apostParams[2]=gensub("[(].*[)]","[]","g",apostParams[2]);
				}
				##
				apreParams[lpreParams+1]=apreParams[lpreParams];
				apreParams[lpreParams]=apostParams[2];
				lpreParams++;
				apostParams[1]="";
				apostParams[2]="";
			}			
		}
		
		# put everything back together
		$0="";
		for (i = 1; i <= lpreParams; i++) {
			if (apreParams[i]!="")	$0=$0 apreParams[i]" ";
		}

		$0=$0 "("Params") ";
		for (i = 1; i <= lpostParams; i++) {
			if (apostParams[i]!="")	$0=$0 apostParams[i]" ";
		}
		
		# cleanup mem
		lParams="";
		delete aParams;
		lpostParams="";
		delete apostParams;
		lpreParams="";
		delete apreParams;
	}
	else {
		# convert arrays
		$0=convertSimpleType($0);
		
		lLine=split($0, aLine , " ")
		for (j = 1; j <= lLine; j++) {
			if (match(aLine[j], ".*[(].*[)].*")) {
				aLine[j]=gensub("[(].*[)]","","g",aLine[j]);
				aLine[j-1]=aLine[j-1]"[]";
			}
		}
		$0 = "";
		for (j = 1; j <= lLine; j++) {
			$0 = $0 aLine[j] " ";
		}
	}
}

#############################################################################
# Rewrite Subs handling events if csharpStyledOutput=1
#############################################################################

/.*[[:blank:]]Handles[[:blank:]]+/ && (csharpStyledOutput==1) {
	name=gensub(/(.*)[[:blank:]]+Handles[[:blank:]]+(\w+)/,"\\2","g",$0);
	print appShift "/// \\remark Handles the " name " event.";
	$0=  gensub(/(.*)[[:blank:]]+Handles[[:blank:]]+(.*)/,"\\1","g",$0);
}
		
#############################################################################
# namespaces
#############################################################################
/^Namespace[[:blank:]]+/ || /[[:blank:]]+Namespace[[:blank:]]+/ {
	sub("Namespace","namespace");
	insideNamespace=1;
	print appShift $0" {";
	AddShift();
	next;
}

/^.*End[[:blank:]]+Namespace/ && insideNamespace==1{
	ReduceShift();
	print appShift "}";
	insideNamespace=0;
	next;
}

#############################################################################
# interfaces, classes, structures
#############################################################################
/^Interface[[:blank:]]+/ ||
/.*[[:blank:]]Interface[[:blank:]]+/ ||
/^Class[[:blank:]]+/ ||
/.*[[:blank:]]Class[[:blank:]]+/ ||
/^Structure[[:blank:]]+/ ||
/.*[[:blank:]]Structure[[:blank:]]+/ ||
/^Type[[:blank:]]+/ ||
/(friend|protected|private|public).*[[:blank:]]+Type[[:blank:]]+/ {
	sub("Interface","interface");
	sub("Class","class");
	sub("Structure","struct");
	sub("Type","struct");
	if(isInherited) {
		endOfInheritance();
	}
	classNestCounter++;
	
	# save class name for constructor handling
	className[classNestCounter]=gensub(".+class[[:blank:]]+([^ ]*).*","\\1","g");
	isInherited=1;
	print appShift $0;
	next;
}

# handle constructors
/.*Sub[[:blank:]]+New.*/ && className[classNestCount]!="" {
	sub("New", "New " className[classNestCount]);
}

function endOfInheritance()
{
		isInherited=0;
		if (lastLine!="") print appShift lastLine;
		print appShift "{";
		AddShift();
		lastLine="";
		return 0;
}

# handle inheritance
isInherited==1{
	if(($0 ~ /^[[:blank:]]*Inherits[[:blank:]]+/) || ($0 ~ /^[[:blank:]]*Implements[[:blank:]]+/)) {
		
		if ( lastLine == "" )
		{
			sub("Inherits",":");
			sub("Implements",":");
			lastLine=$0;
		}
		else
		{
			sub(".*Inherits",",");
			sub(".*Implements",",");
			lastLine=lastLine $0;
		}
	}
	else {
		endOfInheritance();
	}
}

(/.*End[[:blank:]]+Interface/ ||
 /.*End[[:blank:]]+Class.*/ ||
 /.*End[[:blank:]]+Structure/ ||
 /.*End[[:blank:]]+Type/) &&
 (classNestCounter >= 1){
	ReduceShift();
	print appShift "}";
	delete className[classNestCounter+1];
	next;
}


#############################################################################
# Replace Implements with a comment linking to the interface member,
#   since Doxygen does not recognize members with names that differ
#   from their corresponding interface members
#############################################################################
/.+[[:blank:]]+Implements[[:blank:]]+/ {
	if ($0 ~ /.*Property[[:blank:]]+.*/) {
		$0=gensub("(Implements)[[:blank:]]+(.+)$","/** Implements <see cref=\"\\2\"/> */","g",$0); 
	} else {
		$0=gensub("(Implements)[[:blank:]]+(.+)$","/**< Implements <see cref=\"\\2\"/> */","g",$0); 
	}
}

#############################################################################
# Properties
#############################################################################

/^Property[[:blank:]]+/ ||
/.*[[:blank:]]+Property[[:blank:]]+/ {
	sub("[(][)]","");

	if (csharpStyledOutput==1)
	{
		# remove Property keyword
		gsub("^Property[[:blank:]]","")	
		gsub("[[:blank:]]Property[[:blank:]]"," ")
	}
	
	if (match($0,"[(].+[)]")) {
		$0=gensub("[(]","[","g");
		$0=gensub("[)]","]","g");
	} else {
		$0=gensub("[(][)]","","g");
	}
	
	# add c# styled get/set methods
	if ((match($0,"ReadOnly")) || (match($0,"Get"))) {
		if (csharpStyledOutput==1)
			sub("ReadOnly[[:blank:]]","");
		if (instideVB6Property == 1)
		{
			instideVB6Property = 0;
			$0=gensub("[[:blank:]]Get|[[:blank:]]Set|[[:blank:]]Let","","g");
			print appShift $0 "\n" appShift "{ get; set; }";
		}
		else
		{
			$0=gensub(" Get| Set| Let","","g");
			print appShift $0 "\n" appShift "{ get; }";
		}
	} else {
		if ((match($0, "Let") || match($0, "Set"))) {
			instideVB6Property = 1;
			next;
		}
			$0=gensub(" Get| Set| Let","","g");
		print appShift $0 "\n" appShift "{ get; set; }";
		next;
	}
	instideVB6Property = 0;
	next;
}


/.*Operator[[:blank:]]+/ {
	$0=gensub("Operator[[:blank:]]+([^ ]+)[[:blank:]]+","\\1 operator ","g",$0);
}

#############################################################################
# process everything else
#############################################################################
/.*private[[:blank:]]+/ ||
/.*public[[:blank:]]+/ ||
/.*protected[[:blank:]]+/ ||
/.*friend[[:blank:]]+/ ||
/.*internal[[:blank:]]+/ ||
/^Sub[[:blank:]]+/ ||
/.*[[:blank:]]+Sub[[:blank:]]+/ ||
/^Function[[:blank:]]+/ ||
/.*[[:blank:]]+Function[[:blank:]]+/ ||
/.*declare[[:blank:]]+/ ||
/^Event[[:blank:]]+/ ||
/.*[[:blank:]]+Event[[:blank:]]+/ ||
/.*const[[:blank:]]+/ ||
/.*[[:blank:]]+const[[:blank:]]+/ {
		
	# remove square brackets from reserved names
	# but do not match array brackets
	#  "Integer[]" is not replaced
	#  "[Stop]" is replaced by "Stop"	
	$0=gensub("([^[])([\\]])","\\1","g"); 
	$0=gensub("([[])([^\\]])","\\2","g"); 

	if (csharpStyledOutput==1)
	{
		# subs are functions returning void
		gsub("[[:blank:]]Sub[[:blank:]]+"," void ");
		gsub("^Sub[[:blank:]]+","void ");
		gsub("[[:blank:]]Event[[:blank:]]+"," event ");
		gsub("^Event[[:blank:]]+","event ");
	}
	
	# add semicolon before inline comment
	if( $0 != "" ){	
		commentPart=substr($0,index($0,"/"));
		definitionPart=substr($0,0,index($0,"/")-1);
		if ( definitionPart != "" && commentPart != "") {
			$0 = appShift definitionPart"; "commentPart
		} else {
			$0 = appShift $0";";
		}
		# with Declares we can have a superfluous "Function" here.
		sub("Function","");		
		print $0
	}
}

END{
	# close file header if file contains no code
	if (fileHeader!=2 && fileHeader!=0) {
		print " */";
	}
	if (insideVB6Class==1) print ShiftRight "}";
	if (leadingNamespace==2) print "}";
}
