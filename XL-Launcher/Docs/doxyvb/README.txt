-----------------------------------------------------------------------------
  README.txt - How to use Doxygen VB documentation filter by Vsevolod Kukol
-----------------------------------------------------------------------------
Creation:     21.06.2010  Vsevolod Kukol
Last Update:  09.10.2011  Vsevolod Kukol

Copyright (c) 2010-2011 Vsevolod Kukol, sevo(at)sevo(dot)org

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.


Doxygen Visual Basic filter
===========================

The  Doxygen Visual Basic filter is an awk script that converts
Classic VB (VB6) and VB.NET code syntax to a C#-like syntax, so it can be
read and understood by Doxygen.

It is inspired by the Visual Basic (classic) filter script written
by Mathias Henze. More information about his script can be found in
the  Helper Tools section (http://www.stack.nl/~dimitri/doxygen/helpers.html)
on the Doxygen homepage. 


How dows it work
----------------

Doxygen can preprocess all sources using a source filter. A source filter
must be executable, accept the name of the source path as last parameter
and print the resulting source to STDOUT. See the Doxygen manual for
more information.

The vbfilter.awk script is a AWK script, which can be executed using
gawk. Gawk reads the source files line by line, applys vbfilter.awk on
it and passes the output to doxygen. 


Instructions
------------

1. copy the vbfilter sources to a Doc directory in your project folder.
   This can be done by checking out from svn using TortoiseSVN on Windows,
   or the svn command on Linux:
   
       svn checkout http://trac.sevo.org/svn/doxyvb/trunk Doc
   
   The Doc folder should be created in your Project folder to generate
   documentation for a single project.
   If you use .NET and have a solution folder you could create Doc in the
   solution folder to generate one single documentation for all projects.
   
   For VB6 the structure could look like:
       MyProject\MyProject.vbp
       MyProject\[...]
       MyProject\Doc\vbfilter.[bat|sh]
       MyProject\Doc\vbfilter.awk
       MyProject\Doc\[...]

   For .NET the structure could look like:
       MySolution\MySolution.sln
       MySolution\[...]
       MySolution\Doc\vbfilter.[bat|sh]
       MySolution\Doc\vbfilter.awk
       MySolution\Doc\[...]
       MySolution\MyProject


2. Download and install the Doxygen 1.7.0 (or newer) distribution package
   from the Doxygen download page:
   http://www.stack.nl/~dimitri/doxygen/download.html#latestsrc
   
   Many Linux distributions have ready to use doxygen packages.
   
   On Ubuntu you can simply do a:
   
       sudo apt-get install doxygen


3. Optional: Install GraphViz if you want nice graphs in your documentation
			 Link: http://www.graphviz.org/Download_windows.php


4. create the Doc\Doxyfile configuration file with Doxywizard or any
   text editor (like Notepad++). You can use `samples\Doxyfile` as template.
   
   If you use Doxywizard, switch to the "Expert" tab to see all options.


   Important configuration options:

   PROJECT_NAME: set the name of your project here

   OUTPUT_DIRECTORY: full path of the Doc folder, or simply "Doc"
                     if you want to use the `make.[bat|sh]` scripts.

   EXTENSION_MAPPING:
         For VB-NET: "= .vb=csharp"
            For VB6: "= .cls=csharp .frm=csharp .ctl=csharp .bas=csharp"

   INPUT: full path to the sopurces (usually the folder containing `Doc`),
          or simply `.` in order to use the `make.[bat|sh]` scripts.

   FILE_PATTERNS:
         For VB-NET: = *.vb
            For VB6: = *.cls *.bas *.frm *.ctl
   
   INPUT_FILTER: 
         Windows: "\path\to\vbfilter.bat"
           Linux: "path/to/vbfilter.sh"

   HAVE_DOT - Set to YES, to enable Graphs in the documentation.
	          GraphViz (Step 3) has to be installed for this feature.
	
	
	Read the Doxygen manual for all other options under
	http://www.stack.nl/~dimitri/doxygen/manual.html
	
	NOTE: If you want to run Doxygen directly from inside Doxywizard,
	      set the working folder under:
		  "Step 1: Specify the working directory from which doxygen will run"
		  to the folder CONTAINING the Doc folder
		  (in most cases your Project or Solution folder!)

5. open vbfilter.awk in your favorite editor and edit the options in
   the Configuration section as you like.

6. run Doc\make.bat or Doc/make.sh to generate the documentation.


Good to know
============

Integrating in Visual Studio or MonoDevelop
-------------------------------------------
You can configure Visual Studio and MonoDevelop to run make.bat each
time you build your project. This can be done in the project settings.

In VS you can create a custom shortcut icon to run make.bat manually.


Generating CHM (Windows Help)
-----------------------------

If you want Doxygen to generate CHM (Windows Help) files, install
the Microsoft HTML Help Workshop 1.3 (htmlhelp.exe) from
http://go.microsoft.com/fwlink/?LinkId=14188
Direct link: http://go.microsoft.com/fwlink/?LinkId=14188

set the path to hhc.exe using the HHC_LOCATION option
and enable the GENERATE_HTMLHELP option.
(Doxywizard: you'll find both in the HTML section)

GENERATE_HTMLHELP      = YES
HHC_LOCATION           = "C:/Program Files/HTML Help Workshop/hhc.exe"

Other formats
-------------
HTML output is enabled by default. You can additionally enable
RTF, LaTex and some other output formats. LaTex and PDF need
a complete LaText distribution to be installed. Read the
Doxygen manula for more information.

