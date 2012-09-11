@echo off
rem |-----------------------------------------------------------------------------
rem | MAKE.BAT - wraps gawk call to be used in a Doxyfile
rem |-----------------------------------------------------------------------------
rem | Creation:     21.06.2010  Vsevolod Kukol
rem | Last Update:  09.10.2011  Vsevolod Kukol
rem |
rem | This script is meant to be used by the FILTER_PATTERNS setting
rem | in a Doxyfile, because FILTER_PATTERNS does not allow parameters.
rem | Alternatively it can be used by the INPUT_FILTER option.
rem |
rem | Using INPUT_FILTER option:
rem |   Set options in your Doxyfile as follows:
rem |      INPUT_FILTER           = \path\to\make.bat
rem |
rem | Using FILTER_PATTERNS option:
rem |   Set options in your Doxyfile as follows:
rem |      INPUT_FILTER           = 
rem |      FILTER_PATTERNS        = *.vb=\path\to\make.bat
rem |
rem | Copyright (c) 2010-2011 Vsevolod Kukol, sevo(at)sevo(dot)org
rem |
rem | This program is free software; you can redistribute it and/or modify
rem | it under the terms of the GNU General Public License as published by
rem | the Free Software Foundation; either version 2 of the License, or
rem | (at your option) any later version.
rem |-----------------------------------------------------------------------------

"%~dp0\gawk.exe" -f "%~dp0\vbfilter.awk" %*%
