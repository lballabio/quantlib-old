@echo off
rem |-----------------------------------------------------------------------------
rem | MAKE.BAT - runs Doxygen in the directory containing this script
rem |-----------------------------------------------------------------------------
rem | Creation:     21.06.2010  Vsevolod Kukol
rem | Last Update:  25.06.2010  Vsevolod Kukol
rem |
rem | Copyright (c) 2010 Vsevolod Kukol, sevo(at)sevo(dot)org
rem |
rem | This program is free software; you can redistribute it and/or modify
rem | it under the terms of the GNU General Public License as published by
rem | the Free Software Foundation; either version 2 of the License, or
rem | (at your option) any later version.
rem |-----------------------------------------------------------------------------

rem run Doxygen
doxygen "%~dp0\Doxyfile.windows"

