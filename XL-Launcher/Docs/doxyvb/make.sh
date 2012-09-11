#!/bin/sh
#-----------------------------------------------------------------------------
# make.sh - runs Doxygen in the directory containing this script
#-----------------------------------------------------------------------------
# Creation:     09.10.2011  Vsevolod Kukol
# Last Update:  -
#
# Copyright (c) 2011 Vsevolod Kukol, sevo(at)sevo(dot)org
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#-----------------------------------------------------------------------------

SCRIPT_DIR=$(dirname $(readlink -f $0))
cd $SCRIPT_DIR
doxygen Doxyfile.linux
