#!/bin/sh
#-----------------------------------------------------------------------------
# make.sh - wraps gawk call to be used in a Doxyfile
#-----------------------------------------------------------------------------
# Creation:     09.10.2011  Vsevolod Kukol
# Last Update:  -
#
# This script is meant to be used by the FILTER_PATTERNS setting
# in a Doxyfile, because FILTER_PATTERNS does not allow parameters.
# Alternatively it can be used by the INPUT_FILTER option.
#
# Using INPUT_FILTER option:
#   Set options in your Doxyfile as follows:
#      INPUT_FILTER           = /path/to/make.sh
#
# Using FILTER_PATTERNS option:
#   Set options in your Doxyfile as follows:
#      INPUT_FILTER           = 
#      FILTER_PATTERNS        = *.vb=/path/to/make.sh
#
# Copyright (c) 2011 Vsevolod Kukol, sevo(at)sevo(dot)org
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#-----------------------------------------------------------------------------

SCRIPT_DIR=$(dirname $(readlink -f $0))
gawk -f $SCRIPT_DIR/vbfilter.awk "$1"
