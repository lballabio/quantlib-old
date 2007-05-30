
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005, 2006 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

import sys
import os

# Append sys.path with subdirectory 'code' of current directory,
# this is required for rule.py
sys.path.append(os.getcwd() + '/code')

import getopt
from gensrc.Addins import addinlist
from gensrc.Exceptions import excepthook

USAGE_ERROR = """
usage: %(scriptName)s -[flags]
    where [flags] are codes to generate source code for any of:
        x - Excel addin
        o - OpenOffice.org Calc addin
        g - Guile addin
        c - C addin
        v - ValueObjects code
        e - enumerations
        l - Loop typedefs
        d - doxygen documentation files
    or
        a - all of the above
    or
        h - display this help message"""

def usage():
    print USAGE_ERROR % { 'scriptName' : sys.argv[0] }
    sys.exit(1)

# set the error handler

sys.excepthook = excepthook.gensrc_excepthook

# parse command line arguments

try:
    opts, args = getopt.getopt(sys.argv[1:], 'xogcveldah', 'help' )
except getopt.GetoptError:
    usage()

addinIds = []

for o, a in opts:
    if o == '-x':
        addinIds.append('x')
    elif o == '-o':
        addinIds.append('o')
    elif o == '-g':
        addinIds.append('g')
    elif o == '-c':
        addinIds.append('c')
    elif o == '-v':
        addinIds.append('v')
    elif o == '-e':
        addinIds.append('e')
    elif o == '-l':
        addinIds.append('l')
    elif o == '-d':
        addinIds.append('d')
    elif o == '-a':
        if len(opts) != 1: sys.exit('flag -a cannot be combined with other flags')
#        addinIds = [ 'e', 'o', 'g', 'c', 'v', 'x', 'l', 'd' ]
        addinIds = [ 'e', 'o', 'c', 'v', 'x', 'l', 'd' ]
    elif o in ('-h', '--help'):
        usage()
    else:
        usage()

if not len(addinIds):
    usage()

# generate source code for chosen target projects

addinList = addinlist.AddinList(addinIds)
addinList.generate()
#addinList.printDebug()

