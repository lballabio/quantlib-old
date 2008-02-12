
"""
 Copyright (C) 2005, 2006, 2007, 2008 Eric Ehlers
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
from gensrc.Utilities import utilities
from gensrc.Types import typelist
from gensrc.Configuration import configuration
from gensrc.Configuration import environment

USAGE_ERROR = """
usage: %(scriptName)s -[flags]
    where [flags] specify that source code is to be generated for any of:
        x - Excel addin
        o - OpenOffice.org Calc addin
        p - C++ addin
       [c - C addin]
       [g - Guile addin]
        v - ValueObjects code
        e - Enumerations
        l - Loops
        s - Serialization
        d - Doxygen documentation files
    or
        a - All of the above
    or
        h - display this help message"""

def usage():
    print USAGE_ERROR % { 'scriptName' : sys.argv[0] }
    sys.exit(1)

# set the error handler

sys.excepthook = excepthook.gensrc_excepthook

# parse command line arguments

try:
    opts, args = getopt.getopt(sys.argv[1:], 'xopcgvelsdah', 'help' )
except getopt.GetoptError:
    usage()

addinIds = []

for o, a in opts:
    if o == '-x':
        addinIds.append('x')
    elif o == '-o':
        addinIds.append('o')
    elif o == '-p':
        addinIds.append('p')
    #elif o == '-c':
    #    addinIds.append('c')
    #elif o == '-g':
    #    addinIds.append('g')
    elif o == '-v':
        addinIds.append('v')
    elif o == '-e':
        addinIds.append('e')
    elif o == '-l':
        addinIds.append('l')
    elif o == '-s':
        addinIds.append('s')
    elif o == '-d':
        addinIds.append('d')
    elif o == '-a':
        if len(opts) != 1: sys.exit('flag -a cannot be combined with other flags')
        #addinIds = [ 'x', 'o', 'p', 'c', 'g', 'v', 'e', 'l', 's', 'd' ]
        addinIds = [ 'x', 'o', 'p', 'v', 'e', 'l', 's', 'd' ]
    elif o in ('-h', '--help'):
        usage()
    else:
        usage()

if not len(addinIds):
    usage()

# Initialize the environment.  Logically this functionality belongs in the
# constructor of the Environment class but that isn't possible because of
# problems with circular imports.

config = utilities.serializeObject(configuration.Configuration, 'config/config')
environment.Environment.instance().setConfiguration(config)
environment.Environment.instance().setTypes(typelist.TypeList())

# Generate source code for chosen target projects.

addinList = addinlist.AddinList(addinIds)
addinList.generate()
#addinList.printDebug()

