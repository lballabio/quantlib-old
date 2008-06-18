
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
import getopt
from gensrc.addins import addinlist
from gensrc.exceptions import excepthook
from gensrc.configuration import initialization
from gensrc.configuration import environment

USAGE_ERROR = """
usage: %(scriptName)s -[flags] --oh_dir
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
        h - display this help message

    and --oh_dir specifies the path to the root directory of the
    ObjectHandler source code tree."""

def usage():
    print USAGE_ERROR % { 'scriptName' : sys.argv[0] }
    sys.exit(1)

# set the error handler

sys.excepthook = excepthook.gensrc_excepthook

# parse command line arguments

try:
    opts, args = getopt.getopt(sys.argv[1:], 'xopcgvelsdah', ['help', 'oh_dir='] )
except getopt.GetoptError:
    usage()

addinIds = []
ohDir=''
buildAll = False
buildIndividual = False

for o, v in opts:
    if o == '-x':
        buildIndividual = True
        addinIds.append('x')
    elif o == '-o':
        buildIndividual = True
        addinIds.append('o')
    elif o == '-p':
        buildIndividual = True
        addinIds.append('p')
    #elif o == '-c':
    #    buildIndividual = True
    #    addinIds.append('c')
    #elif o == '-g':
    #    buildIndividual = True
    #    addinIds.append('g')
    elif o == '-v':
        buildIndividual = True
        addinIds.append('v')
    elif o == '-e':
        buildIndividual = True
        addinIds.append('e')
    elif o == '-l':
        buildIndividual = True
        addinIds.append('l')
    elif o == '-s':
        buildIndividual = True
        addinIds.append('s')
    elif o == '-d':
        buildIndividual = True
        addinIds.append('d')
    elif o == '-a':
        buildAll = True
        #addinIds = [ 'x', 'o', 'p', 'c', 'g', 'v', 'e', 'l', 's', 'd' ]
        addinIds = [ 'x', 'o', 'p', 'v', 'e', 'l', 's', 'd' ]
    elif o == '--oh_dir':
        ohDir = v
    elif o in ('-h', '--help'):
        usage()
    else:
        usage()

if buildAll and buildIndividual:
    sys.exit('flag -a cannot be combined with other flags')

if not len(addinIds) or not len(ohDir):
    usage()

# Initialize the environment.

initialization.init(ohDir)

# Generate source code for chosen target projects.

addinList = addinlist.AddinList(addinIds)
addinList.generate()
#addinList.printDebug()

