
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005, 2006 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

import sys
from gensrc.Exceptions import exceptions
# initialize the exception handler
sys.excepthook = exceptions.gensrc_excepthook
import getopt
from gensrc.Addins import addinlist

def usage():
    """Fail with a message documenting command line arguments."""
    errorMessage = 'usage: ' + sys.argv[0] + """ -[targets]
    where [targets] is any of:
        q - generate source for QuantLibAddin
        e - generate source for Excel addin
        o - generate source for OpenOffice.org Calc addin
        c - generate source for C addin
        g - generate source for Guile addin
        d - generate doxygen documentation files
        v - generate ValueObjects
        l - generate loop typedefs
    or
        a - all of the above
    or
        h - display this help message"""
    sys.exit(errorMessage)

# parse command line arguments

try:
    opts, args = getopt.getopt(sys.argv[1:], 'qeocgdvlah', 'help' )
except getopt.GetoptError:
    usage()

addinIds = []

for o, a in opts:
    if o == '-q':
        addinIds.append('q')
    elif o == '-e':
        addinIds.append('e')
    elif o == '-o':
        addinIds.append('o')
    elif o == '-c':
        addinIds.append('c')
    elif o == '-g':
        addinIds.append('g')
    elif o == '-d':
        addinIds.append('d')
    elif o == '-v':
        addinIds.append('v')
    elif o == '-l':
        addinIds.append('l')
    elif o == '-a':
        if len(opts) != 1: sys.exit('flag -a cannot be combined with other flags')
        addinIds = [ 'q', 'e', 'o', 'c', 'g', 'd', 'v', 'l' ]
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

