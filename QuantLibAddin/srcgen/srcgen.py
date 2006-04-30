
"""
 Copyright (C) 2005, 2006 Eric Ehlers
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
import getopt
import addinqla
import addinexcel
import addincalc
import addinc
import addinguile
import addindoxygen
import valueobjects
import utilities

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
    or
        a - all of the above
    or
        h - display this help message"""
    sys.exit(errorMessage)

# parse command line arguments

try:
    opts, args = getopt.getopt(sys.argv[1:], 'qeocgdahv', 'help' )
except getopt.GetoptError:
    usage()

addins = []

for o, a in opts:
    if o in ('-h', '--help'):
        usage()
    elif o == '-q':
        addins.append(utilities.serializeObject(addinqla.AddinQla))
    elif o == '-e':
        addins.append(utilities.serializeObject(addinexcel.AddinExcel))
    elif o == '-o':
        addins.append(utilities.serializeObject(addincalc.AddinCalc))
    elif o == '-c':
        addins.append(utilities.serializeObject(addinc.AddinC))
    elif o == '-g':
        addins.append(utilities.serializeObject(addinguile.AddinGuile))
    elif o == '-d':
        addins.append(utilities.serializeObject(addindoxygen.AddinDoxygen))
    elif o == '-v':
        addins.append(utilities.serializeObject(valueobjects.ValueObjects))
    elif o == '-a':
        if len(opts) != 1: sys.exit('flag -a cannot be combined with other flags')
        addins.append(utilities.serializeObject(addinqla.AddinQla))
        addins.append(utilities.serializeObject(addinexcel.AddinExcel))
        addins.append(utilities.serializeObject(addincalc.AddinCalc))
        addins.append(utilities.serializeObject(addinc.AddinC))
        addins.append(utilities.serializeObject(addinguile.AddinGuile))
        addins.append(utilities.serializeObject(addindoxygen.AddinDoxygen))
        addins.append(utilities.serializeObject(valueobjects.ValueObjects))
    else:
        usage()

if not len(addins):
    usage()

# generate source code for chosen target projects

utilities.generate(addins)

