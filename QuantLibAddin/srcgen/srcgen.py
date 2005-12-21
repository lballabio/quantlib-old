
"""
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
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
import Log
import AddinQla
import AddinExcel
import AddinCalc
import AddinC
import AddinGuile
import AddinDoxygen
import Factory

def usage():
    errorMessage = 'usage: ' + sys.argv[0] + ''' -[targets]
    where [targets] is any of:
        q - generate source for QuantLibAddin
        e - generate source for Excel addin (dynamic)
        s - generate source for Excel addin (static)
        o - generate source for OpenOffice.org Calc addin
        c - generate source for C addin
        g - generate source for Guile addin
        d - generate doxygen documentation files
    or
        a - all of the above'''
    sys.exit(errorMessage)

# parse command line arguments

try:
    opts, args = getopt.getopt(sys.argv[1:], 'qeocgdah', 'help' )
except getopt.GetoptError:
    usage()

addins = []

for o, a in opts:
    if o in ('-h', '--help'):
        usage()
    elif o == '-q':
        addins.append(Factory.Factory.getInstance().serializeObject(AddinQla.AddinQla))
    elif o == '-e':
        addins.append(Factory.Factory.getInstance().serializeObject(AddinExcel.AddinExcel))
    elif o == '-o':
        addins.append(Factory.Factory.getInstance().serializeObject(AddinCalc.AddinCalc))
    elif o == '-c':
        addins.append(Factory.Factory.getInstance().serializeObject(AddinC.AddinC))
    elif o == '-g':
        addins.append(Factory.Factory.getInstance().serializeObject(AddinGuile.AddinGuile))
    elif o == '-d':
        addins.append(Factory.Factory.getInstance().serializeObject(AddinDoxygen.AddinDoxygen))
    elif o == '-a':
        if len(opts) != 1:
            sys.exit('flag -a cannot be combined with other flags')
        addins.append(Factory.Factory.getInstance().serializeObject(AddinQla.AddinQla))
        addins.append(Factory.Factory.getInstance().serializeObject(AddinExcel.AddinExcel))
        addins.append(Factory.Factory.getInstance().serializeObject(AddinCalc.AddinCalc))
        addins.append(Factory.Factory.getInstance().serializeObject(AddinC.AddinC))
        addins.append(Factory.Factory.getInstance().serializeObject(AddinGuile.AddinGuile))
        addins.append(Factory.Factory.getInstance().serializeObject(AddinDoxygen.AddinDoxygen))
    else:
        usage()

if not len(addins):
    usage()

# generate source code for chosen target projects

Log.Log.getInstance().logMessage('begin ...')

for addin in addins:
    addin.generate()

Log.Log.getInstance().logMessage('end')

