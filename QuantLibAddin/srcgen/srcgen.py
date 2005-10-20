
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
import common
import utils
import parse
import config
import addin
import addinExcel
import addinCalc
import addinGuile
import addinC
import addinQla
import addinDoxygen
import category

def usage():
    print 'usage: ' + sys.argv[0] + ''' -[targets]
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

    sys.exit(2)

# parse command line arguments

try:
    opts, args = getopt.getopt(sys.argv[1:], 'qesocgdah', 'help' )
except getopt.GetoptError:
    usage()

conf = config.Config()
categories = {}
categories[common.DICT] = {}
categories[common.KEYS] = []
for categoryName in conf.config[common.CATEGORIES]:
    cat = category.Category(categoryName)
    categories[common.DICT][categoryName] = cat
    categories[common.KEYS].append(categoryName)
categories[common.KEYS].sort()

enumerations = parse.parseFile(common.ENUMS)

addins = []

for o, a in opts:
    if o in ('-h', '--help'):
        usage()
    if o == '-q':
        addins.append(addinQla.AddinQla(enumerations))
    if o == '-e':
        addins.append(addinExcel.AddinExcelDynamic(categories))
    if o == '-s':
        addins.append(addinExcel.AddinExcelStatic(categories))
    if o == '-o':
        addins.append(addinCalc.AddinCalc(categories))
    if o == '-c':
        addins.append(addinC.AddinC(categories))
    if o == '-g':
        addins.append(addinGuile.AddinGuile(categories))
    if o == '-d':
        addins.append(addinDoxygen.AddinDoxygen(categories, enumerations))
    if o == '-a':
        addins.append(addinQla.AddinQla(enumerations))
        addins.append(addinExcel.AddinExcelStatic(categories))
        addins.append(addinExcel.AddinExcelDynamic(categories))
        addins.append(addinCalc.AddinCalc(categories))
        addins.append(addinC.AddinC(categories))
        addins.append(addinGuile.AddinGuile(categories))
        addins.append(addinDoxygen.AddinDoxygen(categories, enumerations))

if not len(addins):
    usage()

# generate source code for chosen target projects

utils.logMessage('begin ...')
utils.init()

for addin in addins:
    addin.generate()

utils.logMessage('end')

