
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
import qla
import excel
import calc
import c
import guile
import doxygen

# global constants

PATTERN_FUNCTIONS = r'.*.xml\Z'
PATTERN_ENUMS     = r'enums.xml\Z'

def usage():
    print 'usage: ' + sys.argv[0] + ' -[targets]'
    print '    where [targets] is any of:'
    print '        q - generate source for QuantLibAddin'
    print '        e - generate source for Excel addin'
    print '        o - generate source for OpenOffice.org Calc addin'
    print '        c - generate source for C addin'
    print '        g - generate source for Guile addin'
    print '        d - generate doxygen documentation files'
    print '    or'
    print '        a - all of the above'
    sys.exit(2)

# parse command line arguments

try:
    opts, args = getopt.getopt(sys.argv[1:], 'qeocgdah', 'help' )
except getopt.GetoptError:
    usage()

buildQla   = False
buildExcel = False
buildCalc  = False
buildC     = False
buildDocs  = False
buildGuile = False

for o, a in opts:
    if o in ('-h', '--help'):
        usage()
    if o == '-q':
        buildQla   = True
    if o == '-e':
        buildExcel = True
    if o == '-o':
        buildCalc  = True
    if o == '-c':
        buildC     = True
    if o == '-g':
        buildGuile = True
    if o == '-d':
        buildDocs  = True
    if o == '-a':
        buildQla   = True
        buildExcel = True
        buildCalc  = True
        buildC     = True
        buildGuile = True
        buildDocs  = True

if not buildQla and not buildExcel and not buildCalc \
    and not buildC and not buildGuile and not buildDocs:
    usage()

# generate source code for chosen target projects

utils.logMessage('begin ...')
utils.init()
functionDefs = parse.parseFiles(PATTERN_FUNCTIONS, PATTERN_ENUMS)
enumDefs     = parse.parseFiles(PATTERN_ENUMS)

if buildQla:
    qla.generate(enumDefs)
if buildExcel:
    excel.generate(functionDefs)
if buildCalc:
    calc.generate(functionDefs)
if buildC:
    c.generate(functionDefs)
if buildGuile:
    guile.generate(functionDefs)
if buildDocs:
    doxygen.generate(functionDefs, enumDefs)

utils.logMessage('end')

