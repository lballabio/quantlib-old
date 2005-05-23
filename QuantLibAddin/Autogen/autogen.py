#!/usr/bin/python

import sys
import getopt
import common
import utils
import parse
import c
import calc
import excel
import guile
import doxygen

def usage():
    print 'usage: ' + sys.argv[0] + ' -[targets]'
    print '    where [targets] is any of:'
    print '        e - generate source for Excel addin'
    print '        o - generate source for OpenOffice.org Calc addin'
    print '        c - generate source for C addin'
    print '        d - generate doxygen documentation files'
    print '    or'
    print '        a - all of the above'
    sys.exit(2)

# parse command line arguments

try:
    opts, args = getopt.getopt(sys.argv[1:], 'eocdah', 'help' )
except getopt.GetoptError:
    usage()

buildExcel = False
buildCalc  = False
buildC     = False
buildDocs  = False
buildGuile = False

for o, a in opts:
    if o in ('-h', '--help'):
        usage()
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
        buildExcel = True
        buildCalc  = True
        buildC     = True
        buildGuile = True
        buildDocs  = True

if not buildExcel and not buildCalc and not buildC \
   and not buildGuile and not buildDocs:
    usage()

# generate source code for chosen target projects

utils.logMessage('begin ...')
utils.init()
functionDefs = parse.getFunctionDefs()

if buildC:
    c.generate(functionDefs)
if buildCalc:
    calc.generate(functionDefs)
if buildExcel:
    excel.generate(functionDefs)
if buildGuile:
    guile.generate(functionDefs)
if buildDocs:
    doxygen.generate(functionDefs)

utils.logMessage('end')

