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

'output doxygen documentation files'

import common
import utils
import params

# constants

ROOT = '../Docs/pages/'
ALL_HEADER = \
'/*! \page all All\n\
\section overview Overview\n\
Below is an alphabetical list of links to documentation for\n\
all functions in QuantLibAddin.\n\
\section functions Function List\n\
\n'

# global variables

# parameter list objects
plDoc    = ''    # function prototypes

def deriveRetCode(retVal):
    'format function return value'
    if retVal[common.TENSOR] == common.SCALAR:
        return retVal[common.TYPE]
    elif retVal[common.TENSOR] == common.VECTOR:
        return 'vector < %s > ' % retVal[common.TYPE]
    elif retVal[common.TENSOR] == common.MATRIX:
        return 'vector < vector < %s > > ' % retVal[common.TYPE]

def generateFuncDoc(fileFunc, function):
    'generate documentation for given function'
    global plDoc
    paramList = plDoc.generateCode(function[common.PARAMS])
    retCode = deriveRetCode(function[common.RETVAL])
    fileFunc.write('\\anchor %s \\b %s\n' % (function[common.NAME], function[common.NAME]))
    fileFunc.write('\\code\n')
    fileFunc.write(retCode + '\n')
    fileFunc.write('%s(' % function[common.NAME])
    if paramList != '':
        fileFunc.write('\n')
        fileFunc.write(paramList)
    fileFunc.write(')\n')
    fileFunc.write('\\endcode\n')
    fileFunc.write('\\par Description:\n')
    fileFunc.write(function[common.DESC])
    for param in function[common.PARAMS]:
        fileFunc.write('\\param %s %s\n' % (param[common.NAME], param[common.DESC]))
    fileFunc.write('\\return %s\n\n' % function[common.RETVAL][common.DESC])

def generateAllDoc(allFuncs):
    'generate alphabetical list of links to all functions'
    fileName = ROOT + 'all.docs' + common.TEMPFILE
    fileDoc = file(fileName, 'w')
    utils.printHeader(fileDoc)
    fileDoc.write(ALL_HEADER)
    allFuncs.sort()
    for func in allFuncs:
        fileDoc.write('\\ref %s ()\\n\n' % func)
    fileDoc.write('*/\n')
    fileDoc.close()
    utils.updateIfChanged(fileName)

def generateDocs(functionGroups):
    'generate doxygen documentation files'
    global plDoc
    allFuncs = []
    plDoc = params.ParameterDeclare(2,
        formatVector = 'vector < %s >',
        formatMatrix = 'vector < vector < %s > >')
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        fileName = ROOT + groupName + '.docs' + common.TEMPFILE
        fileDoc = file(fileName, 'w')
        utils.printHeader(fileDoc)
        fileDoc.write('/*! \page %s %s\n' % (groupName, functionGroup[common.DISPLAYNAME]))
        fileDoc.write('\\section overview Overview\n')
        fileDoc.write('%s\n' % functionGroup[common.DESC])
        fileDoc.write('\\section functions Function List\n')
        for function in functionGroup[common.FUNCS]:
            fileDoc.write('\\ref %s ()\\n\n' % function[common.NAME])
            allFuncs.append(function[common.NAME])
        fileDoc.write('\\section documentation Function Documentation\n')
        for function in functionGroup[common.FUNCS]:
            generateFuncDoc(fileDoc, function)
        fileDoc.write('*/\n')
        fileDoc.close()
        utils.updateIfChanged(fileName)
    generateAllDoc(allFuncs)

def generate(functionGroups):
    'generate doxygen documentation files'
    utils.logMessage('  begin generating Doxygen ...')
    generateDocs(functionGroups)
    utils.logMessage('  done generating Doxygen.')

