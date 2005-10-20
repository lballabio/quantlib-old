
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

import addin
import common
import utils
import category
import rule

# constants

ALL_HEADER = """
'/*! \page all All\n
\section overview Overview\n
Below is an alphabetical list of links to documentation for
all functions in QuantLibAddin.\n
\section functions Function List\n
\n"""
LINE_ENUM    = '    <tr><td>%s</td><td>%s</td></tr>\n'
LINE_REF     = '        \\ref enum_%d\\n\n'
LINE_SECTION = '    \\section enum_%d %s\n'
LINE_TABLE   = """    <table>\n
    <tr><td><b>String</b></td><td><b>Enumeration</b></td></tr>\n"""
STUB_ENUMS   = 'stub.doxygen.enums'
STUB_FUNCS   = 'stub.doxygen.functions'
FUNC_DOCS = 'functionDocs'
FUNC_RETCODE = 'functionReturnCode'

class AddinDoxygen(addin.Addin):

    def __init__(self,
            categories,
            enumerations):
        super(AddinDoxygen, self).__init__('doxygen', categories)
        self.enumerations = enumerations
        self.bufEnums = utils.loadBuffer(STUB_ENUMS)
        self.bufFuncs = utils.loadBuffer(STUB_FUNCS)
        self.rootDir = '../Docs/pages/'

    def setRules(self, config):
        self.ruleFunctionDocs = rule.Rule(config[FUNC_DOCS])
        self.ruleFunctionReturnCode = rule.Rule(config[FUNC_RETCODE])

    def generate(self):
        'generate doxygen documentation files'
        utils.logMessage('  begin generating Doxygen ...')
        self.generateDocs()
        self.generateEnums()
        self.generateOverviewDoc()
        utils.logMessage('  done generating Doxygen.')

    def generateEnums(self):
        'generate documentation for enumerations'
        enumList = self.enumerations[common.ENUMDEFS]
        fileName = self.rootDir + 'enums.docs' + common.TEMPFILE
        fileDoc = file(fileName, 'w')
        utils.printHeader(fileDoc)
        fileDoc.write(self.bufEnums)
        for i in xrange(len(enumList)):
            fileDoc.write(LINE_REF % i)
        fileDoc.write('\n')
        i = 0
        for enumDef in enumList:
            fileDoc.write(LINE_SECTION % (i, enumDef[common.CLASS]))
            i += 1
            fileDoc.write(LINE_TABLE)
            for enum in enumDef[common.DEFS]:
                fileDoc.write(LINE_ENUM % (enum[common.STRING], enum[common.ENUM]))
            fileDoc.write('    </table>\n\n')
        fileDoc.write('*/\n\n')
        fileDoc.close()
        utils.updateIfChanged(fileName)

    def generateFuncDoc(self, fileFunc, function):
        'generate documentation for given function'
        functionDoc = self.generateCode(self.ruleFunctionDocs, function.parameters)
        retCode = self.ruleFunctionReturnCode.apply(function.returnValue)
        fileFunc.write('\\anchor %s \\b %s\n' % (function.name, function.name))
        fileFunc.write('\\code\n')
        fileFunc.write(retCode + '\n')
        fileFunc.write('%s(%s)\n' % (function.name, functionDoc))
        fileFunc.write('\\endcode\n')
        fileFunc.write('\\par Description:\n')
        fileFunc.write(function.description)
        for param in function.parameters:
            fileFunc.write('\\param %s %s\n' % (param.name, param.description))
        fileFunc.write('\\return %s\n\n' % function.returnValue.description)

    def generateOverviewDoc(self):
        'generate page summarizing functions'
        fileName = self.rootDir + 'functionsoverview.docs' + common.TEMPFILE
        fileDoc = file(fileName, 'w')
        utils.printHeader(fileDoc)
        fileDoc.write(self.bufFuncs)
        # ensure list of links is sorted alphabetically by display name
        dispNmToCatNm = {}
        displayNames = []
        for categoryKey in self.categories[common.KEYS]:
            category = self.categories[common.DICT][categoryKey]
            dispNmToCatNm[category.displayName] = category.name
            displayNames.append(category.displayName)
        displayNames.sort()
        for displayKey in displayNames:
            fileDoc.write('    \\ref %s\\n\n' % dispNmToCatNm[displayKey])
        fileDoc.write('\n*/\n\n')
        fileDoc.close()
        utils.updateIfChanged(fileName)

    def generateAllDoc(self, allFuncs):
        'generate alphabetical list of links to all functions'
        fileName = self.rootDir + 'all.docs' + common.TEMPFILE
        fileDoc = file(fileName, 'w')
        utils.printHeader(fileDoc)
        fileDoc.write(ALL_HEADER)
        allFuncs.sort()
        for func in allFuncs:
            fileDoc.write('\\ref %s ()\\n\n' % func)
        fileDoc.write('*/\n\n')
        fileDoc.close()
        utils.updateIfChanged(fileName)

    def generateDocs(self):
        'generate doxygen documentation files'
        allFuncs = []
        for categoryKey in self.categories[common.KEYS]:
            category = self.categories[common.DICT][categoryKey]
            fileName = self.rootDir + category.name + '.docs' + common.TEMPFILE
            fileDoc = file(fileName, 'w')
            utils.printHeader(fileDoc)
            fileDoc.write('/*! \page %s %s\n' % (category.name, category.displayName))
            fileDoc.write('\\section overview Overview\n')
            fileDoc.write('%s\n' % category.description)
            fileDoc.write('\\section functions Function List\n')
            for functionKey in category.functions[common.KEYS]:
                function = category.functions[common.DICT][functionKey]
                fileDoc.write('\\ref %s ()\\n\n' % function.name)
                allFuncs.append(function.name)
            fileDoc.write('\\section documentation Function Documentation\n')
            for functionKey in category.functions[common.KEYS]:
                function = category.functions[common.DICT][functionKey]
                self.generateFuncDoc(fileDoc, function)
            fileDoc.write('*/\n\n')
            fileDoc.close()
            utils.updateIfChanged(fileName)
        self.generateAllDoc(allFuncs)

