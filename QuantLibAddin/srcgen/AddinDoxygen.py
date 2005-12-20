
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

import Addin
import Config
import OutputFile
import common
import utils

# constants

LINE_ENUM = '    <tr><td>%s</td><td>%s</td></tr>\n'
LINE_REF = '        \\ref enum_%d\\n\n'
LINE_SECTION = '    \\section enum_%d %s\n'
LINE_TABLE = """    <table>\n
    <tr><td><b>String</b></td><td><b>Enumeration</b></td></tr>\n"""

class AddinDoxygen(Addin.Addin):
    'generate doxygen documentation files'

    def generate(self):
        'generate doxygen documentation files'
        utils.logMessage('  begin generating Doxygen ...')
        self.generateDocs()
        self.generateEnums()
        self.generateCategoryDoc()
        utils.logMessage('  done generating Doxygen.')

    def generateEnums(self):
        'generate documentation for enumerations'
        fileDoc = OutputFile.OutputFile(self.rootDirectory + 'enums.docs')
        fileDoc.write(self.bufferEnumerations.text)
        for i in xrange(len(Config.Config.getInstance().Enumerations)):
            fileDoc.write(LINE_REF % i)
        fileDoc.write('\n')
        i = 0
        for enumeration in Config.Config.getInstance().getEnumerations():
            fileDoc.write(LINE_SECTION % (i, enumeration.type))
            i += 1
            fileDoc.write(LINE_TABLE)
            for enumDef in enumeration.getEnumerationDefinitions():
                fileDoc.write(LINE_ENUM % (enumDef.string, enumDef.value))
            fileDoc.write('    </table>\n\n')
        fileDoc.write('*/\n\n')
        fileDoc.close()

    def generateFunctionDoc(self, fileFunc, function):
        'generate documentation for given function'
        functionDoc = self.generateCode(self.functionDocs, function.Parameters)
        retCode = self.functionReturnCode.apply(function.returnValue)
        fileFunc.write('\\anchor %s \\b %s\n' % (function.name, function.name))
        fileFunc.write('\\code\n')
        fileFunc.write(retCode + '\n')
        fileFunc.write('%s(%s)\n' % (function.name, functionDoc))
        fileFunc.write('\\endcode\n')
        fileFunc.write('\\par Description:\n')
        fileFunc.write(function.description)
        fileFunc.write('\n')
        for param in function.Parameters:
            fileFunc.write('\\param %s %s\n' % (param.name, param.description))
        fileFunc.write('\\return %s\n\n' % function.returnValue.description)

    def generateCategoryDoc(self):
        'generate page summarizing functions'
        fileDoc = OutputFile.OutputFile(self.rootDirectory + 'categories.docs')
        fileDoc.write(self.bufferCategories.text)
        # ensure list of links is sorted alphabetically by display name
        dispNmToCatNm = {}
        displayNames = []
        for category in Config.Config.getInstance().getCategories('*'):
            dispNmToCatNm[category.displayName] = category.name
            displayNames.append(category.displayName)
        displayNames.sort()
        for displayKey in displayNames:
            fileDoc.write('    \\ref %s\\n\n' % dispNmToCatNm[displayKey])
        fileDoc.write('\n*/\n\n')
        fileDoc.close()

    def generateFunctionList(self, allFuncs):
        'generate alphabetical list of links to all functions'
        fileDoc = OutputFile.OutputFile(self.rootDirectory + 'all.docs')
        fileDoc.write(self.bufferHeader.text)
        allFuncs.sort()
        for func in allFuncs:
            fileDoc.write('\\ref %s ()\\n\n' % func)
        fileDoc.write('\n*/\n\n')
        fileDoc.close()

    def generateDocs(self):
        'generate doxygen documentation files'
        allFuncs = []
        for category in Config.Config.getInstance().getCategories('*'):
            fileDoc = OutputFile.OutputFile(self.rootDirectory + category.name + '.docs')
            fileDoc.write('/*! \page %s %s\n' % (category.name, category.displayName))
            fileDoc.write('\\section overview Overview\n')
            fileDoc.write('%s\n' % category.description)
            fileDoc.write('\\section functionlist Function List\n')
            for function in category.getFunctions('*'): 
                fileDoc.write('\\ref %s ()\\n\n' % function.name)
                allFuncs.append(function.name)
            fileDoc.write('\\section documentation Function Documentation\n')
            for function in category.getFunctions('*'): 
                self.generateFunctionDoc(fileDoc, function)
            fileDoc.write('*/\n\n')
            fileDoc.close()
        self.generateFunctionList(allFuncs)

