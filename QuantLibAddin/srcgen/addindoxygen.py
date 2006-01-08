
"""
 Copyright (C) 2005, 2006 Eric Ehlers
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

"""generate doxygen documentation files."""

import addin
import function
import config
import outputfile
import common
import log

# constants

LINE_ENUM = '    <tr><td>%s</td><td>%s</td></tr>\n'
LINE_REF = '        \\ref enum_%d\\n\n'
LINE_SECTION = '    \\section enum_%d %s\n'
LINE_TABLE = """    <table>\n
    <tr><td><b>String</b></td><td><b>Enumeration</b></td></tr>\n"""

class AddinDoxygen(addin.Addin):
    """Generate doxygen documentation files."""

    def generate(self):
        """Generate doxygen documentation files."""
        log.Log.getInstance().logMessage('  begin generating Doxygen ...')
        self.generateDocs()
        self.generateEnums()
        self.generateCategoryDoc()
        log.Log.getInstance().logMessage('  done generating Doxygen.')

    def generateEnums(self):
        """Generate documentation for enumerations."""
        fileDoc = outputfile.OutputFile(self.rootDirectory + 'enums.docs')
        fileDoc.write(self.bufferEnumerations.text)
        for i in xrange(len(config.Config.getInstance().Enumerations)):
            fileDoc.write(LINE_REF % i)
        fileDoc.write('\n')
        i = 0
        for enumeration in config.Config.getInstance().getEnumerations():
            fileDoc.write(LINE_SECTION % (i, enumeration.type))
            i += 1
            fileDoc.write(LINE_TABLE)
            for enumDef in enumeration.getEnumerationDefinitions():
                fileDoc.write(LINE_ENUM % (enumDef.string, enumDef.value))
            fileDoc.write('    </table>\n\n')
        fileDoc.write('*/\n\n')
        fileDoc.close()

    def generateFunctionDoc(self, fileFunc, func):
        """Generate documentation for given function."""
        functionDoc = func.generateParameterList(self.functionDocs, 'string handle')
        retCode = self.functionReturnCode.apply(func.returnValue)
        fileFunc.write('\\anchor %s \\b %s\n' % (func.name, func.name))
        fileFunc.write('\\code\n')
        fileFunc.write(retCode + '\n')
        fileFunc.write('%s(%s)\n' % (func.name, functionDoc))
        fileFunc.write('\\endcode\n')
        fileFunc.write('\\par Description:\n')
        fileFunc.write(func.description)
        fileFunc.write('\n')
        for param in func.Parameters:
            fileFunc.write('\\param %s %s\n' % (param.name, param.description))
        fileFunc.write('\\return %s\n\n' % func.returnValue.description)

    def generateCategoryDoc(self):
        """Generate page listing function categories."""
        fileDoc = outputfile.OutputFile(self.rootDirectory + 'categories.docs')
        fileDoc.write(self.bufferCategories.text)
        # ensure list of links is sorted alphabetically by display name
        dispNmToCatNm = {}
        displayNames = []
        for category in config.Config.getInstance().getCategories('*'):
            dispNmToCatNm[category.displayName] = category.name
            displayNames.append(category.displayName)
        displayNames.sort()
        for displayKey in displayNames:
            fileDoc.write('    \\ref %s\\n\n' % dispNmToCatNm[displayKey])
        fileDoc.write('\n*/\n\n')
        fileDoc.close()

    def generateFunctionList(self, allFuncs):
        """Generate alphabetical list of links to all functions."""
        fileDoc = outputfile.OutputFile(self.rootDirectory + 'all.docs')
        fileDoc.write(self.bufferHeader.text)
        allFuncs.sort()
        for func in allFuncs:
            fileDoc.write('\\ref %s ()\\n\n' % func)
        fileDoc.write('\n*/\n\n')
        fileDoc.close()

    def generateDocs(self):
        """Generate doxygen documentation files."""
        allFuncs = []
        for category in config.Config.getInstance().getCategories('*'):
            fileDoc = outputfile.OutputFile(self.rootDirectory + category.name + '.docs')
            fileDoc.write('/*! \page %s %s\n' % (category.name, category.displayName))
            fileDoc.write('\\section overview Overview\n')
            fileDoc.write('%s\n' % category.description)
            fileDoc.write('\\section functionlist Function List\n')
            for func in category.getFunctions('*'): 
                fileDoc.write('\\ref %s ()\\n\n' % func.name)
                allFuncs.append(func.name)
            fileDoc.write('\\section documentation Function Documentation\n')
            for func in category.getFunctions('*'): 
                self.generateFunctionDoc(fileDoc, func)
            fileDoc.write('*/\n\n')
            fileDoc.close()
        self.generateFunctionList(allFuncs)

