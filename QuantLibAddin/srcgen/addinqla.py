
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

"""generate source code for QuantLibAddin."""

import config
import addin
import outputfile
import common
import log

# constants

ENUM_END   = '        );\n\n'
ENUM_LINE  = '            MAP("%s", %s);\n'
ENUM_START = '        REG_ENUM(%s,\n'

class AddinQla(addin.Addin):
    """generate source code for QuantLibAddin."""

    def generate(self):
        """generate source code for QuantLibAddin."""
        log.Log.getInstance().logMessage('  begin generating QuantLibAddin ...')
        self.generateEnumerations()
        self.generateMainHeader()
        log.Log.getInstance().logMessage('  done generating QuantLibAddin.')

    def generateEnumeration(self, fileEnum, enumeration):
        """generate source code for given enumeration."""
        fileEnum.write(ENUM_START % enumeration.type)
        for enumDef in enumeration.getEnumerationDefinitions():
            if enumeration.constructor:
                enumVal = enumeration.type + '(' + enumDef.value + ')'
            else:
                enumVal = enumDef.value
            fileEnum.write(ENUM_LINE % (enumDef.string.upper(), enumVal))
        fileEnum.write(ENUM_END)

    def generateEnumerations(self):
        """generate source file for enumerations."""
        fileEnum = outputfile.OutputFile(self, self.rootDirectory + 'enumregistry.cpp')
        fileEnum.write(self.bufferIncludes.text)
        for enumeration in config.Config.getInstance().getEnumerations():
            if not enumeration.documentationOnly:
                self.generateEnumeration(fileEnum, enumeration)
        fileEnum.write('    }\n\n}\n\n')
        fileEnum.close()

    def generateMainHeader(self):
        """generate the main header for QuantLibAddin."""
        headerList = ''
        for category in config.Config.getInstance().getCategories('*'):
            headerList += category.includeList() + '\n'
        fileHeader = outputfile.OutputFile(self, self.rootDirectory + 'qladdin.hpp')
        fileHeader.write(self.bufferMainHeader.text % headerList)
        fileHeader.close()

