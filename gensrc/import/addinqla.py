
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

class AddinQla(addin.Addin):
    """generate source code for QuantLibAddin."""

    ENUM_END   =      '        );\n\n'
    ENUM_CURVE_LINE = '            MAP(KeyPair("%(key1)s", "%(key2)s"), %(value)s);\n'
    ENUM_LINE  =      '            MAP("%(string)s", %(value)s);\n'
    ENUM_START =      '        REG_ENUM(%s,\n'
    ENUM_UNREG =      '        UNREG_ENUM(%s)\n'

    def generate(self):
        """generate source code for QuantLibAddin."""
        log.Log.getInstance().logMessage(' begin generating QuantLibAddin ...')
        self.generateEnumTypes()
        self.generateEnumClasses()
        self.generateMainHeader()
        log.Log.getInstance().logMessage(' done generating QuantLibAddin.')

    def generateEnumeration(self, enumeration, buffer):
        """generate source code for given enumeration."""
        ret = AddinQla.ENUM_START % enumeration.type
        for enumDef in enumeration.getEnumerationDefinitions():
            if enumeration.constructor:
                enumVal = enumeration.type + '(' + enumDef.value + ')'
            else:
                enumVal = enumDef.value
            ret += buffer % {
                    'string' : enumDef.string,
                    'key1' : enumDef.key1,
                    'key2' : enumDef.key2,
                    'value' : enumVal}
        ret += AddinQla.ENUM_END
        return ret

    def generateEnumTypes(self):
        """generate source file for enumerated types."""
        buf1 = ''   # code to register the enumeration
        buf2 = ''   # code to unregister the enumeration
        for enumeration in config.Config.getInstance().getEnumTypes():
            buf1 += self.generateEnumeration(enumeration, AddinQla.ENUM_LINE)
            buf2 += AddinQla.ENUM_UNREG % enumeration.type
        buf = self.bufferEnumTypes.text % (buf1, buf2)
        fileName = config.Config.getInstance().libFullPath + 'enumtyperegistry.cpp'
        outputfile.OutputFile(self, fileName, 
            config.Config.getInstance().enumTypeCopyright, buf)

    def generateEnumClasses(self):
        """generate source file for enumerated classes."""
        curveBuffer = ''
        for enumeration in config.Config.getInstance().getEnumCurves():
            curveBuffer += self.generateEnumeration(enumeration, AddinQla.ENUM_CURVE_LINE)
        classBuffer = ''
        for enumeration in config.Config.getInstance().getEnumClasses():
            classBuffer += self.generateEnumeration(enumeration, AddinQla.ENUM_LINE)
        buf = self.bufferEnumClasses.text % (curveBuffer, classBuffer)
        fileName = config.Config.getInstance().libFullPath + 'enumclassregistry.cpp'
        fileEnum = outputfile.OutputFile(self, fileName,
            config.Config.getInstance().enumClassCopyright, buf)

    def generateMainHeader(self):
        """generate the main header for QuantLibAddin."""
        headerList = ''
        for category in config.Config.getInstance().getCategories('*'):
            headerList += category.includeList() + '\n'
        buf = self.bufferMainHeader.text % headerList
        fileName = config.Config.getInstance().libFullPath + 'qladdin.hpp'
        fileHeader = outputfile.OutputFile(self, fileName, self.copyright, buf)

