
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
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

"""generate source code for enumerations."""

from gensrc.Addins import addin
from gensrc.Utilities import outputfile
from gensrc.Utilities import common
from gensrc.Utilities import log
from gensrc.Categories import category
from gensrc.Configuration import environment

class Enumerations(addin.Addin):
    """generate source code for enumerations."""

    ENUM_END   =      '        );\n\n'
    ENUM_CURVE_LINE = '            MAP(KeyPair("%(key1)s", "%(key2)s"), %(value)s);\n'
    ENUM_LINE  =      '            MAP("%(string)s", %(value)s);\n'
    ENUM_START =      '        REG_ENUM(%s,\n'
    ENUM_UNREG =      '        UNREG_ENUM(%s)\n'

    def generate(self, categoryList, enumerationList):
        """generate source code for enumerations."""

        self.categoryList_ = categoryList
        self.enumerationList_ = enumerationList

        log.Log.instance().logMessage(' begin generating enumerations ...')
        self.generateEnumTypes()
        self.generateEnumClasses()
        log.Log.instance().logMessage(' done generating enumerations.')

    def generateEnumeration(self, enumeration, buffer):
        """generate source code for given enumeration."""
        ret = Enumerations.ENUM_START % enumeration.type
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
        ret += Enumerations.ENUM_END
        return ret

    def generateEnumTypes(self):
        """generate source file for enumerated types."""
        buf1 = ''   # code to register the enumeration
        buf2 = ''   # code to unregister the enumeration
        for enumeration in self.enumerationList_.enumeratedTypes():
            buf1 += self.generateEnumeration(enumeration, Enumerations.ENUM_LINE)
            buf2 += Enumerations.ENUM_UNREG % enumeration.type
        buf = self.bufferEnumTypes.text % (buf1, buf2)
        fileName = environment.config().libFullPath + 'enumtyperegistry.cpp'
        outputfile.OutputFile(self, fileName, 
            self.enumerationList_.enumTypeCopyright, buf)

    def generateEnumClasses(self):
        """generate source file for enumerated classes."""
        curveBuffer = ''
        for enumeration in self.enumerationList_.enumeratedCurves():
            curveBuffer += self.generateEnumeration(enumeration, Enumerations.ENUM_CURVE_LINE)
        classBuffer = ''
        for enumeration in self.enumerationList_.enumeratedClasses():
            classBuffer += self.generateEnumeration(enumeration, Enumerations.ENUM_LINE)
        buf = self.bufferEnumClasses.text % (curveBuffer, classBuffer)
        fileName = environment.config().libFullPath + 'enumclassregistry.cpp'
        fileEnum = outputfile.OutputFile(self, fileName,
            self.enumerationList_.enumClassCopyright, buf)

