
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
from gensrc.Configuration import environment

class Enumerations(addin.Addin):
    """generate source code for enumerations."""

    ENUM_END   =      '        );\n\n'
    ENUM_LINE  =      '            MAP("%(string)s", %(value)s);\n'
    ENUM_CURVE_LINE = '            MAP(KeyPair("%(traits)s", "%(interpolator)s"), %(value)s);\n'
    ENUM_START =      '        REG_ENUM(%s,\n'
    ENUM_UNREG =      '        UNREG_ENUM(%s)\n'

    def generate(self, categoryList, enumerationList):
        """generate source code for enumerations."""

        self.enumerationList_ = enumerationList

        log.Log.instance().logMessage(' begin generating enumerations ...')
        self.generateEnumeratedTypes()
        self.generateEnumeratedClasses()
        log.Log.instance().logMessage(' done generating enumerations.')

    def generateEnumeratedType(self, enumeratedTypeGroup):
        """generate source code for enumerated type group."""
        ret = Enumerations.ENUM_START % enumeratedTypeGroup.type()
        for enumeratedType in enumeratedTypeGroup.enumeratedTypes():
            ret += Enumerations.ENUM_LINE % {
                'string' : enumeratedType.string(),
                'value' : enumeratedType.constructor() }
        ret += Enumerations.ENUM_END
        return ret

    def generateEnumeratedClass(self, enumeratedClassGroup):
        """generate source code for enumerated class group."""
        ret = Enumerations.ENUM_START % enumeratedClassGroup.className()
        for enumeratedClass in enumeratedClassGroup.enumeratedClasses():
            ret += Enumerations.ENUM_LINE % {
                'string' : enumeratedClass.string(),
                'value' : enumeratedClass.value() }
        ret += Enumerations.ENUM_END
        return ret

    def generateEnumeratedCurve(self, enumeratedCurveGroup):
        """generate source code for enumerated curve group."""
        ret = Enumerations.ENUM_START % enumeratedCurveGroup.className()
        for enumeratedCurve in enumeratedCurveGroup.enumeratedCurves():
            ret += Enumerations.ENUM_CURVE_LINE % {
                'traits' : enumeratedCurve.traits(),
                'interpolator' : enumeratedCurve.interpolator(),
                'value' : enumeratedCurve.value() }
        ret += Enumerations.ENUM_END
        return ret

    def generateEnumeratedTypes(self):
        """generate source file for enumerated types."""
        buf1 = ''   # code to register the enumeration
        buf2 = ''   # code to unregister the enumeration
        for enumeratedTypeGroup in self.enumerationList_.enumeratedTypeGroups():
            buf1 += self.generateEnumeratedType(enumeratedTypeGroup)
            buf2 += Enumerations.ENUM_UNREG % enumeratedTypeGroup.type()
        buf = self.bufferEnumTypes_.text() % (buf1, buf2)
        fileName = environment.config().libFullPath() + 'enumtyperegistry.cpp'
        outputfile.OutputFile(self, fileName, 
            self.enumerationList_.enumeratedTypeCopyright(), buf)

    def generateEnumeratedClasses(self):
        """generate source file for enumerated classes."""
        curveBuffer = ''
        for enumeratedCurveGroup in self.enumerationList_.enumeratedCurveGroups():
            curveBuffer += self.generateEnumeratedCurve(enumeratedCurveGroup)
        classBuffer = ''
        for enumeratedClassGroup in self.enumerationList_.enumeratedClassGroups():
            classBuffer += self.generateEnumeratedClass(enumeratedClassGroup)
        buf = self.bufferEnumClasses_.text() % (curveBuffer, classBuffer)
        fileName = environment.config().libFullPath() + 'enumclassregistry.cpp'
        fileEnum = outputfile.OutputFile(self, fileName,
            self.enumerationList_.enumeratedClassCopyright(), buf)

