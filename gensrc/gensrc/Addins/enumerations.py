
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

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

    #############################################
    # class variables
    #############################################

    ENUM_REGISTER = '''\
        {
            ObjectHandler::Create<%(type)s> create;
%(buffer)s        }\n\n'''
    ENUM_UNREGISTER = '''\
        ObjectHandler::Create<%(type)s>().unregisterTypes();\n'''
    ENUM_LINE = '''\
            create.registerType("%(string)s", new %(constructor)s);\n'''
    ENUM_REGISTER2 = '''\
        {
            ObjectHandler::Create<boost::shared_ptr<%(type)s> > create;
%(buffer)s        }\n\n'''
    ENUM_LINE2 = '''\
            create.registerType("%(string)s", reinterpret_cast<void*>(%(constructor)s));\n'''
    ENUM_LINE3 = '''\
            create.registerType(ObjectHandler::KeyPair("%(id1)s", "%(id2)s"), reinterpret_cast<void*>(%(value)s));\n'''

    #############################################
    # public interface
    #############################################

    def generate(self, categoryList, enumerationList):
        """generate source code for enumerations."""

        self.enumerationList_ = enumerationList

        log.Log.instance().logMessage(' begin generating Enumerations ...')
        self.generateEnumeratedTypes()
        self.generateEnumeratedClasses()
        self.generateEnumeratedPairs()
        log.Log.instance().logMessage(' done generating Enumerations.')

    def generateEnumeratedTypes(self):
        """generate source file for enumerated types."""
        codeRegister = ''       # code to register the enumeration
        codeUnregister = ''     # code to unregister the enumeration
        for enumeratedTypeGroup in self.enumerationList_.enumeratedTypeGroups():
            codeRegister += self.generateEnumeratedType(enumeratedTypeGroup)
            codeUnregister += Enumerations.ENUM_UNREGISTER % { 
                'type' : enumeratedTypeGroup.type() }
        buffer = self.bufferEnumTypes_.text() % {
                'codeRegister' : codeRegister,
                'codeUnregister' : codeUnregister }
        fileName = environment.config().libFullPath() + '/Enumerations/Register/register_types.cpp'
        outputfile.OutputFile(self, fileName, 
            self.enumerationList_.enumeratedTypeCopyright(), buffer)

    def generateEnumeratedType(self, enumeratedTypeGroup):
        """generate source code for enumerated type group."""
        buffer = ''
        for enumeratedType in enumeratedTypeGroup.enumeratedTypes():
            buffer += Enumerations.ENUM_LINE % {
                'string' : enumeratedType.string(),
                'constructor' : enumeratedType.constructor() }
        return Enumerations.ENUM_REGISTER % {
                'type' : enumeratedTypeGroup.type(),
                'buffer' : buffer }

    def generateEnumeratedClasses(self):
        """generate source file for enumerated types."""
        buffer = ''   # code to register the enumeration
        for enumeratedClassGroup in self.enumerationList_.enumeratedClassGroups():
            buffer += self.generateEnumeratedClass(enumeratedClassGroup)
        buf = self.bufferEnumClasses_.text() % {
                'buffer' : buffer }
        fileName = environment.config().libFullPath() + '/Enumerations/Register/register_classes.cpp'
        outputfile.OutputFile(self, fileName, 
            self.enumerationList_.enumeratedClassCopyright(), buf)

    def generateEnumeratedClass(self, enumeratedClassGroup):
        """generate source code for enumerated type group."""
        buffer = ''
        for enumeratedType in enumeratedClassGroup.enumeratedClasses():
            buffer += Enumerations.ENUM_LINE2 % {
                'string' : enumeratedType.string(),
                'constructor' : enumeratedType.value() }
        return Enumerations.ENUM_REGISTER2 % {
                'type' : enumeratedClassGroup.className(),
                'buffer' : buffer }

    def generateEnumeratedPairs(self):
        """generate source file for enumerated types."""
        buffer = ''   # code to register the enumeration
        for enumeratedPairGroup in self.enumerationList_.enumeratedPairGroups():
            buffer += self.generateEnumeratedPair(enumeratedPairGroup)
        buf = self.bufferEnumPairs_.text() % {
                'buffer' : buffer }
        fileName = environment.config().libFullPath() + '/Enumerations/Register/register_pairs.cpp'
        outputfile.OutputFile(self, fileName, 
            self.enumerationList_.enumeratedPairCopyright(), buf)

    def generateEnumeratedPair(self, enumeratedPairGroup):
        """generate source code for enumerated type group."""
        buffer = ''
        for enumeratedPair in enumeratedPairGroup.enumeratedPairs():
            buffer += Enumerations.ENUM_LINE3 % {
                'id1' : enumeratedPair.id1(),
                'id2' : enumeratedPair.id2(),
                'value' : enumeratedPair.value() }
        return Enumerations.ENUM_REGISTER2 % {
                'type' : enumeratedPairGroup.className(),
                'buffer' : buffer }

