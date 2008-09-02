
"""
 Copyright (C) 2005, 2006 Plamen Neykov
 Copyright (C) 2007, 2008 Eric Ehlers

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

"""generate source code for ValueObjects."""

from gensrc.addins import addin
from gensrc.functions import function
from gensrc.utilities import outputfile
from gensrc.utilities import common
from gensrc.utilities import log
from gensrc.categories import category
from gensrc.configuration import environment

class ValueObjects(addin.Addin):
    """Generate source code for ValueObjects."""

    VO_INCLUDE = '''\
#include <%(libRootDirectory)s/valueobjects/vo_%(categoryName)s.hpp>\n'''

    PROCESSOR_NAME = '''\
        virtual std::string processorName() { return "%(processorName)s"; }'''

    #############################################
    # public interface
    #############################################

    def generate(self, categoryList, enumerationList):
        """Generate source code for ValueObjects."""

        self.categoryList_ = categoryList
        self.enumerationList_ = enumerationList

        allIncludes = ''

        log.Log.instance().logMessage(' begin generating ValueObjects ...')

        for cat in self.categoryList_.categories('*', self.coreCategories_, self.addinCategories_):
            if cat.generateVOs():
                allIncludes += ValueObjects.VO_INCLUDE % {
                    'categoryName' : cat.name(),
                    'libRootDirectory' : environment.config().libRootDirectory() }
                if self.headersInline_:
                    self.generateHeadersInline(cat)
                else:
                    self.generateHeaders(cat)
                    self.generateFunctions(cat)

        allBuffer =  self.bufferAll_.text() % {
            'allIncludes' : allIncludes,
            'libRootDirectory' : environment.config().libRootDirectory() }
        allFilename = self.rootPath_ + 'vo_all.hpp'
        outputfile.OutputFile(self, allFilename, self.copyright_, allBuffer)

        log.Log.instance().logMessage(' done generating ValueObjects.')

    def generateHeaderInline(self, func):
        """Generate class definition source for prototype of given constructor function."""
        if func.processorName():
            processorName = ValueObjects.PROCESSOR_NAME % {
                common.PROCESSOR_NAME : func.processorName() }
        else:
            processorName = ""
        return self.bufferClassDeclInline_.text() % {
            'constructorDeclaration' : func.parameterList().generate(self.constructorDeclaration_),
            'functionName' : func.name(),
            'processorName' : processorName,
            'serializeMembers' : func.parameterList().generate(self.serializeMembers_),
            'memberDeclaration' : func.parameterList().generate(self.memberDeclaration_) }

    def generateFunctionInline(self, func):
        """Generate source code for function."""
        return self.bufferClassBodyInline_.text() % {
            'constructorInit' : func.parameterList().generate(self.constructorInit_),
            'constructorParList' : func.parameterList().generate(self.constructorDeclaration_),
            'functionName' : func.name(),
            'propertyDeclaration' : func.parameterList().generate(self.propertyDeclaration_),
            'propertyGet' : func.parameterList().generate(self.propertyGet_),
            'propertySet' : func.parameterList().generate(self.propertySet_),
            'propertyInsert' : func.parameterList().generate(self.propertyInsert_),
            'propertyPush' : func.parameterList().generate(self.propertyPush_),
            'populateObjectIDs' : func.parameterList().generate(self.populateObjectIDs_) }

    def generateHeadersInline(self, cat):
        """Generate class source for constructor function prototypes."""
        bufHeader = ''
        bufFunc = ''
        for func in cat.functions('*'):
            if func.generateVOs():
                bufHeader += self.generateHeaderInline(func)
                bufFunc += self.generateFunctionInline(func)

        bufHeaderAll = self.bufferIncludesInline_.text() % {
            'categoryName' : cat.name(),
            'functions' : bufFunc,
            'headers' : bufHeader,
            'libRoot' : environment.config().libRootDirectory(),
            'namespaceObjects' : environment.config().namespaceObjects() }
        fileName = self.rootPath_ + 'vo_' + cat.name() + '.hpp'
        outputfile.OutputFile(self, fileName, self.copyright_, bufHeaderAll)

    def generateHeader(self, func):
        """Generate class definition source for prototype of given constructor function."""
        if func.processorName():
            processorName = ValueObjects.PROCESSOR_NAME % {
                common.PROCESSOR_NAME : func.processorName() }
        else:
            processorName = ""
        return self.bufferClassDecl_.text() % {
            'constructorDeclaration' : func.parameterList().generate(self.constructorDeclaration_),
            'functionName' : func.name(),
            'processorName' : processorName,
            'serializeMembers' : func.parameterList().generate(self.serializeMembers_),
            'memberDeclaration' : func.parameterList().generate(self.memberDeclaration_) }

    def generateHeaders(self, cat):
        """Generate class source for constructor function prototypes."""
        bufHeader = ''
        for func in cat.functions('*'):
            if func.generateVOs():
                bufHeader += self.generateHeader(func)

        bufHeaderAll = self.bufferIncludesDecl_.text() % {
            'categoryName' : cat.name(),
            'headers' : bufHeader,
            'libRoot' : environment.config().libRootDirectory(),
            'namespaceObjects' : environment.config().namespaceObjects() }
        fileName = self.rootPath_ + 'vo_' + cat.name() + '.hpp'
        outputfile.OutputFile(self, fileName, self.copyright_, bufHeaderAll)

    def generateFunction(self, func):
        """Generate source code for function."""
        return self.bufferClassBody_.text() % {
            'constructorInit' : func.parameterList().generate(self.constructorInit_),
            'constructorParList' : func.parameterList().generate(self.constructorDeclaration_),
            'functionName' : func.name(),
            'propertyDeclaration' : func.parameterList().generate(self.propertyDeclaration_),
            'propertyGet' : func.parameterList().generate(self.propertyGet_),
            'propertySet' : func.parameterList().generate(self.propertySet_),
            'populateObjectIDs' : func.parameterList().generate(self.populateObjectIDs_) }

    def generateFunctions(self, cat):
        """Generate source for function implementations."""
        bufFunc = ''
        for func in cat.functions('*'): 
            if func.generateVOs():
                bufFunc += self.generateFunction(func)

        bufFuncAll = self.bufferIncludes_.text() % {
            'categoryName' : cat.name(),
            'functions' : bufFunc,
            'libRoot' : environment.config().libRootDirectory(),
            'namespaceObjects' : environment.config().namespaceObjects() }
        fileName = self.rootPath_ + 'vo_' + cat.name() + '.cpp'
        outputfile.OutputFile(self, fileName, self.copyright_, bufFuncAll)

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        super(ValueObjects, self).serialize(serializer)
        serializer.serializeBoolean(self, common.HEADERS_INLINE, False)

