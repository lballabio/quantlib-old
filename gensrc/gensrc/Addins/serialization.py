
"""
 Copyright (C) 2007 Eric Ehlers

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

"""Generate source code for Serialization."""

from gensrc.Addins import addin
from gensrc.Functions import function
from gensrc.Utilities import outputfile
from gensrc.Utilities import common
from gensrc.Utilities import log
from gensrc.Categories import category
from gensrc.Configuration import environment

class Serialization(addin.Addin):
    """Generate source code for Serialization."""

    # constants

    DECLARE_CREATOR = '''\
    boost::shared_ptr<ObjectHandler::Object> create_%(functionName)s(
        const boost::shared_ptr<ObjectHandler::ValueObject>&);\n\n'''
    REGISTER_CREATOR = '''\
        registerCreator("%(functionName)s", create_%(functionName)s);\n'''
    REGISTER_TYPE = '''\
            ar.template register_type<%(namespaceObjects)s::ValueObjects::%(functionName)s>();\n'''
    INCLUDE_CREATOR = '''\
#include <%(libRootDirectory)s/Serialization/create_%(categoryName)s.hpp>\n'''

    #############################################
    # public interface
    #############################################

    def generate(self, categoryList, enumerationList):
        """Generate source code for Serialization."""

        self.categoryList_ = categoryList
        self.enumerationList_ = enumerationList

        log.Log.instance().logMessage(' begin generating %s...' % self.name_)
        self.generateCreators()
        self.generateFactory()
        log.Log.instance().logMessage(' done generating %s.' % self.name_)

    def generateFactory(self):
        """Generate source code for all functions in all categories."""

        bufferCreators = ''
        
        for cat in self.categoryList_.categories('*'):
            if not cat.generateVOs(): continue
                            
            bufferCreators += '\n        // %s\n\n' % cat.displayName()
            
            for func in cat.functions('*'):
                if not func.generateVOs(): continue
                
                bufferCreators += Serialization.REGISTER_CREATOR % {
                    'functionName' : func.name() }
                        
        factoryBuffer = self.bufferFactory_.text() % {
            'bufferCreators' : bufferCreators,
            'libRootDirectory' : environment.config().libRootDirectory(),
            'namespaceObjects' : environment.config().namespaceObjects() }
        factoryFile = self.rootPath_ + 'serializationfactory.cpp'
        outputfile.OutputFile(self, factoryFile, self.copyright_, factoryBuffer)

    def generateCreators(self):
        """Generate source code for all functions in all categories."""

        bufferAll = ''

        for cat in self.categoryList_.categories('*'):
            if not cat.generateVOs(): continue

            bufferDeclarations = ''
            bufferCreators = ''
            
            bufferAll += Serialization.INCLUDE_CREATOR % {
                'categoryName' : cat.name(),
                'libRootDirectory' : environment.config().libRootDirectory() }

            for func in cat.functions('*'):
                if not func.generateVOs(): continue
                bufferDeclarations += Serialization.DECLARE_CREATOR % {
                    'functionName' : func.name() }
                bufferCreators += self.bufferCreator_.text() % {
                    'cppConversions' : func.parameterList().generate(self.cppConversions_),
                    'enumConversions' : func.parameterList().generate(self.enumConversions_),
                    'functionName' : func.name(),
                    'libraryCall' : func.parameterList().generate(self.libraryCall_),
                    'libraryConversions' : func.parameterList().generate(self.libraryConversions_),
                    'libraryFunction' : func.libraryFunction(),
                    'objectConversions' : func.parameterList().generate(self.objectConversions_),
                    'namespaceObjects' : environment.config().namespaceObjects(),
                    'referenceConversions' : func.parameterList().generate(self.referenceConversions_) }

            createHeaderBuffer = self.bufferHeader_.text() % {
                'categoryName' : cat.name(),
                'bufferDeclarations' : bufferDeclarations,
                'libRootDirectory' : environment.config().libRootDirectory(),
                'namespaceObjects' : environment.config().namespaceObjects() }
            createHeaderFile = self.rootPath_ + 'create_' + cat.name() + '.hpp'
            outputfile.OutputFile(self, createHeaderFile, self.copyright_, createHeaderBuffer)

            createBodyBuffer = self.bufferIncludes_.text() % {
                'bufferCreators' : bufferCreators,
                'categoryIncludes' : cat.includeList(),
                'categoryName' : cat.name() }
            createBodyFile = self.rootPath_ + 'create_' + cat.name() + '.cpp'
            outputfile.OutputFile(self, createBodyFile, self.copyright_, createBodyBuffer)

        createAllBuffer = self.bufferAll_.text() % {
            'bufferAll' : bufferAll,
            'libRootDirectory' : environment.config().libRootDirectory() }
        createAllFile = self.rootPath_ + 'create_all.hpp'
        outputfile.OutputFile(self, createAllFile, self.copyright_, createAllBuffer)

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        super(Serialization, self).serialize(serializer)

