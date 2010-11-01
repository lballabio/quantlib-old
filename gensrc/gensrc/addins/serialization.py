
"""
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

"""Generate source code for Serialization."""

from gensrc.addins import addin
from gensrc.addins import serializationexceptions
from gensrc.functions import function
from gensrc.utilities import outputfile
from gensrc.utilities import common
from gensrc.utilities import log
from gensrc.categories import category
from gensrc.configuration import environment
import os
import re

class Serialization(addin.Addin):
    """Generate source code for Serialization."""

    # class constants

    DECLARE_CREATOR = '''\
    boost::shared_ptr<ObjectHandler::Object> create_%(functionName)s(
        const boost::shared_ptr<ObjectHandler::ValueObject>&);\n\n'''
    REGISTER_CREATOR = '''\
        registerCreator("%(functionName)s", create_%(functionName)s);\n'''
    REGISTER_TYPE = '''\
        // %(categoryDisplayName)s\n
        register_%(categoryName)s(ar);\n\n'''
    REGISTER_CALL = '''\
        // class ID %(classID)d in the boost serialization framework
        ar.register_type<%(namespaceObjects)s::ValueObjects::%(functionName)s>();\n'''
    REGISTER_CLASS = 'BOOST_CLASS_EXPORT(%(namespaceObjects)s::ValueObjects::%(functionName)s)\n'
    INCLUDE_CREATOR = '''\
#include <%(libRootDirectory)s/serialization/create/create_%(categoryName)s.hpp>\n'''
    REGISTER_INCLUDE = '''\
#include <%(addinDirectory)s/serialization/register/serialization_%(categoryName)s.hpp>\n'''

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
        self.generateRegister()
        log.Log.instance().logMessage(' done generating %s.' % self.name_)

    def generateFactory(self):
        """Generate source code for all functions in all categories."""

        bufferCreators = ''

        for cat in self.categoryList_.categories('*', self.coreCategories_, self.addinCategories_):
            if not cat.generateVOs(): continue

            bufferCreators += '\n        // %s\n\n' % cat.displayName()

            for func in cat.functions('*'):
                if not func.generateVOs(): continue

                bufferCreators += Serialization.REGISTER_CREATOR % {
                    'functionName' : func.name() }

        self.bufferFactory_.set({
            'bufferCreators' : bufferCreators,
            'libRootDirectory' : environment.config().libRootDirectory(),
            'namespaceObjects' : environment.config().namespaceObjects() })
        factoryFile = self.rootPath_ + 'register_creators.cpp'
        outputfile.OutputFile(self, factoryFile, self.copyright_, self.bufferFactory_)

    def generateCreators(self):
        """Generate source code for all functions in all categories."""

        bufferAll = ''

        for cat in self.categoryList_.categories('*', self.coreCategories_, self.addinCategories_):
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
                bufferCreators += self.bufferCreator_.set({
                    'cppConversions' : func.parameterList().generate(
                        self.cppConversions_),
                    'enumConversions' : func.parameterList().generate(
                        self.enumConversions_),
                    'functionName' : func.name(),
                    'libraryCall' : func.parameterList().generate(
                        self.libraryCall_),
                    'libraryConversions' : func.parameterList().generate(
                        self.libraryConversions_),
                    'libraryFunction' : func.libraryFunction(),
                    'objectConversions' : func.parameterList().generate(
                        self.objectConversions_),
                    'namespaceObjects' : environment.config().namespaceObjects(),
                    'referenceConversions' : func.parameterList().generate(
                        self.referenceConversions_),
                    'populateObjectIDs' : func.parameterList().generate(
                        self.populateObjectIDs_) })

            self.bufferHeader_.set({
                'categoryName' : cat.name(),
                'bufferDeclarations' : bufferDeclarations,
                'libRootDirectory' : environment.config().libRootDirectory(),
                'namespaceObjects' : environment.config().namespaceObjects() })
            createHeaderFile = self.rootPath_ + 'create/create_' + cat.name() + '.hpp'
            outputfile.OutputFile(self, createHeaderFile, self.copyright_, self.bufferHeader_)

            self.bufferIncludes_.set({
                'bufferCreators' : bufferCreators,
                'serializationIncludes' : cat.serializationIncludes(),
                'categoryName' : cat.name() })
            createBodyFile = self.rootPath_ + 'create/create_' + cat.name() + '.cpp'
            outputfile.OutputFile(self, createBodyFile, self.copyright_, self.bufferIncludes_)

        self.bufferAll_.set({
            'bufferAll' : bufferAll,
            'libRootDirectory' : environment.config().libRootDirectory() })
        createAllFile = self.rootPath_ + 'create/create_all.hpp'
        outputfile.OutputFile(self, createAllFile, self.copyright_, self.bufferAll_)

    def generateRegister(self):
        """Generate source code for serialization factory."""

        allIncludes = ''
        bufferRegister = ''

        # Keep track of the ID assigned to each Addin class by
        # the boost serialization framework.  This number is initialized to 4
        # because 0-3 are reserved for ObjectHandler as explained in file
        # QuantLibAddin/qlo/serialization/register/serialization_oh.cpp
        classID = 4

        # Initialize the global map with the values reserved for ObjectHandler.
        # 0 and 1 refer respectively to ValueObject and vector of ValueObject,
        # but these are omitted because they never occur in app XML data.
        idMap = { 'ohGroup' : 2, 'ohRange' : 3 }

        for cat in self.categoryList_.categories(
            '*', self.coreCategories_, self.addinCategories_):

            if not cat.generateVOs(): continue

            bufferCpp = ''
            allIncludes += Serialization.REGISTER_INCLUDE % {
                'categoryName' : cat.name(),
                'addinDirectory' : environment.config().libRootDirectory() }

            bufferRegister += Serialization.REGISTER_TYPE % {
                'categoryDisplayName' : cat.displayName(),
                'categoryName' : cat.name() }

            self.bufferSerializeDeclaration_.set({
                'addinDirectory' : environment.config().libRootDirectory(),
                'categoryName' : cat.name(),
                'namespaceAddin' : environment.config().namespaceObjects() })
            headerFile = '%sregister/serialization_%s.hpp' % (
                    self.rootPath_, cat.name() )
            outputfile.OutputFile(self, headerFile, self.copyright_, self.bufferSerializeDeclaration_)

            export_stm = ''
            for func in cat.functions('*'):
                if not func.generateVOs(): continue

                bufferCpp += Serialization.REGISTER_CALL % {
                    'classID' : classID,
                    'functionName' : func.name(),
                    'namespaceObjects' : environment.config().namespaceObjects() }
                export_stm += Serialization.REGISTER_CLASS % {
                    'classID' : classID,
                    'functionName' : func.name(),
                    'namespaceObjects' : environment.config().namespaceObjects() }
                idMap[func.name()] = classID
                classID += 1

            self.bufferSerializeBody_.set({
                'addinDirectory' : environment.config().libRootDirectory(),
                'bufferCpp' : bufferCpp,
                'categoryName' : cat.name(),
                'libRootDirectory' : environment.config().libRootDirectory(),
                'namespaceAddin' : environment.config().namespaceObjects(),
                'export_stm': export_stm})
            cppFile = '%sregister/serialization_%s.cpp' % ( self.rootPath_, cat.name() )
            outputfile.OutputFile(self, cppFile, self.copyright_, self.bufferSerializeBody_)

        self.bufferSerializeAll_.set({
            'includeGuard' : environment.config().libRootDirectory(),
            'allIncludes' : allIncludes,
            'addinDirectory' : environment.config().libRootDirectory() })
        allFilename = self.rootPath_ + 'register/serialization_all.hpp'
        outputfile.OutputFile(self, allFilename, self.copyright_, self.bufferSerializeAll_)

        self.bufferSerializeRegister_.set({
            'addinDirectory' : environment.config().libRootDirectory(),
            'bufferRegister' : bufferRegister,
            'namespaceAddin' : environment.config().namespaceObjects() })
        factoryFile = self.rootPath_ + 'register/serialization_register.hpp'
        outputfile.OutputFile(self, factoryFile, self.copyright_, self.bufferSerializeRegister_)

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        super(Serialization, self).serialize(serializer)

