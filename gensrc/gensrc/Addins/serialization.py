
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
from gensrc.Addins import serializationexceptions
from gensrc.Functions import function
from gensrc.Utilities import outputfile
from gensrc.Utilities import common
from gensrc.Utilities import log
from gensrc.Categories import category
from gensrc.Configuration import environment
import glob
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
    INCLUDE_CREATOR = '''\
#include <%(libRootDirectory)s/Serialization/create_%(categoryName)s.hpp>\n'''
    REGISTER_INCLUDE = '''\
#include <%(addinDirectory)s/Serialization/serialization_%(categoryName)s.hpp>\n'''

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
                'serializationIncludes' : cat.serializationIncludes(),
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

#############################################
# utility functions
#############################################

# Map class names to the ID numbers from the boost serialization framework.
# Global because it's shared by generateSerialization() and findReplace().
idMap = {}

def findReplace(m):
    """This function is called with a match object containing the 5 groups
    documented in the regex below.  Group 2 is the old class ID and group 4
    is the class name.  Use the class name to look up the new class ID in
    the map, and return the original text with new ID replacing old."""
    global idMap

    className = m.group(4)
    if className in idMap:
        return m.group(1) + str(idMap[className]) + m.group(3) + className + m.group(5)
    else:
        raise serializationexceptions.InvalidClassException(className)

def updateData(addin):
    """Bring application XML data up to date with any changes that may have occurred
    to the ID numbers that the boost serialization framework assigns to Addin classes."""

#   The pattern we are searching for looks like this:
#       <px class_id_reference="114" object_id="_2">
#           <objectID>EURSwaptionMeanReversion10Y_QuoteHandle</objectID>
#           <className>qlRelinkableHandleQuote</className>
#           <CurrentLink>EURSwaptionMeanReversion10Y_Quote</CurrentLink>
#       </px>

#   We chop it into 5 groups:
    pattern = r'''
(<px\ (?:(?:class_id_reference)|(?:class_id))=")    # 1) Match either of the following:
                                                    #       <px class_id="
                                                    #       <px class_id_reference="
(\d*)                                               # 2) the class ID number
(".*?<className>)                                   # 3) everything from endquote " to tag <className>
(\w*)                                               # 4) the class name
(</className>.*?</px>)                              # 5) everything else
'''

    # Compile the regex
    regex = re.compile(pattern, re.M | re.S | re.X)

    # Loop through the application data files
    dataDirectoryPattern = addin.dataDirectory() + '/*.xml'
    for fileName in glob.glob(dataDirectoryPattern):
        # Perform the find and replace on each file
        fileBuffer = open(fileName)
        bufferIn = fileBuffer.read()
        fileBuffer.close()
        bufferOut = regex.sub(findReplace, bufferIn)
        outputfile.OutputFile(addin, fileName, None, bufferOut, False)

def generateSerialization(addin):
    """Generate source code for all functions in all categories.

    This is a utility function, not part of the Serialization addin class.
    This function generates the source code required for the serialization
    factory.  The code is platform independent.  However, the generated source
    code must be compiled as part of the final Addin binary (not in a static
    library) because of issues related to linking and to template
    metaprogramming performed by the boost::serialization library. 
    Each Addin that supports serialization must call in to this function."""
    global idMap

    allIncludes = ''
    bufferRegister = ''
    includeGuard = 'addin_' + addin.name().lower()
    addinDirectory = addin.relativePath()

    # Keep track of the ID assigned to each Addin class by
    # the boost serialization framework.  This number is initialized to 3
    # because 0, 1 and 2 are reserved for ObjectHandler as explained in file
    # QuantLibAddin/Addins/Cpp/Serialization/serialization_oh.cpp
    classID = 3
    # Initialize the global map with the values reserved for ObjectHandler.
    # 0 and 1 refer respectively to ValueObject and vector of ValueObject,
    # but these are omitted because they never occur in app XML data.
    idMap = { 'ohRange' : 3 }

    for cat in addin.categoryList_.categories('*'):

        if not cat.generateVOs(): continue

        bufferCpp = ''
        allIncludes += Serialization.REGISTER_INCLUDE % {
            'categoryName' : cat.name(),
            'addinDirectory' : addinDirectory }

        bufferRegister += Serialization.REGISTER_TYPE % {
            'categoryDisplayName' : cat.displayName(),
            'categoryName' : cat.name() }

        bufferHpp = addin.bufferSerializeDeclaration_.text() % {
            'addinDirectory' : includeGuard,
            'categoryName' : cat.name(),
            'namespaceAddin' : addin.namespaceAddin_ }
        headerFile = '%sSerialization/serialization_%s.hpp' % ( addin.rootPath_, cat.name() )
        outputfile.OutputFile(addin, headerFile, addin.copyright_, bufferHpp)

        for func in cat.functions('*'):
            if not func.generateVOs(): continue

            bufferCpp += Serialization.REGISTER_CALL % {
                'classID' : classID,
                'functionName' : func.name(),
                'namespaceObjects' : environment.config().namespaceObjects() }

            idMap[func.name()] = classID
            classID += 1

        bufferBody = addin.bufferSerializeBody_.text() % {
            'addinDirectory' : addinDirectory,
            'bufferCpp' : bufferCpp,
            'categoryName' : cat.name(),
            'libRootDirectory' : environment.config().libRootDirectory(),
            'namespaceAddin' : addin.namespaceAddin_ }
        cppFile = '%sSerialization/serialization_%s.cpp' % ( addin.rootPath_, cat.name() )
        outputfile.OutputFile(addin, cppFile, addin.copyright_, bufferBody)
    allBuffer =  addin.bufferSerializeAll_.text() % {
        'includeGuard' : includeGuard,
        'allIncludes' : allIncludes,
        'addinDirectory' : addinDirectory }
    allFilename = addin.rootPath_ + 'Serialization/serialization_all.hpp'
    outputfile.OutputFile(addin, allFilename, addin.copyright_, allBuffer)

    #if addin.serializationBase_:
    #    callBaseIn = "%s::register_in(ia);" % addin.serializationBase_
    #    callBaseOut = "%s::register_out(oa);" % addin.serializationBase_
    #else:
    #    callBaseIn =''
    #    callBaseOut =''

    bufferFactory = addin.bufferSerializeRegister_.text() % {
        'addinDirectory' : addinDirectory,
        'bufferRegister' : bufferRegister,
        'namespaceAddin' : addin.namespaceAddin_ } 
    #    'libRootDirectory' : environment.config().libRootDirectory(),
    #    'callBaseIn': callBaseIn,
    #    'callBaseOut': callBaseOut}
    factoryFile = addin.rootPath_ + 'Serialization/serialization_register.hpp'
    outputfile.OutputFile(addin, factoryFile, addin.copyright_, bufferFactory)

    if addin.dataDirectory():
        updateData(addin)

