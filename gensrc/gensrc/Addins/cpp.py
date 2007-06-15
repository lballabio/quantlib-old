
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

"""Generate source code for C++ addin."""

from gensrc.Addins import addin
from gensrc.Addins import cppexceptions
#from gensrc.Serialization import serializable
from gensrc.Utilities import outputfile
#from gensrc.Functions import supportedplatform
#from gensrc.Serialization import xmlreader
#from gensrc.Utilities import buffer
#from gensrc.Utilities import common
from gensrc.Utilities import log
from gensrc.Categories import category
#from gensrc.Configuration import environment

#import re
#import string

class CppAddin(addin.Addin):
    """Generate source code for C++ addin."""

    #############################################
    # class variables
    #############################################

    voSupported_ = True
    convertPermanentFlag_ = '''
        bool permanentCpp =
            ObjectHandler::callOperToScalar<bool>(*permanent, "permanent", false);
        if (permanentCpp)
            objectPointer->setPermanent();'''

    #############################################
    # public interface
    #############################################

    def generate(self, categoryList, enumerationList):
        """Generate source code for C++ addin."""

        self.categoryList_ = categoryList
        self.enumerationList_ = enumerationList

        log.Log.instance().logMessage(' begin generating %s...' % self.name_)
        self.generateFunctions()
        log.Log.instance().logMessage(' done generating %s.' % self.name_)

    def generateFunctions(self):
        """Generate source code for all functions in all categories."""
        bufferAll = ''
        for cat in self.categoryList_.categories(self.name_):
            categoryIncludes = cat.includeList()
            #if cat.containsLoopFunction():
            #    categoryIncludes += LOOP_INCLUDES % (
            #        environment.config().loopRootDirectory(),
            #        cat.name())
            bufferAll += "#include <%s.hpp>\n" % cat.name()
            bufferCpp = ''
            bufferHpp = ''
            for func in cat.functions(self.name_): 
                bufferCpp += self.generateFunction(func)
                bufferHpp += self.generateDeclaration(func)
            bufferBody = self.bufferBody_.text() % { 
                'bufferCpp' : bufferCpp,
                'categoryIncludes' : categoryIncludes }
            bufferHeader = self.bufferHeader_.text() % { 
                'categoryName' : cat.name(),
                'bufferHpp' : bufferHpp }
            fileNameCpp = '%s%s.cpp' % ( self.rootPath_, cat.name())
            outputfile.OutputFile(self, fileNameCpp, cat.copyright(), bufferBody)
            fileNameHpp = '%s%s.hpp' % ( self.rootPath_, cat.name())
            outputfile.OutputFile(self, fileNameHpp, cat.copyright(), bufferHeader)
        fileNameAll = '%saddincpp.hpp' % self.rootPath_
        outputfile.OutputFile(self, fileNameAll, self.copyright_, bufferAll)

    def generateFunction(self, func):
        """Generate source code for a given function."""
        return self.bufferFunction_.text() % {
            'functionBody' : func.generateBody(self),
            'functionReturnType' : self.functionReturnType_.apply(func.returnValue()),
            'functionDeclaration' : func.parameterList().generate(self.functionDeclaration_),
            'functionName' : func.name() }

    def generateDeclaration(self, func):
        """Generate source code for a given function."""
        return self.bufferDeclaration_.text() % {
            'functionReturnType' : self.functionReturnType_.apply(func.returnValue()),
            'functionDeclaration' : func.parameterList().generate(self.functionDeclaration_),
            'functionName' : func.name() }

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        super(CppAddin, self).serialize(serializer)

