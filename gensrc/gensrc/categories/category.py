
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

"""Class to represent a group of functions."""

from gensrc.utilities import common
from gensrc.utilities import utilities
from gensrc.functions import function
from gensrc.functions import supportedplatform
from gensrc.serialization import serializable
from gensrc.configuration import environment

class Category(serializable.Serializable):
    """Class to represent a group of functions."""

    # class constants

    SERIALIZATION_INCLUDES = '''
#include <%(libRootDirectory)s/%(categoryName)s.hpp>
#include <%(libRootDirectory)s/valueobjects/vo_%(categoryName)s.hpp>\n'''

    #############################################
    # public interface
    #############################################

    def platformSupported(self, platformName, implementation):
        """Determine whether this category is supported for given platform."""
        for func in self.functions_.values():
            if func.platformSupported(platformName, implementation):
                return True

    def functions(self, platformName, implementation = supportedplatform.AUTO):
        """Serve up functions alphabetically by name."""
        for functionKey in self.functionKeys_: 
            func = self.functions_[functionKey]
            if platformName == '*' \
            or func.platformSupported(platformName, implementation):
                yield func

    def includeList(self, loopBuffer = None):
        """Return the list of #include directives required for source code
        relating to this Category."""
        ret = self.addinIncludeList_
        if loopBuffer and self.containsLoopFunction_:
            ret += loopBuffer % (
                environment.config().libRootDirectory(),
                self.name_)
        return ret

    def printDebug(self):
        """Write debug info to stdout."""
        for func in self.functions('*'):
            func.printDebug()

    def xlFunctionWizardCategory(self):
        return self.xlFunctionWizardCategory_

    def containsLoopFunction(self):
        return self.containsLoopFunction_

    def copyright(self):
        return self.copyright_

    def displayName(self):
        return self.displayName_

    def description(self):
        return self.description_

    def generateVOs(self):
        return self.generateVOs_

    def serializationIncludes(self):
        return self.serializationIncludeList_

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeProperty(self, common.DISPLAY_NAME)
        serializer.serializeProperty(self, common.DESCRIPTION)
        serializer.serializeProperty(self, common.FUNCTION_CATEGORY)
        serializer.serializeObjectDict(self, function.Function)
        serializer.serializeList(self, common.ADDIN_INCLUDES, common.INCLUDE, True)
        serializer.serializeList(self, common.SERIALIZATION_INCLUDES, common.INCLUDE, True)
        serializer.serializeProperty(self, common.COPYRIGHT)

    def postSerialize(self):
        """Perform post serialization initialization."""
        self.generateVOs_ = False
        self.containsLoopFunction_ = False
        for func in self.functions_.values():
            if func.generateVOs():
                self.generateVOs_ = True
            if func.loopParameter():
                self.containsLoopFunction_ = True

    def sort_uniq(self, list):
        ret = []
        for item in list:
            if item and ret.count(item) == 0: ret.append(item)
        ret.sort()
        return ret

    def enumIncludes(self, enumerationList, constructorOnly):
        ret = ''
        if environment.config().usingEnumerations():
            enumIncludes = []
            for func in self.functions_.values():
                if constructorOnly and not func.generateVOs(): continue
                enumIncludes.extend(enumerationList.enumIncludes(
                    func.parameterList().parameters()))
            enumIncludesUnique = self.sort_uniq(enumIncludes)
            for enumInclude in enumIncludesUnique:
                ret += '#include <%s>\n' % enumInclude
        return ret
        
    def init(self, enumerationList):
        """Generate list of #include directives necessary to compile code
        in this category."""

        self.addinIncludeList_ = self.enumIncludes(enumerationList, False)

        if self.addinIncludes_ == None:
            self.addinIncludeList_ += '#include <%s/%s.hpp>\n' % (
                environment.config().libRootDirectory(),
                self.name_)
        else:
            for includeFile in self.addinIncludes_:
                self.addinIncludeList_ += '#include <%s>\n' % includeFile

        if self.generateVOs_:
            self.addinIncludeList_ += '#include <%s/valueobjects/vo_%s.hpp>\n' % (
                environment.config().libRootDirectory(),
                self.name_)

        self.serializationIncludeList_ = self.enumIncludes(enumerationList, True)

        if self.serializationIncludes_ == None:
            self.serializationIncludeList_ += Category.SERIALIZATION_INCLUDES % {
                'categoryName' : self.name_,
                'libRootDirectory' : environment.config().libRootDirectory() }
        else:
            for includeFile in self.serializationIncludes_:
                self.serializationIncludeList_ += '#include <%s>\n' % includeFile

