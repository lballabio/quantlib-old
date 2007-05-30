
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

"""class to represent a group of functions."""

from gensrc.Utilities import common
from gensrc.Utilities import utilities
from gensrc.Functions import function
from gensrc.Functions import supportedplatform
from gensrc.Serialization import serializable
from gensrc.Configuration import environment

class Category(serializable.Serializable):
    """class to represent a group of functions."""

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

    def includeList(self):
        """Generate list of #include directives necessary to compile code
        in this category."""
        ret = ''
        if self.includes_ == None:
            ret = '#include <%s/%s.hpp>\n' % (
                environment.config().libRootDirectory(),
                self.name_)
        else:
            for includeFile in self.includes_:
                ret += '#include <%s>\n' % includeFile
        if self.generateVOs_:
            ret += '#include <%s/vo_%s.hpp>\n' % (
                environment.config().voRootDirectory(),
                self.name_)
        return ret

    def printDebug(self):
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

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeProperty(self, common.DISPLAY_NAME)
        serializer.serializeProperty(self, common.DESCRIPTION)
        serializer.serializeProperty(self, common.FUNCTION_CATEGORY)
        serializer.serializeObjectDict(self, function.Function)
        serializer.serializeList(self, 'includes', 'include', True)
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

