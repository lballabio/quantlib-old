
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

"""class to represent a group of functions."""

from gensrc.Utilities import common
from gensrc.Utilities import utilities
from gensrc.Functions import function
from gensrc.Serialization import serializable
from gensrc.Configuration import environment

class Category(serializable.Serializable):
    """class to represent a group of functions."""

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
        self.generateVOs = False
        self.containsLoopFunction = False
        for func in self.Functions.values():
            if func.generateVOs:
                self.generateVOs = True
            if func.loopParameter:
                self.containsLoopFunction = True

    def platformSupported(self, platformName, implementation):
        """Determine whether this category is supported for given platform."""
        for function in self.Functions.values():
            if function.platformSupported(platformName, implementation):
                return True

    def getFunctions(self, platformName, implementation = function.AUTO):
        """Serve up functions alphabetically by name."""
        for functionKey in self.FunctionKeys: 
            function = self.Functions[functionKey]
            if platformName == '*' \
            or function.platformSupported(platformName, implementation):
                yield function

    def includeList(self):
        """Generate list of #include directives necessary to compile code
        in this category."""
        ret = ''
        if self.includes == None:
            ret = '#include <%s/%s.hpp>\n' % (
                environment.config().libRootDirectory,
                self.name)
        else:
            for includeFile in self.includes:
                ret += '#include <%s>\n' % includeFile
        if self.generateVOs:
            ret += '#include <%s/vo_%s.hpp>\n' % (
                environment.config().voRootDirectory,
                self.name)
        return ret

