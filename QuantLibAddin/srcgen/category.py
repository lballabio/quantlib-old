
"""
 Copyright (C) 2005, 2006 Eric Ehlers
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

import common
import function
import serializable

class Category(serializable.Serializable):
    """class to represent a group of functions."""

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeProperty(self, common.DISPLAY_NAME)
        serializer.serializeProperty(self, common.DESCRIPTION)
        serializer.serializeObjectDict(self, function.Function)
        serializer.serializeList(self, 'includes', 'include', True)

    def platformSupported(self, platformID):
        """determine whether this category supported for given platform."""
        for function in self.Functions.values():
            if function.platformSupported(platformID):
                return True

    def getFunctions(self, platformId):
        """serve up functions alphabetically by name."""
        for functionKey in self.FunctionKeys: 
            function = self.Functions[functionKey]
            if platformId == '*' or function.platformSupported(platformId):
                yield function

    def includeList(self):
        ret = ''
        if self.__dict__.has_key('includes'):
            for include in self.includes: ret += '#include <%s>\n' % include
        else:
            ret = '#include <qla/%s.hpp>' % self.name
            if self.containsConstructor():
                ret += '\n#include <qla/vo_%s.hpp>' % self.name
        return ret

    def containsConstructor(self):
        """Indicate whether this category of functions includes a constructor.

        This function is used when generating Value Objects code to determine
        whether VOs are required for the current category."""
        for func in self.Functions.values():
            if isinstance(func, function.Constructor):
                return True

