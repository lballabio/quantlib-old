
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

from gensrc.Utilities import common
from gensrc.Serialization import serializable

class SuperType(serializable.Serializable):
    """Properties that may be shared by multiple DataType objects."""

    #############################################
    # class variables
    #############################################

    groupName_ = 'SuperTypes'

    #############################################
    # public interface
    #############################################

    def nativeType(self):
        return self.nativeType_

    def conversionSuffix(self):
        return self.conversionSuffix_

    def memberAccess(self):
        return self.memberAccess_

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeAttribute(self, common.NATIVE_TYPE)
        serializer.serializeAttribute(self, common.CONVERSION_SUFFIX)
        serializer.serializeAttribute(self, common.MEMBER_ACCESS, '->')

class SuperTypeDict(serializable.Serializable):
    """Wrapper for a dictionary of SuperType objects."""

    #############################################
    # public interface
    #############################################

    def superTypes(self):
        return self.superTypes_

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeObjectDict(self, SuperType)

