
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

from gensrc.Types import fulltype
from gensrc.Types import exceptions

class TypeList(object):
    """A collection of all the FullType objects that are defined
    for this running instance of gensrc."""

    #############################################
    # public interface
    #############################################

    def __init__(self, dataTypeDict, superTypeDict):
        self.dataTypeDict_ = dataTypeDict       # deserialized from type metadata
        self.superTypeDict_ = superTypeDict     # deserialized from type metadata
        self.fullTypeDict_ = {}                 # derived from types and supertypes

    def getType(self, dataTypeName, superTypeName = None):
        """Return the FullType that corresponds to the requested DataType/SuperType
        combination.

        If the FullType has already been derived then return it.  If not then
        construct it, add it to the dict of known FullTypes, and return it."""

        if self.fullTypeDict_.has_key((dataTypeName, superTypeName)):
            return self.fullTypeDict_[dataTypeName, superTypeName]

        if self.dataTypeDict_.dataTypes().has_key(dataTypeName):
            dataType = self.dataTypeDict_.dataTypes()[dataTypeName]
        else:
            raise exceptions.InvalidTypeNameException(dataTypeName)

        if superTypeName:
            superTypeNameEffective = superTypeName
        else:
            superTypeNameEffective = dataType.defaultSuperType()

        if self.superTypeDict_.superTypes().has_key(superTypeNameEffective):
            superType = self.superTypeDict_.superTypes()[superTypeNameEffective]
        else:
            raise exceptions.InvalidSuperTypeNameException(superTypeNameEffective)

        fullType = fulltype.FullType(dataType, superType)
        self.fullTypeDict_[dataTypeName, superTypeName] = fullType
        return fullType

