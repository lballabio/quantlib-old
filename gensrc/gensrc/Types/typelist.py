
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

    #############################################
    # public interface
    #############################################

    def __init__(self, typeDict, superTypeDict):
        self.typeDict_ = typeDict
        self.superTypeDict_ = superTypeDict
        self.fullTypeDict_ = {}

    def getType(self, typeName, superTypeName = None):

        if self.fullTypeDict_.has_key((typeName, superTypeName)):
            return self.fullTypeDict_[typeName, superTypeName]

        if self.typeDict_.dataTypes().has_key(typeName):
            type = self.typeDict_.dataTypes()[typeName]
        else:
            raise exceptions.InvalidTypeNameException(typeName)

        if superTypeName:
            superTypeNameEffective = superTypeName
        else:
            superTypeNameEffective = type.defaultSuperType()

        if self.superTypeDict_.superTypes().has_key(superTypeNameEffective):
            superType = self.superTypeDict_.superTypes()[superTypeNameEffective]
        else:
            raise exceptions.InvalidSuperTypeNameException(superTypeNameEffective)

        fullType = fulltype.FullType(type, superType)
        self.fullTypeDict_[typeName, superTypeName] = fullType
        return fullType

