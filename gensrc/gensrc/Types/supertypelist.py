
"""
 Copyright (C) 2007 Eric Ehlers

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

from gensrc.Utilities import common
from gensrc.Serialization import serializable
import sys
import re

class DataType(serializable.Serializable):

    groupName = 'DataTypes'
    RE_NAMESPACE = re.compile('(.*)::(.*)')
    namespace = None
    classname = None

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NATIVE_TYPE)
        serializer.serializeValue(self)

    def postSerialize(self):
        """Perform post serialization initialization."""
        m = DataType.RE_NAMESPACE.match(self.value)
        if m:
            self.namespace = m.group(1)
            self.classname = m.group(2)

class SuperType(serializable.Serializable):

    groupName = 'SuperTypes'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeAttribute(self, common.NATIVE_TYPE)
        serializer.serializeObjectList(self, DataType)

    def postSerialize(self):
        """Perform post serialization initialization."""
        for typeItem in self.DataTypes:
            typeItem.superType = self.name
            if typeItem.nativeType == None: 
                typeItem.nativeType = self.nativeType

class SuperTypeList(serializable.Serializable):

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeObjectDict(self, SuperType)

    def postSerialize(self):
        """Perform post serialization initialization."""

        self.typeNames_ = {}

        for superType in self.SuperTypes.values():
            for typeItem in superType.DataTypes:
                if self.typeNames_.has_key(typeItem.value):
                    self.typeNames_[typeItem.value].append(superType.name)
                else:
                    self.typeNames_[typeItem.value] = [superType.name]

    def getType(self, typeName, superTypeName = None):
        if not self.typeNames_.has_key(typeName):
            sys.exit('invalid type name: "%s"' % typeName)

        if superTypeName:
            if self.SuperTypes.has_key(superTypeName):
                for typeItem in self.SuperTypes[superTypeName].DataTypes:
                    if typeItem.value == typeName:
                        return typeItem
                sys.exit('supertype "%s" does not have type "%s"' % (superTypeName, typeName))
            else:
                sys.exit('invalid supertype name: "%s"' % superTypeName)
        else:
            if len(self.typeNames_[typeName]) > 1:
                sys.exit('ambiguous type name: "%s" - "%s"' % (typeName, self.typeNames_[typeName]))
            for typeItem in self.SuperTypes[self.typeNames_[typeName][0]].DataTypes:
                if typeItem.value == typeName:
                    return typeItem

