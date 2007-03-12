
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

from gensrc.Types import exceptions
from gensrc.Utilities import common
from gensrc.Serialization import serializable
import re

class DataType(serializable.Serializable):

    groupName_ = 'DataTypes'
    #namespace_ = None
    #classname_ = None
    namespace_ = ''
    classname_ = ''
    RE_NAMESPACE = re.compile('(.*)::(.*)')

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NATIVE_TYPE)
        serializer.serializeValue(self)

    def postSerialize(self):
        """Perform post serialization initialization."""
        m = DataType.RE_NAMESPACE.match(self.value_)
        if m:
            self.namespace_ = m.group(1)
            self.classname_ = m.group(2)

    def value(self):
        return self.value_

    def setSuperType(self, value):
        self.superType_ = value

    def overrideNativeType(self, value):
        if self.nativeType_ == None:
            self.nativeType_ = value

    def nativeType(self):
        return self.nativeType_

    def superType(self):
        return self.superType_

    def classname(self):
        return self.classname_

class SuperType(serializable.Serializable):

    groupName_ = 'SuperTypes'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeAttribute(self, common.NATIVE_TYPE)
        serializer.serializeObjectList(self, DataType)

    def postSerialize(self):
        """Perform post serialization initialization."""
        for typeItem in self.dataTypes_:
            typeItem.setSuperType(self.name_)
            typeItem.overrideNativeType(self.nativeType_)

    def dataTypes(self):
        return self.dataTypes_

class SuperTypeList(serializable.Serializable):

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeObjectDict(self, SuperType)

    def postSerialize(self):
        """Perform post serialization initialization."""

        self.typeNames_ = {}

        for superType in self.superTypes_.values():
            for typeItem in superType.dataTypes():
                if self.typeNames_.has_key(typeItem.value()):
                    self.typeNames_[typeItem.value()].append(superType.name())
                else:
                    self.typeNames_[typeItem.value()] = [superType.name()]

    def getType(self, typeName, superTypeName = None):
        if not self.typeNames_.has_key(typeName):
            raise exceptions.TypeNameUndefinedException(typeName)

        if superTypeName:
            if self.superTypes_.has_key(superTypeName):
                for typeItem in self.superTypes_[superTypeName].dataTypes():
                    if typeItem.value() == typeName:
                        return typeItem
                raise exceptions.TypeSuperUndefinedException(typeName, superTypeName)
            else:
                raise exceptions.SupertypeNameUndefinedException(superTypeName)
        else:
            if len(self.typeNames_[typeName]) > 1:
                raise exceptions.TypeNameAmbiguousException(typeName, self.typeNames_[typeName])
            for typeItem in self.superTypes_[self.typeNames_[typeName][0]].dataTypes():
                if typeItem.value() == typeName:
                    return typeItem

