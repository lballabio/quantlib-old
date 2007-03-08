
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

"""encapsulate state necessary to generate source code 
relating to a function parameter."""

from gensrc.Parameters import exceptions
from gensrc.Utilities import common
from gensrc.Serialization import serializable
from gensrc.Configuration import environment
import re

class Value(serializable.Serializable):
    """Represent any value which may be passed to or received from a Function."""
    const = True
    description = ''
    loop = ''
    vectorIterator = ''
    lastParameter = False
    ignore = False

    def printValue(self, value):
        if value is None:
            return ','
        else:
            return str(value) + ','

    def printDebug(self):
        print self.printValue(self.name) + \
            self.printValue(self.tensorRank) + \
            self.printValue(self.type) + \
            self.printValue(self.loop) + \
            self.printValue(self.vectorIterator) + \
            self.printValue(self.default) + \
            self.printValue(self.ignore) + \
            self.printValue(self.const)

class Parameter(Value):
    """Encapsulate state necessary to generate source code 
    relating to a function parameter."""

    groupName = 'Parameters'

    # strings which are not valid as parameter names.
    # TODO add C++ keywords etc.
    ILLEGAL_NAMES = [ 'type', 'None' ]

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeProperty(self, common.TYPE)
        serializer.serializeProperty(self, 'superType')
        serializer.serializeProperty(self, common.TENSOR_RANK)
        serializer.serializeProperty(self, common.DESCRIPTION)
        serializer.serializeAttributeBoolean(self, common.IGNORE)
        serializer.serializeAttributeBoolean(self, common.CONST, True)
        serializer.serializeAttribute(self, common.DEFAULT)
        serializer.serializeAttribute(self, common.VECTOR_ITERATOR)

    def postSerialize(self):
        """Perform post serialization initialization."""
        if Parameter.ILLEGAL_NAMES.count(self.name):
            raise exceptions.ParameterNameInvalidException(self.name)
        self.dataType = environment.getType(self.type, self.superType)

class ReturnValue(Value):
    """Encapsulate state necessary to generate source code 
    relating to a function return value."""

    name = 'returnValue'
    default = False

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeProperty(self, common.TYPE)
        serializer.serializeProperty(self, 'superType')
        serializer.serializeProperty(self, common.TENSOR_RANK)

    def postSerialize(self):
        """Perform post serialization initialization."""
        self.dataType = environment.getType(self.type, self.superType)

class ConstructorReturnValue(Value):
    """Class to represent state shared by the return values
    of all constructors in QuantLibAddin."""

    name = 'returnValue'
    tensorRank = common.SCALAR
    type = common.STRING
    superType = 'native'
    default = False

    def __init__(self):
        self.dataType = environment.getType(self.type, self.superType)

class PermanentFlag(Value):
    """All ctors have a final optional boolean parameter 'permanent'"""

    name = 'permanent'
    tensorRank = common.SCALAR
    type = common.BOOL
    default = 'false'
    ignore = True
    description = 'object permanent/nonpermanent'

    def __init__(self):
        self.dataType = environment.getType(self.type)

class ConstructorObjectID(Parameter):
    """ID of an object.

    Implicitly used as the first input parameter for all 
    Constructors (where the ID is assigned to the new object)."""

    name = 'objectID'
    tensorRank = common.SCALAR
    type = common.STRING
    superType = 'native'
    default = False
    ignore = False
    description = 'id of object to be created'

    def __init__(self):
        self.dataType = environment.getType(self.type, self.superType)

class MemberObjectID(Parameter):
    """ID of an object.

    Implicitly used as the first input parameter for all 
    Constructors (where the ID is assigned to the new object) and
    Members (where the ID indicates the object to be retrieved)."""

    name = 'objectID'
    tensorRank = common.SCALAR
    default = False
    ignore = False

    def __init__(self, typeName, superTypeName):
        #self.failIfEmpty = True # Member function can't be invoked on null object

        self.type = typeName
        self.superType = superTypeName
        self.dataType = environment.getType(typeName, superTypeName)
        self.description = 'id of existing %s object' % self.dataType.value

        # FIXME add this info to type metadata?
        if self.dataType.superType == 'objectClass' or self.dataType.superType == 'objectHandle':
            self.nameCnv = self.name + 'Obj'
        elif self.dataType.superType == 'libraryClass' or self.dataType.superType == 'handleToLib':
            self.nameCnv = self.name + 'LibObj'

class EnumerationId(Parameter):
    """ID of an enumeration.

    the ID of an Enumeration to be retrieved from the Registry."""

    tensorRank = common.SCALAR
    default = False
    ignore = False

    def __init__(self, typeName, superTypeName):
        self.type = typeName
        self.superType = superTypeName
        self.dataType = environment.getType(typeName, superTypeName)
        self.description = 'ID of Enumeration of class %s' % self.dataType.value
        self.name = re.match(r"\w*::(\w*)", self.dataType.value).group(1).lower()
        self.nameCnv = self.name + 'Enum'

class DependencyTrigger(Parameter):
    """dependency tracking trigger.

    A dummy parameter used to force dependencies between cells
    in a worksheet."""

    name = 'trigger'
    type = common.ANY
    superType = 'native'
    tensorRank = common.SCALAR
    default = False
    ignore = True
    description = 'dependency tracking trigger'

    def __init__(self):
        self.dataType = environment.getType(self.type, self.superType)

