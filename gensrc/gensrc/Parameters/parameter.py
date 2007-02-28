
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

from gensrc.Utilities import common
from gensrc.Serialization import serializable
import re
import sys

class Value(serializable.Serializable):
    """Represent any value which may be passed to or received from a Function."""
    const = True
    description = ''
    enumeration = ''
    handleToLib = ''
    handleToLib2 = ''
    libraryClass = ''
    libraryType = ''
    libToHandle = ''
    loop = ''
    objectClass = ''
    underlyingClass = ''
    underlyingClassNonconst = ''
    vectorIterator = ''
    lastParameter = False

    ignore = False

    def printValue(self, value):
        if value is None:
            return 'None,'
        else:
            return str(value) + ','

    def printDebug(self):
        print self.printValue(self.name) + \
            self.printValue(self.tensorRank) + \
            self.printValue(self.type) + \
            self.printValue(self.handleToLib) + \
            self.printValue(self.enumeration) + \
            self.printValue(self.handleToLib2) + \
            self.printValue(self.libraryClass) + \
            self.printValue(self.libraryType) + \
            self.printValue(self.libToHandle) + \
            self.printValue(self.loop) + \
            self.printValue(self.objectClass) + \
            self.printValue(self.underlyingClass) + \
            self.printValue(self.underlyingClassNonconst) + \
            self.printValue(self.vectorIterator) + \
            self.printValue(self.default) + \
            self.printValue(self.ignore) + \
            self.printValue(self.const)

class Parameter(Value):
    """Encapsulate state necessary to generate source code 
    relating to a function parameter."""

    groupName = 'Parameters'
    ILLEGAL_NAMES = [ 'type', 'None' ]

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeProperty(self, common.TYPE)
        serializer.serializeProperty(self, common.TENSOR_RANK)
        serializer.serializeProperty(self, common.DESCRIPTION)
        serializer.serializeAttributeBoolean(self, common.IGNORE)
        serializer.serializeAttributeBoolean(self, common.CONST, True)
        serializer.serializeAttribute(self, common.DEFAULT)
        serializer.serializeAttribute(self, common.LIBRARY_TYPE)
        serializer.serializeAttribute(self, common.ENUM)
        serializer.serializeAttribute(self, common.OBJECT_CLASS)
        serializer.serializeAttribute(self, common.LIBRARY_CLASS)
        serializer.serializeAttribute(self, common.LIB_TO_HANDLE)
        serializer.serializeAttribute(self, common.HANDLE_TO_LIB)
        serializer.serializeAttribute(self, common.HANDLE_TO_LIB2)
        serializer.serializeAttribute(self, common.UNDERLYING_CLASS)
        serializer.serializeAttribute(self, common.UNDERLYING_CLASS_NONCONST)
        serializer.serializeAttribute(self, common.VECTOR_ITERATOR)

    def postSerialize(self):
        """Perform post serialization initialization."""
        if Parameter.ILLEGAL_NAMES.count(self.name):
            sys.exit('illegal parameter name: ' + self.name)
        if not self.handleToLib2:
            self.handleToLib2 = self.handleToLib

class ReturnValue(Value):
    """Encapsulate state necessary to generate source code 
    relating to a function return value."""

    name = 'returnValue'
    default = False

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeProperty(self, common.TYPE)
        serializer.serializeProperty(self, common.TENSOR_RANK)
        serializer.serializeAttribute(self, common.LIBRARY_TYPE)
        serializer.serializeAttribute(self, common.ENUM)

class ConstructorReturnValue(Value):
    """Class to represent state shared by the return values
    of all constructors in QuantLibAddin."""

    name = 'returnValue'
    type = common.STRING
    tensorRank = common.SCALAR
    default = False

class PermanentFlag(Value):
    """All ctors have a final optional boolean parameter 'permanent'"""

    name = 'permanent'
    type = common.BOOL
    tensorRank = common.SCALAR
    description = 'object permanent/nonpermanent'
    default = 'false'
    ignore = True

class ParameterObjectID(Parameter):
    """ID of an object.

    Implicitly used as the first input parameter for all 
    Constructors (where the ID is assigned to the new object) and
    Members (where the ID indicates the object to be retrieved)."""

    name = 'objectID'
    type = common.STRING
    tensorRank = common.SCALAR
    default = False
    ignore = False

    def __init__(self, 
            objectClass = '', 
            libraryClass = '', 
            handleToLib = '',
            handleToLib2 = ''):
        #self.failIfEmpty = True # Member function can't be invoked on null object
        # FIXME need a cleaner way to do this
        if objectClass:
            self.description = 'id of existing ObjectHandler %s object' % objectClass
            self.objectClass = objectClass
            self.nameCnv = self.name + 'Obj'
        elif libraryClass:
            self.description = 'id of existing QuantLib %s object' % libraryClass
            self.libraryClass = libraryClass
            self.nameCnv = self.name + 'LibObj'
        elif handleToLib:
            self.description = 'id of existing QuantLib %s object' % handleToLib
            self.handleToLib = handleToLib
            self.handleToLib2 = handleToLib2
            self.nameCnv = self.name + 'LibObj'
        else:
            self.description = 'id of object to be created'

class EnumerationId(Parameter):
    """ID of an enumeration.

    the ID of an Enumeration to be retrieved from the Registry."""

    type = common.STRING
    tensorRank = common.SCALAR
    default = False
    ignore = False

    def __init__(self, enumeration):
        self.description = 'ID of Enumeration of class %s' % enumeration
        self.enumeration = enumeration
        self.name = re.match(r"\w*::(\w*)", enumeration).group(1).lower()
        self.nameCnv = self.name + 'Enum'

class DependencyTrigger(Parameter):
    """dependency tracking trigger.

    A dummy parameter used to force dependencies between cells
    in a worksheet."""
    type = common.ANY
    tensorRank = common.SCALAR
    default = False
    ignore = True
    description = 'dependency tracking trigger'
    name = 'trigger'

