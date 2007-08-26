
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

"""encapsulate state necessary to generate source code 
relating to a function parameter."""

from gensrc.Parameters import exceptions
from gensrc.Utilities import common
from gensrc.Serialization import serializable
from gensrc.Configuration import environment
import re

class Value(serializable.Serializable):
    """Represent any value which may be passed to or received from a Function."""

    #############################################
    # class variables
    #############################################

    const_ = True
    description_ = ''
    loop_ = ''
    vectorIterator_ = ''
    lastParameter_ = False
    ignore_ = False
    default_ = ''
    errorValue_ = None

    #############################################
    # public interface
    #############################################

    def tensorRank(self):
        return self.tensorRank_

    def type(self):
        return self.type_

    def loop(self):
        return self.loop_

    def vectorIterator(self):
        return self.vectorIterator_

    def default(self):
        return self.default_

    def ignore(self):
        return self.ignore_

    def const(self):
        return self.const_

    def dataType(self):
        return self.dataType_

    def nameConverted(self):
        return self.name_ + self.dataType_.conversionSuffix()

    def description(self):
        return self.description_

    def lastParameter(self):
        return self.lastParameter_

    def errorValue(self):
        if self.errorValue_:
            return ", static_cast<%s>(%s)" % (self.dataType_.nativeType(), self.errorValue_)
        else:
            return ''

    def setLastParameter(self, val):
        self.lastParameter_ = val

    def setLoop(self, val):
        self.loop_ = val

    def setDefault(self, val):
        self.default_ = val

    def printValue(self, value):
        if value is None:
            return ''
        else:
            return str(value) + ''

    def printDebug(self):
        print "type=" + str(type(self))
        print "name=" + self.printValue(self.name_)
        print "tensorRank=" + self.printValue(self.tensorRank_)
        print "type=" + self.printValue(self.type_)
        print "loop=" + self.printValue(self.loop_)
        print "vecIter=" + self.printValue(self.vectorIterator_)
        print "default=" + self.printValue(self.default_)
        print "ignore=" + self.printValue(self.ignore_)
        print "const=" + self.printValue(self.const_)

class Parameter(Value):
    """Encapsulate state necessary to generate source code 
    relating to a function parameter."""

    groupName_ = 'Parameters'

    # strings which are not valid as parameter names.
    # TODO add C++ keywords etc.
    ILLEGAL_NAMES = ( 'TYPE', 'NONE' )

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
        serializer.serializeAttribute(self, 'errorValue')
        serializer.serializeAttribute(self, common.VECTOR_ITERATOR)

    def postSerialize(self):
        """Perform post serialization initialization."""
        if len(self.name_) < 1:
            raise exceptions.ParameterNameNullException()
        if not self.name_[0].isupper():
            raise exceptions.ParameterNameCapitalizationException(self.name_)
        if self.name_.upper() in Parameter.ILLEGAL_NAMES:
            raise exceptions.ParameterIllegalNameException(
                self.name_, Parameter.ILLEGAL_NAMES)
        self.dataType_ = environment.getType(self.type_, self.superType_)

class ReturnValue(Value):
    """Encapsulate state necessary to generate source code 
    relating to a function return value."""

    name_ = 'returnValue'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeProperty(self, common.TYPE)
        serializer.serializeProperty(self, 'superType')
        serializer.serializeProperty(self, common.TENSOR_RANK)

    def postSerialize(self):
        """Perform post serialization initialization."""
        self.dataType_ = environment.getType(self.type_, self.superType_)

class ConstructorReturnValue(Value):
    """Class to represent state shared by the return values
    of all constructors in QuantLibAddin."""

    name_ = 'returnValue'
    tensorRank_ = common.SCALAR
    type_ = common.STRING
    superType_ = 'native'

    def __init__(self):
        self.dataType_ = environment.getType(self.type_, self.superType_)

class PermanentFlag(Value):
    """All ctors have a final optional boolean parameter 'permanent'"""

    name_ = 'Permanent'
    tensorRank_ = common.SCALAR
    type_ = common.BOOL
    description_ = 'object permanent/nonpermanent'
    default_ = 'false'

    def __init__(self):
        self.dataType_ = environment.getType(self.type_)

class ConstructorObjectId(Parameter):
    """ID of an object.

    Implicitly used as the first input parameter for all 
    Constructors (where the ID is assigned to the new object)."""

    name_ = 'ObjectId'
    tensorRank_ = common.SCALAR
    type_ = common.STRING
    superType_ = 'native'
    ignore_ = False
    description_ = 'id of object to be created'

    def __init__(self):
        self.dataType_ = environment.getType(self.type_, self.superType_)

class MemberObjectId(Parameter):
    """ID of an object.

    Implicitly used as the first input parameter for all 
    Constructors (where the ID is assigned to the new object) and
    Members (where the ID indicates the object to be retrieved)."""

    name_ = 'ObjectId'
    tensorRank_ = common.SCALAR
    ignore_ = False

    def __init__(self, typeName, superTypeName):
        #self.failIfEmpty = True # Member function can't be invoked on null object

        self.type_ = typeName
        self.superType_ = superTypeName
        self.dataType_ = environment.getType(typeName, superTypeName)
        self.description_ = 'id of existing %s object' % self.dataType_.value()

class EnumerationId(Parameter):
    """ID of an enumeration.

    the ID of an Enumeration to be retrieved from the Registry."""

    tensorRank_ = common.SCALAR
    ignore_ = False

    def __init__(self, typeName, superTypeName):
        self.type_ = typeName
        self.superType_ = superTypeName
        self.dataType_ = environment.getType(typeName, superTypeName)
        self.description_ = 'ID of Enumeration of class %s' % self.dataType_.value()
        self.name_ = re.match(r"\w*::(\w*)", self.dataType_.value()).group(1).lower()

class DependencyTrigger(Parameter):
    """dependency tracking trigger.

    A dummy parameter used to force dependencies between cells
    in a worksheet."""

    name_ = 'Trigger'
    type_ = common.ANY
    superType_ = 'native'
    tensorRank_ = common.SCALAR
    ignore_ = True
    description_ = 'dependency tracking trigger'

    def __init__(self):
        self.dataType_ = environment.getType(self.type_, self.superType_)

