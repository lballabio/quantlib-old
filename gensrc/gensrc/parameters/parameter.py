
"""
 Copyright (C) 2005, 2006, 2007, 2008 Eric Ehlers
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2005 Plamen Neykov

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

"""Encapsulate state and behavior necessary to generate source code
relating to a function parameter."""

from gensrc.parameters import exceptions
from gensrc.utilities import common
from gensrc.serialization import serializable
from gensrc.configuration import environment

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
    errorValue_ = ''

    #############################################
    # public interface
    #############################################

    def tensorRank(self):
        """Return the tensor rank of this value i.e. scalar/vector/matrix."""
        return self.tensorRank_

    def loop(self):
        """Return boolean indicating whether this value is a loop parameter."""
        return self.loop_

    def vectorIterator(self):
        """Return a value specifying the iterator behavior for this vector
        parameter - begin, end, or beginAndEnd.

        begin indicates that the underlying function requires an iterator
        pointing to the beginning of the std::vector.  end indicates that the
        underlying function requires an iterator pointing to the beginning of the
        std::vector.  beginAndEnd indicates that the underlying function requires both
        values."""
        return self.vectorIterator_

    def default(self):
        """Return the default value that was specified for this parameter in the
        event that no value is available - or None."""
        return self.default_

    def ignore(self):
        """Return a boolean indicating whether this parameter is ignored by
        the underlying library function."""
        return self.ignore_

    def const(self):
        """Return a boolean indicating whether this parameter is passed as a
        const value."""
        return self.const_

    def fullType(self):
        """Return the FullType object that corresponds to this value."""
        return self.fullType_

    def nameConverted(self):
        """Return the variable name to be used for this parameter after datatype
        conversion has been performed."""
        return self.name_ + self.fullType_.conversionSuffix()

    def description(self):
        """Return the explanatory description that was provided for this parameter
        for end user documentation."""
        return self.description_

    def lastParameter(self):
        """Return a boolean indicating whether this parameter is the last
        in the list for the given function."""
        return self.lastParameter_

    def errorValue(self):
        """Return the value, if any, to be substituted for this parameter
        if the original value is erroneous."""
        return self.errorValue_

    def setLastParameter(self, val):
        """Set the boolean value indicating whether this parameter is the last
        in the list for the given function."""
        self.lastParameter_ = val

    def setLoop(self, val):
        """Set the boolean value indicating whether this parameter is configured
        as the loop parameter for the function."""
        self.loop_ = val

    def setDefault(self, val):
        """Set the default value to be used in place of this parameter in the event
        that no value is available."""
        self.default_ = val

    def printValue(self, value):
        """For debugging purposes, write the given value to stdout."""
        if value is None:
            return ''
        else:
            return str(value) + ''

    def printDebug(self):
        """For debugging purposes, write the properties of this parameter to stdout."""
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

    # Strings which are not valid as parameter names.
    # TODO add C++ keywords etc.
    ILLEGAL_NAMES = ( 'TYPE', 'NONE' )

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeProperty(self, common.TYPE)
        serializer.serializeProperty(self, common.SUPER_TYPE)
        serializer.serializeProperty(self, common.TENSOR_RANK)
        serializer.serializeProperty(self, common.DESCRIPTION)
        serializer.serializeAttributeBoolean(self, common.IGNORE)
        serializer.serializeAttributeBoolean(self, common.CONST, True)
        serializer.serializeAttribute(self, common.DEFAULT)
        serializer.serializeAttribute(self, common.ERROR_VALUE)
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
        self.fullType_ = environment.getType(self.type_, self.superType_)
        if self.default_:
            self.description_ += ' Default value = ' + self.default_.replace('"', '') + '.'

class ReturnValue(Value):
    """Encapsulate state necessary to generate source code
    relating to a function return value."""

    name_ = 'returnValue'

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeProperty(self, common.TYPE)
        serializer.serializeProperty(self, common.SUPER_TYPE)
        serializer.serializeProperty(self, common.TENSOR_RANK)

    def postSerialize(self):
        """Perform post serialization initialization."""
        self.fullType_ = environment.getType(self.type_, self.superType_)

class ConstructorReturnValue(Value):
    """Class to represent state shared by the return values
    of all constructors."""

    name_ = 'returnValue'
    tensorRank_ = common.SCALAR

    def __init__(self):
        """Initialize the ConstructorReturnValue object."""
        self.fullType_ = environment.getType(common.STRING)

class PermanentFlag(Value):
    """All ctors have a final optional boolean parameter 'permanent'"""

    name_ = 'Permanent'
    tensorRank_ = common.SCALAR
    description_ = 'object permanent/nonpermanent'
    default_ = 'false'

    def __init__(self):
        """Initialize the PermanentFlag object."""
        self.fullType_ = environment.getType(common.BOOL)

class ConstructorObjectId(Parameter):
    """ID of an object.

    Implicitly used as the first input parameter for all
    Constructors (where the ID is assigned to the new object)."""

    name_ = 'ObjectId'
    tensorRank_ = common.SCALAR
    ignore_ = False
    description_ = 'id of object to be created'

    def __init__(self):
        """Initialize the ConstructorObjectId object."""
        self.fullType_ = environment.getType(common.STRING)

class MemberObjectId(Parameter):
    """ID of an object.

    Implicitly used as the first input parameter for all
    Members (where the ID indicates the object to be retrieved)."""

    name_ = 'ObjectId'
    tensorRank_ = common.SCALAR
    ignore_ = False

    def __init__(self, typeName, superTypeName):
        """Initialize the MemberObjectId object."""
        #self.failIfEmpty = True # Member function can't be invoked on null object

        self.fullType_ = environment.getType(typeName, superTypeName)
        self.description_ = 'id of existing %s object' % self.fullType_.value()

class EnumerationId(Parameter):
    """ID of an enumeration.

    The ID of an Enumeration to be retrieved from the Registry."""

    tensorRank_ = common.SCALAR
    ignore_ = False

    def __init__(self, typeName, superTypeName):
        """Initialize the EnumerationId object."""
        self.fullType_ = environment.getType(typeName, superTypeName)
        self.name_ = self.fullType_.classname().lower()
        self.description_ = 'ID of Enumeration of class %s' % self.fullType_.value()

class DependencyTrigger(Parameter):
    """Dependency tracking trigger.

    A dummy parameter used to force dependencies between cells
    in a worksheet."""

    name_ = 'Trigger'
    tensorRank_ = common.SCALAR
    ignore_ = True
    description_ = 'dependency tracking trigger'

    def __init__(self):
        """Initialize the DependencyTrigger object."""
        self.fullType_ = environment.getType(common.ANY)

class OverwriteFlag(Parameter):
    """A flag to indicate whether new object should overwrite old."""

    name_ = 'Overwrite'
    tensorRank_ = common.SCALAR
    ignore_ = True
    description_ = 'overwrite flag'

    def __init__(self):
        """Initialize the OverwriteFlag object."""
        self.fullType_ = environment.getType(common.BOOL)

