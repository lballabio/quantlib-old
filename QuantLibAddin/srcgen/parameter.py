
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

"""encapsulate state necessary to generate source code 
relating to a function parameter."""

import common
import serializable

class Value(serializable.Serializable):
    """Represent any value which may be passed to or received from a Function.

    This base class is presently empty and is included to clarify the class 
    hierarchy."""
    pass

class Parameter(Value):
    """Encapsulate state necessary to generate source code 
    relating to a function parameter."""

    groupName = 'Parameters'

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeProperty(self, common.TYPE)
        serializer.serializeProperty(self, common.TENSOR_RANK)
        serializer.serializeProperty(self, common.DESCRIPTION)
        serializer.serializeAttributeBoolean(self, common.IGNORE)
        serializer.serializeAttribute(self, common.DEFAULT)
        serializer.serializeAttribute(self, common.LIBRARY_CLASS)
        serializer.serializeAttribute(self, common.QL_TYPE)

    def postSerialize(self):
        """Determine whether the datatype of this parameter requires a conversion."""
        if self.ignore or (self.tensorRank == common.SCALAR
        and self.type != common.ANY and not self.default):
            self.needsConversion = False
        else:
            self.needsConversion = True

class ReturnValue(Value):
    """Encapsulate state necessary to generate source code 
    relating to a function return value."""

    # sometimes a ReturnValue will be treated like a Parameter
    # in which case the properties below require default value
    name = ''
    default = False

    def key(self):
        """return unique identifier for this object."""
        return 'returnValue'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeProperty(self, common.TYPE)
        serializer.serializeProperty(self, common.TENSOR_RANK)
        serializer.serializeProperty(self, common.DESCRIPTION)
        serializer.serializeAttribute(self, common.RETURN_FUNC)
        serializer.serializeAttribute(self, common.CNV_STR)

    def returnFunction(self):
        if self.return_func:
            return "." + self.return_func + "()"
        else:
            return ""

    def conversion(self):
        if self.conversionString:
            return self.conversionString % 'returnValue'
        else:
            return 'returnValue'

class ConstructorReturnValue(Value):
    """Class to represent state shared by the return values
    of all constructors in QuantLibAddin."""

    name = ''
    type = 'string'
    tensorRank = 'scalar'
    description = 'instance name of newly created object'
    default = False

class ParameterHandle(Parameter):
    """Handle of an object.

    Implicitly used as the first input parameter for all 
    Constructors (where the handle is assigned to the new object) and
    Members (where the handle indicates the object to be retrieved)."""

    name = 'instanceName'
    type = 'string'
    tensorRank = 'scalar'
    default = False
    needsConversion = False
    ignore = False

    def __init__(self, description):
        self.description = description

