
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

from gensrc.Exceptions import exceptions

class TypeException(exceptions.GensrcException):
    """Exceptions encountered when processing datatypes."""

class InvalidTypeNameException(TypeException):

    INVALID_TYPE_NAME_ERROR = """
The type name "%(typeName)s" is not defined in the "types.xml" datatype metadata file."""

    def __init__(self, typeName):
        self.value_ = InvalidTypeNameException.INVALID_TYPE_NAME_ERROR % {
            'typeName' : typeName }

class InvalidSuperTypeNameException(TypeException):

    INVALID_SUPER_TYPE_NAME_ERROR = """
The supertype name "%(superTypeName)s" is not defined in the "supertypes.xml" datatype metadata file."""

    def __init__(self, superTypeName):
        self.value_ = InvalidSuperTypeNameException.INVALID_SUPER_TYPE_NAME_ERROR % {
            'superTypeName' : superTypeName }

class InvalidNativeTypeException(TypeException):

    INVALID_NATIVE_TYPE_ERROR = """
Error processing type/supertype combination '%(typeName)s/%(superTypeName)s - neither specifies a native datatype."""

    def __init__(self, typeName, superTypeName):
        self.value_ = InvalidNativeTypeException.INVALID_NATIVE_TYPE_ERROR % {
            'superTypeName' : superTypeName,
            'typeName' : typeName }

