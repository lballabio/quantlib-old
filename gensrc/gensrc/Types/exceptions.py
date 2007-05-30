
"""
 Copyright (C) 2007 Eric Ehlers

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

class TypeNameUndefinedException(TypeException):

    INVALID_NAME_ERROR = """
The type name "%(typeName)s" is not defined in the "types.xml" datatype metadata file."""

    def __init__(self, typeName):
        self.value_ = TypeNameUndefinedException.INVALID_NAME_ERROR % {
            'typeName' : typeName }

class TypeSuperUndefinedException(TypeException):

    SUPERTYPE_UNDEFINED_ERROR = """
Datatype "%(typeName)s" doesn't appear in the definition
of supertype "%(superTypeName)s" in the "types.xml" datatype metadata file."""

    def __init__(self, typeName, superTypeName):
        self.value_ = TypeSuperUndefinedException.SUPERTYPE_UNDEFINED_ERROR % {
            'typeName' : typeName,
            'superTypeName' : superTypeName }

class SupertypeNameUndefinedException(TypeException):

    INVALID_SUPERTYPE_NAME_ERROR = """
The supertype name "%(superTypeName)s" is not defined in the "types.xml" datatype metadata file."""

    def __init__(self, superTypeName):
        self.value_ = SupertypeNameUndefinedException.INVALID_SUPERTYPE_NAME_ERROR % {
            'superTypeName' : superTypeName }

class TypeNameAmbiguousException(TypeException):

    AMBIGUOUS_NAME_ERROR = """
The datatype name "%(typeName)s" is ambiguous.
This type is defined in the "types.xml" datatype metadata file
under the following supertypes:
    %(superTypeNames)s
Please specify which of these supertypes is required."""

    def __init__(self, typeName, superTypeNames):
        self.value_ = TypeNameAmbiguousException.AMBIGUOUS_NAME_ERROR % {
            'typeName' : typeName,
            'superTypeNames' : ', '.join(superTypeNames) }

