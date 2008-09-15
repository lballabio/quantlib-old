
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

"""Exceptions encountered when performing serialization."""

from gensrc.exceptions import exceptions
import sys

class SerializationException(exceptions.GensrcException):
    """Exceptions encountered when performing serialization."""

class SerializationOverrideException(SerializationException):
    """Illegal use of a base member function."""

    OVERRIDE_ERROR = """
Method %(method)s must be overridden in classes derived from %(parent)s."""

    def __init__(self, parent, method):
        """Initialize the SerializationOverrideException object."""
        self.value_ = OVERRIDE_ERROR % {
            'parent' : parent,
            'method' : method }

class SerializationParseException(SerializationException):
    """Error parsing an XML document."""

    PARSE_ERROR = """
Error loading XML document '%(xmlDocumentName)s' :
%(parseError)s"""

    def __init__(self, xmlDocumentName):
        """Initialize the SerializationParseException object."""
        errorClass, errorObject, traceBack = sys.exc_info()
        self.value_ = SerializationParseException.PARSE_ERROR % {
            'xmlDocumentName' : xmlDocumentName,
            'parseError' : str(errorObject) }

class SerializationDictException(SerializationException):
    """Error populating dict from XML data - data required but not found."""

    EMPTY_DICT_ERROR = """
Error loading XML document '%(xmlDocumentName)s' :
dict element "%(dictElementName)s" is empty."""

    def __init__(self, xmlDocumentName, dictElementName):
        """Initialize the SerializationDictException object."""
        self.value_ = SerializationDictException.EMPTY_DICT_ERROR % {
            'xmlDocumentName' : xmlDocumentName,
            'dictElementName' : dictElementName }

class SerializationElementMissingException(SerializationException):
    """XML document missing a required node."""

    ELEMENT_MISSING_ERROR = '''
Error loading XML document '%(xmlDocumentName)s' :
No element with name "%(elementName)s"'''

    def __init__(self, xmlDocumentName, elementName):
        """Initialize the SerializationElementMissingException object."""
        self.value_ = SerializationElementMissingException.ELEMENT_MISSING_ERROR % {
            'xmlDocumentName' : xmlDocumentName,
            'elementName' : elementName }

class SerializationConvertBooleanException(SerializationException):
    """Error reading a value from XML and converting it to type boolean."""

    CONVERT_BOOLEAN_ERROR = '''
Error loading XML document '%(xmlDocumentName)s' :
Unable to convert string "%(stringValue)s" to boolean.'''

    def __init__(self, xmlDocumentName, stringValue):
        """Initialize the SerializationConvertBooleanException object."""
        self.value_ = SerializationConvertBooleanException.CONVERT_BOOLEAN_ERROR % {
            'xmlDocumentName' : xmlDocumentName,
            'stringValue' : stringValue }

class SerializationCreatorException(SerializationException):
    """XML document contains a class for which no creator has been implemented
    in the gensrc object factory."""

    UNDEFINED_CREATOR_ERROR = '''
no creator function found for class "%(className)s"'''

    def __init__(self, className):
        """Initialize the SerializationCreatorException object."""
        self.value_ = SerializationCreatorException.UNDEFINED_CREATOR_ERROR % {
            'className' : className }

class SerializationDuplicateKeyException(SerializationException):
    """XML document contains duplicate tag where none is expected."""

    DUPLICATE_KEY_ERROR = '''
Error loading XML document '%(xmlDocumentName)s' :
Error deserializing element with name "%(keyName)s" - duplicate key'''

    def __init__(self, xmlDocumentName, keyName):
        """Initialize the SerializationDuplicateKeyException object."""
        self.value_ = DuplicateKeyException.DUPLICATE_KEY_ERROR % {
            'xmlDocumentName' : xmlDocumentName,
            'keyName' : keyName }

