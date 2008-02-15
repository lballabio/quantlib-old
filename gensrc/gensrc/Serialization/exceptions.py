
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
import sys

class SerializationException(exceptions.GensrcException):
    """Exceptions encountered when performing serialization."""

class SerializationOverrideException(SerializationException):

    OVERRIDE_ERROR = """
Method %(method)s must be overridden in classes derived from %(parent)s."""

    def __init__(self, parent, method):
        self.value_ = OVERRIDE_ERROR % {
            'parent' : parent,
            'method' : method }

class SerializationParseException(SerializationException):

    PARSE_ERROR = """
Error loading XML document '%(xmlDocumentName)s' :
%(parseError)s"""

    def __init__(self, xmlDocumentName):
        errorClass, errorObject, traceBack = sys.exc_info()
        self.value_ = SerializationParseException.PARSE_ERROR % {
            'xmlDocumentName' : xmlDocumentName,
            'parseError' : str(errorObject) }

class SerializationDictException(SerializationException):

    EMPTY_DICT_ERROR = """
Error loading XML document '%(xmlDocumentName)s' :
dict element "%(dictElementName)s" is empty."""

    def __init__(self, xmlDocumentName, dictElementName):
        self.value_ = SerializationDictException.EMPTY_DICT_ERROR % {
            'xmlDocumentName' : xmlDocumentName,
            'dictElementName' : dictElementName }

class SerializationElementMissingException(SerializationException):

    ELEMENT_MISSING_ERROR = '''
Error loading XML document '%(xmlDocumentName)s' :
No element with name "%(elementName)s"'''

    def __init__(self, xmlDocumentName, elementName):
        self.value_ = SerializationElementMissingException.ELEMENT_MISSING_ERROR % {
            'xmlDocumentName' : xmlDocumentName,
            'elementName' : elementName }

class SerializationConvertBooleanException(SerializationException):

    CONVERT_BOOLEAN_ERROR = '''
Error loading XML document '%(xmlDocumentName)s' :
Unable to convert string "%(stringValue)s" to boolean.'''

    def __init__(self, xmlDocumentName, stringValue):
        self.value_ = SerializationConvertBooleanException.CONVERT_BOOLEAN_ERROR % {
            'xmlDocumentName' : xmlDocumentName,
            'stringValue' : stringValue }

class SerializationCreatorException(SerializationException):

    UNDEFINED_CREATOR_ERROR = '''
no creator function found for class "%(className)s"'''

    def __init__(self, className):
        self.value_ = SerializationCreatorException.UNDEFINED_CREATOR_ERROR % {
            'className' : className }

class SerializationDuplicateKeyException(SerializationException):

    DUPLICATE_KEY_ERROR = '''
Error loading XML document '%(xmlDocumentName)s' :
Error deserializing element with name "%(keyName)s" - duplicate key'''

    def __init__(self, xmlDocumentName, keyName):
        self.value_ = DuplicateKeyException.DUPLICATE_KEY_ERROR % {
            'xmlDocumentName' : xmlDocumentName,
            'keyName' : keyName }

