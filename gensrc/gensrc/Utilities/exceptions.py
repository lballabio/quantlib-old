
"""
 Copyright (C) 2008 Eric Ehlers

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

class UtilitiesException(exceptions.GensrcException):
    """Exceptions encountered when performing utility functions."""

class UtilitiesSerializationException(UtilitiesException):

    SERIALIZATION_ERROR = """
Error loading object of class '%(className)s' from XML document '%(fileName)s.xml' :
%(parseError)s"""

    def __init__(self, fileName, className):
        errorClass, errorObject, traceBack = sys.exc_info()
        self.value_ = UtilitiesSerializationException.SERIALIZATION_ERROR % {
            'className' : className,
            'fileName' : fileName,
            'parseError' : str(errorObject) }

class UtilitiesSerializationListException(UtilitiesException):

    SERIALIZATION_LIST_ERROR = """
Error loading list '%(listName)s' containing items '%(itemName)s'
from XML document '%(fileName)s.xml' :
%(parseError)s"""

    def __init__(self, fileName, listName, itemName):
        errorClass, errorObject, traceBack = sys.exc_info()
        self.value_ = UtilitiesSerializationListException.SERIALIZATION_LIST_ERROR % {
            'fileName' : fileName,
            'itemName' : itemName,
            'listName' : listName,
            'parseError' : str(errorObject) }

