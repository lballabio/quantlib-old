
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

class ParameterException(exceptions.GensrcException):
    """Exceptions encountered when processing datatypes."""

class ParameterIllegalNameException(ParameterException):

    ILLEGAL_NAME_ERROR = """
Error processing parameter name "%(parameterName)s":
This string is reserved and cannot be used parameter name.
Below is the list of (case insensitive) strings
which cannot be used as parameter names:
    %(illegalNameList)s"""

    def __init__(self, parameterName, illegalNameList):
        self.value_ = ParameterIllegalNameException.ILLEGAL_NAME_ERROR % {
            'illegalNameList' : illegalNameList,
            'parameterName' : parameterName }

class ParameterNameNullException(ParameterException):

    NULL_STRING_ERROR = """
Null string provided for parameter name."""

    def __init__(self):
        self.value_ = ParameterNameNullException.NULL_STRING_ERROR

class ParameterNameCapitalizationException(ParameterException):

    INVALID_CAPITALIZATION_ERROR = """
The string "%(parameterName)s" is not a valid parameter name -
All parameter names must begin with an uppercase letter."""

    def __init__(self, parameterName):
        self.value_ = ParameterNameCapitalizationException.INVALID_CAPITALIZATION_ERROR % {
            'parameterName' : parameterName }

class ParameterDuplicateNameException(ParameterException):

    DUPLICATE_NAME_ERROR = """
Error processing parameter name "%(parameterName)s":
This string has been specified for two different parameters
within the same function. (The test is case insensitive)."""

    def __init__(self, parameterName):
        self.value_ = ParameterDuplicateNameException.DUPLICATE_NAME_ERROR % {
            'parameterName' : parameterName }

