
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

from gensrc.exceptions import exceptions

class ExcelException(exceptions.GensrcException):
    """exceptions encountered when generating the source code
    for the Excel addin."""

class ExcelParameterLengthException(ExcelException):

    PARAM_ERROR = """
Error processing function %(functionName)s -
The comma-delimited list of parameter names is invalid:

%(parameterNames)s

This string has a length of %(len)d which exceeds the max of %(max)d
allowed by the Excel function wizard.
Please shorten the names of the parameters."""

    def __init__(self, functionName, parameterNames, max):
        self.value_ = ExcelParameterLengthException.PARAM_ERROR % {
            'functionName' : functionName,
            'parameterNames' : parameterNames,
            'len' : len(parameterNames),
            'max' : max }

class ExcelParameterCountException(ExcelException):

    MAXPARAMERR = '''\
Error processing function "%(functionName)s" -
This function is defined with %(parameterCount)d arguments
which exceeds the Excel maximum of %(parameterMax)d arguments.'''

    def __init__(self, functionName, parameterCount, parameterMax):
        self.value_ = ExcelParameterCountException.MAXPARAMERR % {
            'functionName' : functionName,
            'parameterCount' : parameterCount,
            'parameterMax' : parameterMax }

class ExcelCellNameException(ExcelException):

    CELL_NAME_ERROR = '''
Error processing function %(functionName)s -
The function name "%(functionName)s" is invalid, because
it conflicts with an Excel cell range name.

Excel 2007, cells are named from A1 to %(maxColumnId)s%(maxRowNumber)s
and these identifiers may not be used as function names.'''

    def __init__(self, functionName, maxColumnId, maxRowNumber):
        self.value_ = ExcelCellNameException.CELL_NAME_ERROR % {
            'functionName' : functionName,
            'maxColumnId' : maxColumnId,
            'maxRowNumber' : maxRowNumber }

class ExcelStringLengthException(ExcelException):

    MAX_LEN_ERROR = '''
The following string is invalid:
"%(stringValue)s"
This string has a length of %(stringLength)d bytes
which exceeds the Excel maximum string length of %(maxStringLength)d bytes.'''

    def __init__(self, stringValue, stringLength, maxStringLength):
        self.value_ = ExcelStringLengthException.MAX_LEN_ERROR % {
            'stringValue' : stringValue,
            'stringLength' : stringLength,
            'maxStringLength' : maxStringLength }

