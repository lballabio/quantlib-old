
"""
 Copyright (C) 2007 Eric Ehlers

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

from gensrc.Exceptions import exceptions

class FunctionException(exceptions.GensrcException):
    """Exceptions encountered when processing functions."""

class BehaviorLoopParameterException(FunctionException):

    BEHAVIOR_LOOP_ERROR = """
Error processing function %(functionName)s -
Function has been configured to loop on parameter "%(loopParameterName)s"
but there is no input parameter with that name."""

    def __init__(self, functionName, loopParameterName):
        self.value = BehaviorLoopParameterException.BEHAVIOR_LOOP_ERROR % {
            'functionName' : functionName,
            'loopParameterName' : loopParameterName }

class BehaviorLoopNonVectorException(FunctionException):

    BEHAVIOR_LOOP_ERROR = """
Error processing function %(functionName)s -
Function has been configured to loop on parameter "%(loopParameterName)s"
but that parameter is not a vector."""

    def __init__(self, functionName, loopParameterName):
        self.value = BehaviorLoopNonVectorException.BEHAVIOR_LOOP_ERROR % {
            'functionName' : functionName,
            'loopParameterName' : loopParameterName }

class BehaviorReturnNonVectorException(FunctionException):

    BEHAVIOR_LOOP_ERROR = """
Error processing function %(functionName)s -
Function has been configured to loop,
but the return value is not a vector."""

    def __init__(self, functionName, loopParameterName):
        self.value = BehaviorReturnNonVectorException.BEHAVIOR_LOOP_ERROR % {
            'functionName' : functionName }

