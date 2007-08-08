
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

class SerializationException(exceptions.GensrcException):
    """Exceptions encountered when generating the source code
    for serialization."""

class InvalidClassException(SerializationException):

    CLASS_NAME_ERROR = """
Error processing application XML data -
The class name %(className)s is invalid"""

    def __init__(self, className):
        self.value_ = InvalidClassException.CLASS_NAME_ERROR % {
            'className' : className }

