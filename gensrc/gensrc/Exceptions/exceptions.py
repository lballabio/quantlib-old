
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

import sys
import re

class GensrcException(Exception):

    def __str__(self):
        """Stringify this exception.  This application runs in a makefile
        project under visual studio, which truncates empty lines, we prevent
        this by prefixing a > to each line."""
        return re.sub('(?m)^', '> ', self.value)

class GensrcUsageException(GensrcException):

    USAGE_ERROR = """
usage: %(scriptName)s -[targets]
    where [targets] is any of:
        q - generate source for QuantLibAddin
        e - generate source for Excel addin
        o - generate source for OpenOffice.org Calc addin
        c - generate source for C addin
        g - generate source for Guile addin
        d - generate doxygen documentation files
        v - generate ValueObjects
        l - generate loop typedefs
    or
        a - all of the above
    or
        h - display this help message"""

    def __init__(self):
        self.value = GensrcUsageException.USAGE_ERROR % {
            'scriptName' : sys.argv[0] }

