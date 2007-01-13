
"""
 Copyright (C) 2006 Eric Ehlers

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

"""encapsulate state necessary to generate source code 
relating to a function parameter."""

import common
import config
import parameter
import serializable

class ParameterList(serializable.Serializable):
    name = 'ParameterList'
    skipFirst = False

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeObjectList(self, parameter.Parameter)

    def postSerialize(self):
        """Perform post serialization initialization."""

        # set some values:
        # - underlyingCount - #/params passed to underlying function i.e. 
        #   excluding 1) params with ignore = True 2) objectIDs etc
        # - lastParameter - required for rules with padLastParameter=True

        self.underlyingCount = 0
        i = 1
        for param in self.Parameters:
            if not param.ignore:
                self.underlyingCount += 1
            if i == self.ParameterCount:
                param.lastParameter = True
            i += 1

    def generate(self, ruleGroup):
        """Generate source code relating to a list of function parameters."""
        code = ''
        i = 0
        for param in self.Parameters:
            i += 1
            if ruleGroup.checkSkipFirst and i == 1 and self.skipFirst: continue
            paramConversion = ruleGroup.apply(param)
            if paramConversion: 
                if code:
                    code += ruleGroup.delimiter + paramConversion
                else:
                    code = paramConversion
        if code and ruleGroup.wrapText:
            code = ruleGroup.wrapText % code
        return code

    def prepend(self, param):
        if self.ParameterCount == 0:
            param.lastParameter = True
        self.Parameters.insert(0, param)
        self.ParameterCount += 1
        self.skipFirst = True

    def append(self, param):
        if self.ParameterCount:
            self.Parameters[self.ParameterCount - 1].lastParameter = False
        self.Parameters.append(param)
        param.lastParameter = True
        self.ParameterCount += 1

