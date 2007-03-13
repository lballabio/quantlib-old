
"""
 Copyright (C) 2006, 2007 Eric Ehlers

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

from gensrc.Utilities import common
from gensrc.Parameters import parameter
from gensrc.Serialization import serializable

class ParameterList(serializable.Serializable):

    #############################################
    # class variables
    #############################################

    name_ = 'ParameterList'
    skipFirst_ = False

    #############################################
    # public interface
    #############################################

    def generate(self, ruleGroup):
        """Generate source code relating to a list of function parameters."""
        code = ''
        i = 0
        for param in self.parameters_:
            i += 1
            if ruleGroup.checkSkipFirst() and i == 1 and self.skipFirst_: continue
            paramConversion = ruleGroup.apply(param)
            if paramConversion: 
                if code:
                    code += ruleGroup.delimiter() + paramConversion
                else:
                    code = paramConversion
        if code and ruleGroup.wrapText():
            code = ruleGroup.wrapText() % code
        return code

    def prepend(self, param):
        if self.parameterCount_ == 0:
            param.setLastParameter(True)
        self.parameters_.insert(0, param)
        self.parameterCount_ += 1
        self.skipFirst_ = True

    def append(self, param):
        if self.parameterCount_:
            self.parameters_[self.parameterCount_ - 1].setLastParameter(False)
        self.parameters_.append(param)
        param.setLastParameter(True)
        self.parameterCount_ += 1

    def parameters(self):
        return self.parameters_

    def underlyingCount(self):
        return self.underlyingCount_

    def parameterCount(self):
        return self.parameterCount_

    def printDebug(self):
        for param in self.parameters_:
            param.printDebug()

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeObjectList(self, parameter.Parameter)

    def postSerialize(self):
        """Perform post serialization initialization."""

        # set some values:
        # - underlyingCount - #/params passed to underlying function i.e. 
        #   excluding 1) params with ignore = True 2) objectIDs etc
        # - lastParameter - required for rules with padLastParameter=True

        self.underlyingCount_ = 0
        i = 1
        for param in self.parameters_:
            if not param.ignore():
                self.underlyingCount_ += 1
            if i == self.parameterCount_:
                param.setLastParameter(True)
            i += 1

