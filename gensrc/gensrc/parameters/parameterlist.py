
"""
 Copyright (C) 2006, 2007 Eric Ehlers

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

"""Encapsulate state necessary to generate source code 
relating to a function parameter."""

from gensrc.utilities import common
from gensrc.parameters import parameter
from gensrc.parameters import exceptions
from gensrc.serialization import serializable

class ParameterList(serializable.Serializable):
    """The list of Parameter objects that relate to a given
    Function object."""

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
        codeItems = []
        firstItem = True
        for param in self.parameters_:
            if firstItem:
                firstItem = False
                if ruleGroup.checkSkipFirst() and self.skipFirst_: continue
            ruleResult = ruleGroup.apply(param)
            if ruleResult:
                codeItems.append(ruleResult)
        # FIXME the logic below should be moved into the RuleGroup class?
        code = ruleGroup.delimiter().join(codeItems)
        if code and ruleGroup.wrapText():
            code = ruleGroup.wrapText() % code
        return code

    def prepend(self, param):
        """Prepend a parameter to the list."""
        if self.parameterCount_ == 0:
            param.setLastParameter(True)
        self.parameters_.insert(0, param)
        self.parameterCount_ += 1
        self.skipFirst_ = True

    def append(self, param):
        """Append a parameter to the list."""
        if self.parameterCount_:
            self.parameters_[self.parameterCount_ - 1].setLastParameter(False)
        self.parameters_.append(param)
        param.setLastParameter(True)
        self.parameterCount_ += 1

    def parameters(self):
        """Return the list of parameters."""
        return self.parameters_

    def underlyingCount(self):
        """Return the number of parameters that will actually
        be passed to the underlying Addin function.

        This count excludes parameters with the "ignore" flag set to true,
        e.g. object IDs."""
        return self.underlyingCount_

    def parameterCount(self):
        """Return the number of parameters in the list."""
        return self.parameterCount_

    def printDebug(self):
        """Print debug information to stdout."""
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

        # Derive some values:
        # - underlyingCount - The number of parameters that will actually
        # be passed to the underlying Addin function.  This excludes 
        # parameters with the "ignore" flag set to true, e.g. object IDs.
        # - lastParameter - A boolean which must be set to true for the
        # last parameter in the list.  The value is used for Rules 
        # where padLastParameter=True

        # Also ensure that all parameter IDs are unique.

        alreadySeen = []
        self.underlyingCount_ = 0
        i = 1
        for param in self.parameters_:
            if param.name().upper() in alreadySeen:
                raise exceptions.ParameterDuplicateNameException(
                    param.name())
            alreadySeen.append(param.name().upper())
            if not param.ignore():
                self.underlyingCount_ += 1
            if i == self.parameterCount_:
                param.setLastParameter(True)
            i += 1

