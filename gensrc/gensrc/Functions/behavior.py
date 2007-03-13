
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

"""Class to generate the appropriate source code for a function depending on
its behavior.

At present two behaviors are supported:  'Normal' and 'Loop'. 

Loop behavior means that one of the Addin function's inputs is a vector,
and the Addin function loops on this vector, calling the underlying library 
function once for each iteration of the vector.  The results are saved in
a vector which is the return value of the Addin function. """

from gensrc.Functions import exceptions
from gensrc.Utilities import common
from gensrc.Utilities import buffer
from gensrc.Configuration import environment

class BehaviorMember(object):
    """Generate source code for a Member function which does not loop."""

    #############################################
    # class variables
    #############################################

    funcMemberBuffer = buffer.loadBuffer('stub.func.member')

    #############################################
    # public interface
    #############################################

    def generateBody(self, addin):
        """Generate source code for the body of the function."""
        return BehaviorMember.funcMemberBuffer % {
            'dereference' : self.func_.deref(),
            'libraryFunction' : self.func_.libraryFunction(),
            'libraryReturnType' : addin.libraryReturnType().apply(self.func_.returnValue()),
            'objectID' : self.func_.parameterObjectID().nameConverted(),
            'parameterList' : self.func_.parameterList().generate(addin.libraryCall()) }

    #############################################
    # private member functions
    #############################################

    def __init__(self, func):
        """Save a reference to the function."""
        self.func_ = func

class BehaviorProcedure(object):
    """Generate source code for a Procedure function which does not loop."""

    #############################################
    # class variables
    #############################################

    funcProcedureBuffer = buffer.loadBuffer('stub.func.procedure')

    #############################################
    # public interface
    #############################################

    def generateBody(self, addin):
        """Generate source code for the body of the function."""
        return BehaviorProcedure.funcProcedureBuffer % {
            'alias' : self.func_.alias(),
            'libraryReturnType' : addin.libraryReturnType().apply(self.func_.returnValue()),
            'parameterList' : self.func_.parameterList().generate(addin.libraryCall()) }

    #############################################
    # private member functions
    #############################################

    def __init__(self, func):
        """Save a reference to the function."""
        self.func_ = func

