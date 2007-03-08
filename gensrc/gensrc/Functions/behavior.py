
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

import sys
from gensrc.Utilities import common
from gensrc.Utilities import buffer
from gensrc.Configuration import environment

class BehaviorMemberNormal(object):
    """Generate source code for a Member function which does not loop."""

    funcMemberBuffer = buffer.loadBuffer('stub.func.member')

    def __init__(self, func):
        """Save a reference to the function."""
        self.func = func

    def generateBody(self, addin):
        """Generate source code for the body of the function."""
        return BehaviorMemberNormal.funcMemberBuffer % {
            'dereference' : self.func.deref,
            'libraryFunction' : self.func.libraryFunction, 
            'libraryReturnType' : addin.libraryReturnType.apply(self.func.returnValue),
            'objectID' : self.func.parameterObjectID.nameCnv,
            'parameterList' : self.func.ParameterList.generate(addin.libraryCall) }

class BehaviorProcedureNormal(object):
    """Generate source code for a Procedure function which does not loop."""

    funcProcedureBuffer = buffer.loadBuffer('stub.func.procedure')

    def __init__(self, func):
        """Save a reference to the function."""
        self.func = func

    def generateBody(self, addin):
        """Generate source code for the body of the function."""
        return BehaviorProcedureNormal.funcProcedureBuffer % {
            'alias' : self.func.alias,
            'libraryReturnType' : addin.libraryReturnType.apply(self.func.returnValue),
            'parameterList' : self.func.ParameterList.generate(addin.libraryCall) }

class BehaviorLoop(object):
    """Generate source code for a Member function which loops on one of
    its input parameters."""

    # some strings for error handling
    LOOP_ERROR = """
*********************************************************************
Error processing function %(functionName)s.
Function has been configured to loop on parameter %(loopParameterName)s.
But configuration of loop behavior failed because %(reason)s.
Please either rectify the above problem or disable looping 
for this function.
*********************************************************************"""
    REASON_NOT_FOUND = '''\
there is no input parameter with that name'''
#    REASON_LOOP_DEFAULT = '''\
#a default value has been specified for the loop parameter'''
    REASON_LOOP_NONVECTOR = '''\
the loop parameter is not a vector'''
    REASON_RETURN_NONVECTOR = '''\
the return value is not a vector'''

    def __init__(self):
        """Configure the function to loop."""

        # Save a reference to the input parameter on which the function loops.
        self.loopParamRef = None
        for param in self.func.ParameterList.Parameters:
            if param.name == self.func.loopParameter:
                self.loopParamRef = param
                break
        if not self.loopParamRef:
            self.fail(BehaviorLoop.REASON_NOT_FOUND)

        # Try to trap a few of the common problems that would prevent
        # the generated source code of the loop function from compiling.
        #if self.loopParamRef.default:
        #    self.fail(BehaviorLoop.REASON_LOOP_DEFAULT)
        if self.loopParamRef.tensorRank != common.VECTOR:
            self.fail(BehaviorLoop.REASON_LOOP_NONVECTOR)
        if self.func.returnValue.tensorRank != common.VECTOR:
            self.fail(BehaviorLoop.REASON_RETURN_NONVECTOR)

        # Configure the function to loop on the given parameter.
        self.loopParamRef.loop = True
        self.loopParamRef.default = None    # really should fail instead
        self.func.returnValue.loop = True

    def fail(self, reason):
        """Abort with an explanation."""
        errorMessage = BehaviorLoop.LOOP_ERROR % {
            'functionName' : self.func.name,
            'loopParameterName' : self.func.loopParameter,
            'reason' : reason }
        sys.exit(errorMessage)

    def generateBody(self, addin):
        """Generate source code for the body of the function."""
        return addin.bufferLoop.text % {
            'functionDisplayName' : self.func.name,
            'inputList' : self.func.ParameterList.generate(addin.loopInputs),
            'inputParam' : self.loopParamRef.name,
            'functionCodeName' : self.functionCodeName,
            'inputType' : addin.loopReturnType.apply(self.loopParamRef),
            'namespaceObjects' : environment.config().namespaceObjects,
            'objectName' : self.objectName,
            'returnType' : addin.loopReturnType.apply(self.func.returnValue) }

class BehaviorMemberLoop(BehaviorLoop):
    """Customize the BehaviorLoop class with some strings specific to Member functions."""

    # constants
    BIND_POINTER = """boost::_mfi::cmf%(cmfCount)d<
                    %(returnType)s,
                    %(functionScope)s,%(inputTypes)s>"""
    BIND_LIST = """boost::_bi::list%(listCount)d<
                    boost::_bi::value<%(functionReference)s >,%(inputTypes)s > >"""
    const = ' const'

    def __init__(self, func):
        """Save a reference to the function, and configure the function to loop."""
        self.func = func
        BehaviorLoop.__init__(self)
        self.setScope()
        self.functionCodeName = self.functionScope + '::' + self.func.libraryFunction
        self.objectName = '\n' + 16 * ' ' + self.func.parameterObjectID.nameCnv + ','

    def setScope(self):
        """Set a few properties that will be required to generate source code."""
        #self.functionScope = '%s::%s' % (
        #    environment.config().namespaceLibrary, self.func.type)
        self.functionScope = '%s::%s' % (
            environment.config().namespaceLibrary, self.func.parameterObjectID.dataType.classname)
        self.functionReference = 'boost::shared_ptr<%s>' % self.functionScope
        self.functionScope2 = self.functionScope + '::* '

    def bindPointer(self, inputTypes, returnType):
        """Return source code for a boost::bind declaration."""
        return BehaviorMemberLoop.BIND_POINTER % {
            'cmfCount' : self.func.ParameterList.underlyingCount,
            'inputTypes' : self.func.ParameterList.generate(inputTypes),
            'functionScope' : self.functionScope,
            'returnType' : returnType }

    def bindList(self, inputTypes):
        """Return source code for a boost::bi::list declaration."""
        return BehaviorMemberLoop.BIND_LIST % {
            'inputTypes' : self.func.ParameterList.generate(inputTypes),
            'listCount' : self.func.ParameterList.underlyingCount + 1,
            'functionReference' : self.functionReference }

class BehaviorEnumerationLoop(BehaviorMemberLoop):
    """Customize the BehaviorMemberLoop class with some strings specific to 
    EnumerationMember functions."""

    def setScope(self):
        """Set a few properties that will be required to generate source code."""
        self.functionScope = self.func.type
        self.functionReference = self.func.type
        self.functionScope2 = self.functionScope + '::* '

class BehaviorProcedureLoop(BehaviorLoop):
    """Customize the BehaviorLoop class with some strings specific to Procedure functions."""

    # constants
    BIND_POINTER = '%(returnType)s (__cdecl*)(%(inputTypes)s)'
    BIND_LIST = 'boost::_bi::list%(listCount)d<%(inputTypes)s > >'
    functionScope2 = '*'
    objectName = ''
    const = ''

    def __init__(self, func):
        """Save a reference to the function, and configure the function to loop."""
        self.func = func
        BehaviorLoop.__init__(self)
        self.functionCodeName = self.func.alias

    def bindPointer(self, inputTypes, returnType):
        """Return source code for a boost::bind declaration."""
        return BehaviorProcedureLoop.BIND_POINTER % {
            'inputTypes' : self.func.ParameterList.generate(inputTypes),
            'returnType' : returnType }

    def bindList(self, inputTypes):
        """Return source code for a boost::bi::list declaration."""
        return BehaviorProcedureLoop.BIND_LIST % {
            'inputTypes' : self.func.ParameterList.generate(inputTypes),
            'listCount' : self.func.ParameterList.underlyingCount }

