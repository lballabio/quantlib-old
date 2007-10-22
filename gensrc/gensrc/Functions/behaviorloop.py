
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

class BehaviorLoop(object):
    """Generate source code for a Member function which loops on one of
    its input parameters."""

    #############################################
    # public interface
    #############################################

    def generateBody(self, addin):
        """Generate source code for the body of the function."""
        if self.functionSignature_:
            functionSignature = '(' + environment.config().namespaceObjects() + '::' + self.functionSignature_ + ')'
        else:
            functionSignature = ''
        return addin.bufferLoop().text() % {
            'inputList' : self.func_.parameterList().generate(addin.loopInputs()),
            'inputParam' : addin.loopName(self.loopParamRef_),
            'functionCodeName' : self.functionCodeName_,
            'functionName' : self.functionName_,
            'functionSignature' : functionSignature,
            'inputType' : addin.loopReturnType().apply(self.loopParamRef_),
            'objectName' : self.objectName_,
            'returnType' : addin.loopReturnType().apply(self.func_.returnValue()) }

    #############################################
    # private member functions
    #############################################

    def __init__(self):
        """Configure the function to loop."""

        # Save a reference to the input parameter on which the function loops.
        self.loopParamRef_ = None
        for param in self.func_.parameterList().parameters():
            if param.name() == self.func_.loopParameter():
                self.loopParamRef_ = param
                break
        if not self.loopParamRef_:
            raise exceptions.BehaviorLoopParameterException(self.func_.name(), self.func_.loopParameter())

        # Try to trap a few of the common problems that would prevent
        # the generated source code of the loop function from compiling.
        if self.loopParamRef_.tensorRank() != common.VECTOR:
            raise exceptions.BehaviorLoopNonVectorException(self.func_.name(), self.func_.loopParameter())
        if self.func_.returnValue().tensorRank() != common.VECTOR:
            raise exceptions.BehaviorReturnNonVectorException(self.func_.name(), self.func_.loopParameter())

        # Configure the function to loop on the given parameter.
        self.loopParamRef_.setLoop(True)
        self.func_.returnValue().setLoop(True)
        self.functionName_ = environment.config().namespaceObjects() + '::' + self.func_.name()
        self.functionSignature_ = ''
        # Set the default value of the loop parameter to None, overwriting any default value that
        # may have been configured.  FIXME should raise an exception instead.
        #self.loopParamRef_.setDefault(None)
        self.loopParamRef_.setDefault('')

class BehaviorMemberLoop(BehaviorLoop):
    """Customize the BehaviorLoop class with some strings specific to Member functions."""

    #############################################
    # class variables
    #############################################

    BIND_POINTER = """boost::_mfi::%(const)smf%(cmfCount)d<
                    %(returnType)s,
                    %(functionType)s,%(inputTypes)s>"""
    BIND_LIST = """boost::_bi::list%(listCount)d<
                    boost::_bi::value<%(functionReference)s >,%(inputTypes)s > >"""

    #############################################
    # public interface
    #############################################

    def bindPointer(self, inputTypes, returnType):
        """Return source code for a boost::bind declaration."""
        if self.func_.const():
            const = 'c'
        else:
            const = ''
        return BehaviorMemberLoop.BIND_POINTER % {
            'cmfCount' : self.func_.parameterList().underlyingCount(),
            'const' : const,
            'inputTypes' : self.func_.parameterList().generate(inputTypes),
            'functionType' : self.func_.type(),
            'returnType' : returnType }

    def bindList(self, inputTypes):
        """Return source code for a boost::bi::list declaration."""
        return BehaviorMemberLoop.BIND_LIST % {
            'inputTypes' : self.func_.parameterList().generate(inputTypes),
            'listCount' : self.func_.parameterList().underlyingCount() + 1,
            'functionReference' : self.functionReference() }

    #############################################
    # private member functions
    #############################################

    def __init__(self, func):
        """Save a reference to the function, and configure the function to loop."""
        self.func_ = func
        BehaviorLoop.__init__(self)
        self.functionCodeName_ = self.func_.type() + '::' + self.func_.libraryFunction()
        self.functionSignature_ = self.func_.name() + 'Signature'
        self.objectName_ = '\n' + 16 * ' ' + self.func_.parameterObjectId().nameConverted() + ','

    def functionReference(self):
        return 'boost::shared_ptr<%s>' % self.func_.type()

class BehaviorEnumerationLoop(BehaviorMemberLoop):
    """Customize the BehaviorMemberLoop class with some strings specific to 
    EnumerationMember functions."""

    #############################################
    # private member functions
    #############################################

    def functionReference(self):
        return self.func_.type()

class BehaviorProcedureLoop(BehaviorLoop):
    """Customize the BehaviorLoop class with some strings specific to Procedure functions."""

    #############################################
    # class variables
    #############################################

    BIND_POINTER = '%(returnType)s (__cdecl*)(%(inputTypes)s)'
    BIND_LIST = 'boost::_bi::list%(listCount)d<%(inputTypes)s > >'
    objectName_ = ''

    #############################################
    # public interface
    #############################################

    def bindPointer(self, inputTypes, returnType):
        """Return source code for a boost::bind declaration."""
        return BehaviorProcedureLoop.BIND_POINTER % {
            'inputTypes' : self.func_.parameterList().generate(inputTypes),
            'returnType' : returnType }

    def bindList(self, inputTypes):
        """Return source code for a boost::bi::list declaration."""
        return BehaviorProcedureLoop.BIND_LIST % {
            'inputTypes' : self.func_.parameterList().generate(inputTypes),
            'listCount' : self.func_.parameterList().underlyingCount() }

    #############################################
    # private member functions
    #############################################

    def __init__(self, func):
        """Save a reference to the function, and configure the function to loop."""
        self.func_ = func
        BehaviorLoop.__init__(self)
        self.functionCodeName_ = self.func_.alias()

