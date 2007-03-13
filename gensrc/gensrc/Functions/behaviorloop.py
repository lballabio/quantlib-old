
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

class BehaviorLoop(object):
    """Generate source code for a Member function which loops on one of
    its input parameters."""

    #############################################
    # public interface
    #############################################

    def generateBody(self, addin):
        """Generate source code for the body of the function."""
        return addin.bufferLoop().text() % {
            'functionDisplayName' : self.func_.name(),
            'inputList' : self.func_.parameterList().generate(addin.loopInputs()),
            'inputParam' : self.loopParamRef_.name(),
            'functionCodeName' : self.functionCodeName_,
            'inputType' : addin.loopReturnType().apply(self.loopParamRef_),
            'namespaceObjects' : environment.config().namespaceObjects(),
            'objectName' : self.objectName_,
            'returnType' : addin.loopReturnType().apply(self.func_.returnValue()) }

    def functionScope2(self):
        return self.functionScope2_

    def const(self):
        return self.const_

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
        # Set the default value of the loop parameter to None, overwriting any default value that
        # may have been configured.  FIXME should raise an exception instead.
        #self.loopParamRef_.setDefault(None)
        self.loopParamRef_.setDefault('')

class BehaviorMemberLoop(BehaviorLoop):
    """Customize the BehaviorLoop class with some strings specific to Member functions."""

    #############################################
    # class variables
    #############################################

    BIND_POINTER = """boost::_mfi::cmf%(cmfCount)d<
                    %(returnType)s,
                    %(functionScope)s,%(inputTypes)s>"""
    BIND_LIST = """boost::_bi::list%(listCount)d<
                    boost::_bi::value<%(functionReference)s >,%(inputTypes)s > >"""
    const_ = ' const'

    #############################################
    # public interface
    #############################################

    def setScope(self):
        """Set a few properties that will be required to generate source code."""
        #self.functionScope = '%s::%s' % (
        #    environment.config().namespaceLibrary, self.func_.type)
        self.functionScope_ = '%s::%s' % (
            environment.config().namespaceLibrary(), self.func_.parameterObjectID().dataType().classname())
        self.functionReference_ = 'boost::shared_ptr<%s>' % self.functionScope_
        self.functionScope2_ = self.functionScope_ + '::* '

    def bindPointer(self, inputTypes, returnType):
        """Return source code for a boost::bind declaration."""
        return BehaviorMemberLoop.BIND_POINTER % {
            'cmfCount' : self.func_.parameterList().underlyingCount(),
            'inputTypes' : self.func_.parameterList().generate(inputTypes),
            'functionScope' : self.functionScope_,
            'returnType' : returnType }

    def bindList(self, inputTypes):
        """Return source code for a boost::bi::list declaration."""
        return BehaviorMemberLoop.BIND_LIST % {
            'inputTypes' : self.func_.parameterList().generate(inputTypes),
            'listCount' : self.func_.parameterList().underlyingCount() + 1,
            'functionReference' : self.functionReference_ }

    #############################################
    # private member functions
    #############################################

    def __init__(self, func):
        """Save a reference to the function, and configure the function to loop."""
        self.func_ = func
        BehaviorLoop.__init__(self)
        self.setScope()
        self.functionCodeName_ = self.functionScope_ + '::' + self.func_.libraryFunction()
        self.objectName_ = '\n' + 16 * ' ' + self.func_.parameterObjectID().nameCnv() + ','

class BehaviorEnumerationLoop(BehaviorMemberLoop):
    """Customize the BehaviorMemberLoop class with some strings specific to 
    EnumerationMember functions."""

    #############################################
    # public interface
    #############################################

    def setScope(self):
        """Set a few properties that will be required to generate source code."""
        self.functionScope_ = self.func_.type()
        self.functionReference_ = self.func_.type()
        self.functionScope2_ = self.functionScope_ + '::* '

class BehaviorProcedureLoop(BehaviorLoop):
    """Customize the BehaviorLoop class with some strings specific to Procedure functions."""

    #############################################
    # class variables
    #############################################

    BIND_POINTER = '%(returnType)s (__cdecl*)(%(inputTypes)s)'
    BIND_LIST = 'boost::_bi::list%(listCount)d<%(inputTypes)s > >'
    functionScope2_ = '*'
    objectName_ = ''
    const_ = ''

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

