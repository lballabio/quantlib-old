
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

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

"""Encapsulate state and behavior required to generate source code for a
function."""

from gensrc.Utilities import common
from gensrc.Utilities import buffer
from gensrc.Serialization import serializable
from gensrc.Functions import function
from gensrc.Functions import behavior
from gensrc.Parameters import parameterlist
from gensrc.Parameters import parameter
from gensrc.Configuration import environment

class Constructor(function.Function):
    """Function which constructs a library object."""

    #############################################
    # class variables
    #############################################

    generateVOs_ = True
    funcCtorBuffer_ = buffer.loadBuffer('stub.func.constructor')
    DESCRIPTION = 'Construct an object of class %s and return its id'

    #############################################
    # public interface
    #############################################

    def generateBody(self, addin):
        """Generate source code for function body."""
        return Constructor.funcCtorBuffer_ % {
            'functionName' : self.name_,
            'idStrip' : addin.idStrip(self.parameterList_),
            'idSuffix' : addin.objectIdSuffix(),
            'libraryFunction' : self.libraryFunction_,
            'libraryParameters' : self.parameterList_.generate(addin.libraryCall()),
            'namespaceObjects' : environment.config().namespaceObjects(),
            'overwriteVariable' : addin.overwriteVariable(),
            'repositoryClass' : addin.repositoryClass(),
            'voParameters' : self.parameterList_.generate(addin.voCall()) }

    def libraryFunction(self):
        return self.libraryFunction_

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        super(Constructor, self).serialize(serializer)
        serializer.serializeProperty(self, common.LIBRARY_FUNCTION)
        serializer.serializeAttributeBoolean(self, common.CONST, False)

    def postSerialize(self):
        """Perform post serialization initialization."""
        function.Function.postSerialize(self)
        # implicit in the definition of a Constructor is that the first parameter
        # is a string to be used as the objectID of the new object
        self.parameterList_.prepend(parameter.ConstructorObjectId())
        # All ctors have a final optional boolean parameter 'Permanent'
        self.parameterList_.append(parameter.PermanentFlag())
        # dependency tracking trigger
        if self.dependencyTrigger_:
            self.parameterList_.append(parameter.DependencyTrigger())
        # All ctors have a final optional boolean parameter 'Overwrite'
        self.parameterList_.append(parameter.OverwriteFlag())
        self.description_ = Constructor.DESCRIPTION % self.libraryFunction_
        if not self.longDescription_:
            self.longDescription_ = self.description_

    #############################################
    # private member functions
    #############################################

    def __init__(self):
        self.returnValue_ = parameter.ConstructorReturnValue()

