
"""
 Copyright (C) 2005, 2006, 2007, 2008 Eric Ehlers
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
from gensrc.Functions import behaviorloop
from gensrc.Parameters import parameterlist
from gensrc.Parameters import parameter
from gensrc.Configuration import environment

class Member(function.Function):
    """Function which invokes member function of existing library object."""

    #############################################
    # public interface
    #############################################

    def generateBody(self, addin):
        """Generate source code for the body of the function."""
        return self.behavior_.generateBody(addin)

    def memberAccess(self):
        return self.memberAccess_

    def objectId(self):
        return self.objectId_

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        super(Member, self).serialize(serializer)
        serializer.serializeProperty(self, common.LIBRARY_FUNCTION)
        serializer.serializeAttribute(self, common.TYPE)
        serializer.serializeAttribute(self, common.SUPER_TYPE)
        serializer.serializeAttribute(self, common.LOOP_PARAMETER)
        serializer.serializeObject(self, parameter.ReturnValue)
        serializer.serializeAttributeBoolean(self, common.CONST, True)

    def postSerialize(self):
        """Perform post serialization initialization."""
        function.Function.postSerialize(self)
        
        # The 1st param of a Member function is always the ID of the object to be retrieved
        parameterObjectId = parameter.MemberObjectId(self.type_, self.superType_)
        self.parameterList_.prepend(parameterObjectId)
        self.memberAccess_ = parameterObjectId.fullType().memberAccess()
        self.objectId_ = parameterObjectId.nameConverted()
        
        # dependency tracking trigger
        if self.dependencyTrigger_:
            self.parameterList_.append(parameter.DependencyTrigger())
        # determine behavior (normal or loop)
        if self.loopParameter_:
            self.behavior_ = behaviorloop.BehaviorMemberLoop(self)
        else:
            self.behavior_ = behavior.BehaviorMember(self)
