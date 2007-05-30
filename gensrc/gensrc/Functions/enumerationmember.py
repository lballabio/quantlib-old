
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
from gensrc.Functions import member
from gensrc.Functions import behavior
from gensrc.Functions import behaviorloop
from gensrc.Parameters import parameterlist
from gensrc.Parameters import parameter
from gensrc.Configuration import environment

class EnumerationMember(member.Member):
    """Function which invokes member function of Enumeration from the Registry."""

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        super(EnumerationMember, self).serialize(serializer)

    def postSerialize(self):
        """Perform post serialization initialization."""
        function.Function.postSerialize(self)
        # implicit in the definition of an EnumerationMember is that the first parameter
        # is the ID of the enumeration to be retrieved

        # FIXME rework so not necessary to retain "self.parameterObjectID"
        # as reference to first parameter
        self.parameterObjectID_ = parameter.EnumerationId(self.type_, self.superType_)
        self.parameterList_.prepend(self.parameterObjectID_)
        # dependency tracking trigger
        if self.dependencyTrigger_:
            self.parameterList_.append(parameter.DependencyTrigger())
        # determine behavior (normal or loop)
        if self.loopParameter_:
            self.behavior_ = behaviorloop.BehaviorEnumerationLoop(self) 
        else:
            self.behavior_ = behavior.BehaviorMember(self)

