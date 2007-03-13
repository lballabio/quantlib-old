
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

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

"""construct an object given its class name."""

from gensrc.Serialization import exceptions
from gensrc.Patterns import singleton
from gensrc.Enumerations import enumeratedclasses
from gensrc.Enumerations import enumeratedcurves
from gensrc.Enumerations import enumeratedtypes
from gensrc.Functions import constructor
from gensrc.Functions import member
from gensrc.Functions import procedure
from gensrc.Functions import enumerationmember
from gensrc.Functions import supportedplatform
from gensrc.Rules import rule
from gensrc.Types import supertype

class Factory(singleton.Singleton):
    """construct an object given its class name."""

    creators_ = {
        'Constructor' : constructor.Constructor,
        'EnumeratedClass' : enumeratedclasses.EnumeratedClass,
        'EnumeratedClassGroup' : enumeratedclasses.EnumeratedClassGroup,
        'EnumeratedCurve' : enumeratedcurves.EnumeratedCurve,
        'EnumeratedCurveGroup' : enumeratedcurves.EnumeratedCurveGroup,
        'EnumeratedType' : enumeratedtypes.EnumeratedType,
        'EnumeratedTypeGroup' : enumeratedtypes.EnumeratedTypeGroup,
        'EnumerationMember' : enumerationmember.EnumerationMember,
        'Member' : member.Member,
        'Procedure' : procedure.Procedure,
        'SuperType' : supertype.SuperType,
        'SupportedPlatform' : supportedplatform.SupportedPlatform,
    }

    def makeObject(self, className):
        """construct an object given its class name."""
        if self.creators_.has_key(className):
            return self.creators_[className]()
        else:
            raise exceptions.SerializationCreatorException(className)

