
"""
 Copyright (C) 2005, 2006, 2007, 2008 Eric Ehlers

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

"""construct an object given its class name."""

from gensrc.serialization import exceptions
from gensrc.patterns import singleton
from gensrc.enumerations import enumeratedclasses
from gensrc.enumerations import enumeratedpairs
from gensrc.enumerations import enumeratedtypes
from gensrc.functions import constructor
from gensrc.functions import member
from gensrc.functions import procedure
from gensrc.functions import enumerationmember
from gensrc.functions import supportedplatform
from gensrc.rules import rule
from gensrc.types import datatype
from gensrc.types import supertype

class Factory(singleton.Singleton):
    """construct an object given its class name."""

    creators_ = {
        'Constructor' : constructor.Constructor,
        'DataType' : datatype.DataType,
        'EnumeratedClass' : enumeratedclasses.EnumeratedClass,
        'EnumeratedClassGroup' : enumeratedclasses.EnumeratedClassGroup,
        'EnumeratedPair' : enumeratedpairs.EnumeratedPair,
        'EnumeratedPairGroup' : enumeratedpairs.EnumeratedPairGroup,
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

