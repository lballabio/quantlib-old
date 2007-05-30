
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

"""encapsulate enumerations for a library datatype."""

from gensrc.Serialization import serializable
from gensrc.Utilities import common

class EnumeratedPair(serializable.Serializable):
    """encapsulate a string->value mapping for a library enumerated type."""

    #############################################
    # class variables
    #############################################

    groupName_ = 'EnumeratedPairs'

    #############################################
    # class variables
    #############################################

    def name(self):
        """return unique identifier for this object."""
        return self.id1_ + ':' + self.id2_

    def id1(self):
        return self.id1_

    def id2(self):
        return self.id2_

    def value(self):
        return self.value_

    #############################################
    # public interface
    #############################################

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeProperty(self, 'id1')
        serializer.serializeProperty(self, 'id2')
        serializer.serializeProperty(self, common.VALUE)

class EnumeratedPairGroup(serializable.Serializable):
    """encapsulate enumerations for a library datatype."""

    #############################################
    # class variables
    #############################################

    groupName_ = 'EnumeratedPairGroups'

    #############################################
    # public interface
    #############################################

    def className(self):
        return self.class_

    def enumeratedPairs(self):
        """serve up enumeration definition objects alphabetically by name."""
        for key in self.enumeratedPairKeys_:
            yield self.enumeratedPairs_[key]

    #############################################
    # serializer interface
    #############################################

    def name(self):
        """return unique identifier for this object."""
        return self.class_

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, 'class')
        serializer.serializeObjectDict(self, EnumeratedPair)

