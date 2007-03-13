
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

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

"""encapsulate enumerations for a QuantLib datatype."""

from gensrc.Serialization import serializable
from gensrc.Utilities import common

class EnumeratedCurve(serializable.Serializable):
    """encapsulate a string->value mapping for a QuantLib enumerated type."""

    groupName_ = 'EnumeratedCurves'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeProperty(self, 'traits')
        serializer.serializeProperty(self, 'interpolator')
        serializer.serializeProperty(self, common.VALUE)

    def name(self):
        """return unique identifier for this object."""
        return self.traits_ + ':' + self.interpolator_

    def traits(self):
        return self.traits_

    def interpolator(self):
        return self.interpolator_

    def value(self):
        return self.value_

class EnumeratedCurveGroup(serializable.Serializable):
    """encapsulate enumerations for a QuantLib datatype."""

    groupName_ = 'EnumeratedCurveGroups'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, 'class')
        serializer.serializeObjectDict(self, EnumeratedCurve)

    def name(self):
        """return unique identifier for this object."""
        return self.class_

    def className(self):
        return self.class_

    def enumeratedCurves(self):
        """serve up enumeration definition objects alphabetically by name."""
        for key in self.enumeratedCurveKeys_:
            yield self.enumeratedCurves_[key]

