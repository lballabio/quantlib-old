
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

"""encapsulate enumerations for a library datatype."""

from gensrc.Serialization import serializable
from gensrc.Utilities import common

class EnumeratedClass(serializable.Serializable):
    """encapsulate a string->value mapping for a library enumerated class."""

    groupName_ = 'EnumeratedClasses'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeProperty(self, common.STRING)
        serializer.serializeProperty(self, common.VALUE)
        serializer.serializeProperty(self, common.LIBRARY_CLASS)

    def name(self):
        """return unique identifier for this object."""
        return self.string_

    def string(self):
        return self.string_

    def value(self):
        return self.value_

    def libraryClass(self):
        return self.libraryClass_

class EnumeratedClassGroup(serializable.Serializable):
    """encapsulate enumerations for a library datatype."""

    groupName_ = 'EnumeratedClassGroups'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, 'class')
        serializer.serializeObjectDict(self, EnumeratedClass)

    def name(self):
        """return unique identifier for this object."""
        return self.class_

    def className(self):
        return self.class_

    def enumeratedClasses(self):
        """serve up enumerated classes alphabetically by name."""
        for key in self.enumeratedClassKeys_:
            yield self.enumeratedClasses_[key]

