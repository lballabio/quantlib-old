
"""
 Copyright (C) 2007, 2008 Eric Ehlers

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

"""Encapsulate a list of enumerated types, classes, and pairs."""

from gensrc.enumerations import enumeratedtypes
from gensrc.enumerations import enumeratedclasses
from gensrc.enumerations import enumeratedpairs
from gensrc.serialization import xmlreader
from gensrc.utilities import common
import os.path

class EnumerationList(object):
    """Encapsulate a list of enumerated types, classes, and pairs."""

    #############################################
    # public interface
    #############################################

    def enumeratedTypeCopyright(self):
        """Return the copyright message for enumerated types."""
        return self.enumeratedTypeCopyright_

    def enumeratedClassCopyright(self):
        """Return the copyright message for enumerated classes."""
        return self.enumeratedClassCopyright_

    def enumeratedPairCopyright(self):
        """Return the copyright message for enumerated pairs."""
        return self.enumeratedPairCopyright_

    def enumeratedTypeGroupsCount(self):
        """Return the number of enumerated types."""
        return len(self.enumeratedTypeGroups_)

    def enumeratedClassGroupsCount(self):
        """Return the number of enumerated classes."""
        return len(self.enumeratedClassGroups_)

    def enumeratedTypeGroups(self):
        """Serve up enumerated type objects alphabetically by name."""
        for key in self.enumeratedTypeGroupKeys_:
            yield self.enumeratedTypeGroups_[key]

    def enumeratedClassGroups(self):
        """Serve up enumerated class objects alphabetically by name."""
        for key in self.enumeratedClassGroupKeys_:
            yield self.enumeratedClassGroups_[key]

    def enumeratedPairGroups(self):
        """Serve up enumerated pair objects alphabetically by name."""
        for key in self.enumeratedPairGroupKeys_:
            yield self.enumeratedPairGroups_[key]

    #############################################
    # private member functions
    #############################################

    def __init__(self):
        """Initialize the EnumerationList object and load enumeration parameters from
        available config files."""

        self.typeDict_ = {}

        if os.path.exists('metadata/enumerations/enumeratedtypes.xml'):
            xmlEnumTypes = xmlreader.XmlReader('metadata/enumerations/enumeratedtypes')
            xmlEnumTypes.serializeObjectDict(self, enumeratedtypes.EnumeratedTypeGroup)
            xmlEnumTypes.serializeProperty(self, common.ENUM_TYPE_COPYRIGHT)
            self.hasEnumeratedTypes = True
            for item in self.enumeratedTypeGroups_.values():
                self.typeDict_[item.type()] = item.includeFile()
        else:
            self.hasEnumeratedTypes = False

        if os.path.exists('metadata/enumerations/enumeratedclasses.xml'):
            xmlEnumClasses = xmlreader.XmlReader('metadata/enumerations/enumeratedclasses')
            xmlEnumClasses.serializeObjectDict(self, enumeratedclasses.EnumeratedClassGroup)
            xmlEnumClasses.serializeProperty(self, common.ENUM_CLASS_COPYRIGHT)
            self.hasEnumeratedClasses = True
            for item in self.enumeratedClassGroups_.values():
                self.typeDict_[item.className()] = item.includeFile()
        else:
            self.hasEnumeratedClasses= False

        if os.path.exists('metadata/enumerations/enumeratedpairs.xml'):
            xmlEnumPairs = xmlreader.XmlReader('metadata/enumerations/enumeratedpairs')
            xmlEnumPairs.serializeObjectDict(self, enumeratedpairs.EnumeratedPairGroup)
            xmlEnumPairs.serializeProperty(self, common.ENUM_PAIR_COPYRIGHT)
            self.hasEnumeratedPairs = True
            for item in self.enumeratedPairGroups_.values():
                self.typeDict_[item.className()] = item.includeFile()
        else:
            self.hasEnumeratedPairs = False


    def enumIncludes(self, parameterList):
        """Generate a list of all the #includes necessary to compile source code
        for any enumerations which may appear in the given parameter list."""
        ret = []
        for p in parameterList:
            if p.fullType().superType() == common.ENUM \
            and self.typeDict_.has_key(p.fullType().value()):
                ret.append(self.typeDict_[p.fullType().value()])
        return ret

