
"""
 Copyright (C) 2007 Eric Ehlers

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

from gensrc.Enumerations import enumeratedtypes
from gensrc.Enumerations import enumeratedclasses
from gensrc.Enumerations import enumeratedpairs
from gensrc.Serialization import xmlreader
from gensrc.Utilities import common

class EnumerationList(object):

    #############################################
    # public interface
    #############################################

    def enumeratedTypeCopyright(self):
        return self.enumeratedTypeCopyright_

    def enumeratedClassCopyright(self):
        return self.enumeratedClassCopyright_

    def enumeratedPairCopyright(self):
        return self.enumeratedPairCopyright_

    def enumeratedTypeGroups(self):
        """serve up enumerated type objects alphabetically by name."""
        for key in self.enumeratedTypeGroupKeys_:
            yield self.enumeratedTypeGroups_[key]

    def enumeratedTypeGroupsCount(self):
        return len(self.enumeratedTypeGroups_)

    def enumeratedClassGroupsCount(self):
        return len(self.enumeratedClassGroups_)

    def enumeratedClassGroups(self):
        """serve up enumerated class objects alphabetically by name."""
        for key in self.enumeratedClassGroupKeys_:
            yield self.enumeratedClassGroups_[key]

    def enumeratedPairGroups(self):
        """serve up enumerated pair objects alphabetically by name."""
        for key in self.enumeratedPairGroupKeys_:
            yield self.enumeratedPairGroups_[key]

    #############################################
    # private member functions
    #############################################

    def __init__(self):

        xmlEnumTypes = xmlreader.XmlReader('metadata/Enumerations/enumeratedtypes')
        xmlEnumTypes.serializeObjectDict(self, enumeratedtypes.EnumeratedTypeGroup)
        xmlEnumTypes.serializeProperty(self, common.ENUM_TYPE_COPYRIGHT)

        xmlEnumClasses = xmlreader.XmlReader('metadata/Enumerations/enumeratedclasses')
        xmlEnumClasses.serializeObjectDict(self, enumeratedclasses.EnumeratedClassGroup)
        xmlEnumClasses.serializeProperty(self, common.ENUM_CLASS_COPYRIGHT)

        xmlEnumPairs = xmlreader.XmlReader('metadata/Enumerations/enumeratedpairs')
        xmlEnumPairs.serializeObjectDict(self, enumeratedpairs.EnumeratedPairGroup)
        xmlEnumPairs.serializeProperty(self, common.ENUM_PAIR_COPYRIGHT)

        self.typeDict_ = {}
        for item in self.enumeratedTypeGroups_.values():
            self.typeDict_[item.type()] = item.includeFile()
        for item in self.enumeratedClassGroups_.values():
            self.typeDict_[item.className()] = item.includeFile()
        for item in self.enumeratedPairGroups_.values():
            self.typeDict_[item.className()] = item.includeFile()

    def include(self, typeName):
        if self.typeDict_.has_key(typeName):
            return self.typeDict_[typeName]
        else:
            return ''
