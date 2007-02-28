
"""
 Copyright (C) 2007 Eric Ehlers

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

from gensrc.Enumerations import enumeration
from gensrc.Serialization import xmlreader
from gensrc.Utilities import common

class EnumerationList(object):

    def __init__(self):

        xmlEnumTypes = xmlreader.XmlReader('metadata/enumtypes')
        xmlEnumTypes.serializeObjectDict(self, enumeration.Enumeration, 'EnumType')
        xmlEnumTypes.serializeProperty(self, common.ENUM_TYPE_COPYRIGHT)

        xmlEnumClasses = xmlreader.XmlReader('metadata/enumclasses')
        xmlEnumClasses.serializeObjectDict(self, enumeration.Enumeration, 'EnumClass')
        xmlEnumClasses.serializeProperty(self, common.ENUM_CLASS_COPYRIGHT)

        xmlEnumCurves = xmlreader.XmlReader('metadata/enumcurves')
        xmlEnumCurves.serializeObjectDict(self, enumeration.Enumeration, 'EnumCurve')

    def enumeratedTypes(self):
        """serve up enumerated type objects alphabetically by name."""
        for enumTypeKey in self.EnumTypeKeys:
            yield self.EnumType[enumTypeKey]

    def enumeratedClasses(self):
        """serve up enumerated class objects alphabetically by name."""
        for enumClassKey in self.EnumClassKeys:
            yield self.EnumClass[enumClassKey]

    def enumeratedCurves(self):
        """serve up enumerated curve objects alphabetically by name."""
        for enumCurveKey in self.EnumCurveKeys:
            yield self.EnumCurve[enumCurveKey]

