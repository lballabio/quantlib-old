
"""
 Copyright (C) 2005, 2006 Eric Ehlers
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

"""global configuration state for srcgen application."""

import singleton
import category
import enumeration
import factory
import xmlreader
import buffer

class Config(singleton.Singleton):
    """global configuration state for srcgen application."""

    def __init__(self):
        """load state from metadata."""
        xmlConfig = xmlreader.XmlReader('config')
        xmlConfig.serializeObject(self, buffer.Buffer)
        xmlConfig.serializeList(self, 'categoryNames', 'categoryName')
        self.categoryNames.sort()
        self.categoryDict = {}
        for categoryName in self.categoryNames:
            self.categoryDict[categoryName] = \
                factory.Factory.getInstance().serializeObject(category.Category, categoryName)

        xmlEnumerations = xmlreader.XmlReader('enumerations')
        xmlEnumerations.serializeObjectDict(self, enumeration.Enumeration)

    def getCategories(self, platformId):
        """serve up function category objects alphabetically by name."""
        for categoryName in self.categoryNames:
            category = self.categoryDict[categoryName]
            if platformId == '*' or category.platformSupported(platformId):
                yield category

    def getEnumerations(self):
        """serve up enumeration objects alphabetically by name."""
        for enumerationKey in self.EnumerationKeys:
            yield self.Enumerations[enumerationKey]

