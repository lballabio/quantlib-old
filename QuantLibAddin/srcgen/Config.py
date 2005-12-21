
"""
 Copyright (C) 2005 Eric Ehlers
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

'configuration'

import Singleton
import Category
import Enumeration
import Factory
import XmlReader
import Buffer
import utils

class Config(Singleton.Singleton):
    'global configuration state for srcgen application'

    def __init__(self):
        'load state from metadata'
        xmlConfig = XmlReader.XmlReader('Config')
        xmlConfig.serializeObject(self.__dict__, Buffer.Buffer)
        xmlConfig.serializeList(self.__dict__, 'categoryNames', 'categoryName')
        self.categoryNames.sort()
        self.categoryDict = {}
        for categoryName in self.categoryNames:
            self.categoryDict[categoryName] = \
                Factory.Factory.getInstance().serializeObject(Category.Category, categoryName)

        xmlEnumerations = XmlReader.XmlReader('enumerations')
        xmlEnumerations.serializeObjectDict(self.__dict__, Enumeration.Enumeration)

    def getCategories(self, platformId):
        'serve up function category objects alphabetically by name'
        for categoryName in self.categoryNames:
            category = self.categoryDict[categoryName]
            if platformId == '*' or category.platformSupported(platformId):
                yield category

    def getEnumerations(self):
        'serve up enumeration objects alphabetically by name'
        for enumerationKey in self.EnumerationKeys:
            yield self.Enumerations[enumerationKey]

