
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

import common
import singleton
import function
import category
import enumeration
import utilities
import xmlreader
import os

class Config(singleton.Singleton):
    """global configuration state for srcgen application."""

    def __init__(self):
        """initialize"""

        # check environment variable
        self.rootDir = os.environ['GENSRC_DIR']
        if not self.rootDir:
            sys.exit('Environment variable GENSRC_DIR is not set')

    def initialize(self):

        # load copyright buffer
        fileBuffer = open('stubs/stub.copyright')
        self.copyrightBuffer = fileBuffer.read()
        fileBuffer.close()
        
        # load state from configuration file
        xmlConfig = xmlreader.XmlReader('config/config')
        xmlConfig.serializeList(self, 'categoryNames', 'categoryName')
        xmlConfig.serializeProperty(self, 'libRootDirectory')
        xmlConfig.serializeProperty(self, 'voRootDirectory')
        xmlConfig.serializeProperty(self, 'loopRootDirectory')
        xmlConfig.serializeProperty(self, 'excelRootDirectory')
        xmlConfig.serializeProperty(self, common.NAMESPACE_OBJ)
        xmlConfig.serializeProperty(self, common.NAMESPACE_LIB)
        xmlConfig.serializeProperty(self, 'prefix')
        xmlConfig.serializeBoolean(self, 'usingEnumerations')
        xmlConfig.serializeList(self, 'implicitConversions', 'implicitConversion')
        self.categoryNames.sort()
        self.categoryDict = {}
        for categoryName in self.categoryNames:
            self.categoryDict[categoryName] = \
                utilities.serializeObject(category.Category, 'metadata/' + categoryName)

        # load enumeration metadata
        if self.usingEnumerations:
            xmlEnumerations = xmlreader.XmlReader('metadata/enumtypes')
            xmlEnumerations.serializeObjectDict(self, enumeration.Enumeration, 'EnumType')
            xmlEnumerations.serializeProperty(self, common.ENUM_TYPE_COPYRIGHT)
            xmlEnumerations = xmlreader.XmlReader('metadata/enumclasses')
            xmlEnumerations.serializeObjectDict(self, enumeration.Enumeration, 'EnumClass')
            xmlEnumerations.serializeProperty(self, common.ENUM_CLASS_COPYRIGHT)
            xmlEnumerations = xmlreader.XmlReader('metadata/enumcurves')
            xmlEnumerations.serializeObjectDict(self, enumeration.Enumeration, 'EnumCurve')

        # initialize paths
        self.excelFullPath = '../' + self.excelRootDirectory + '/'
        if self.libRootDirectory:
            self.libFullPath = '../' + self.libRootDirectory + '/'
        if self.voRootDirectory:
            self.voFullPath = '../' + self.voRootDirectory + '/'
        if self.loopRootDirectory:
            self.loopFullPath = '../' + self.loopRootDirectory + '/'

    def getCategories(self, platformName, implementation = function.AUTO):
        """serve up function category objects alphabetically by name."""
        for categoryName in self.categoryNames:
            category = self.categoryDict[categoryName]
            if platformName == '*' or category.platformSupported(platformName, implementation):
                yield category

    def getEnumTypes(self):
        """serve up enumerated type objects alphabetically by name."""
        for enumTypeKey in self.EnumTypeKeys:
            yield self.EnumType[enumTypeKey]

    def getEnumClasses(self):
        """serve up enumerated class objects alphabetically by name."""
        for enumClassKey in self.EnumClassKeys:
            yield self.EnumClass[enumClassKey]

    def getEnumCurves(self):
        """serve up enumerated curve objects alphabetically by name."""
        for enumCurveKey in self.EnumCurveKeys:
            yield self.EnumCurve[enumCurveKey]

