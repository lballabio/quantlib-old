
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

"""class to encapsulate data and behavior 
required to generate addin source code."""

from gensrc.Utilities import common
from gensrc.Rules import rule
from gensrc.Serialization import serializable
from gensrc.Serialization import xmlreader
from gensrc.Utilities import buffer
import os

class Addin(serializable.Serializable):
    """class to encapsulate data and behavior 
    required to generate addin source code."""

    stringConvert = '%s'
    voSupported = False
    objectIdSuffix = ''

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeProperty(self, common.ROOT_DIRECTORY)
        serializer.serializeObjectPropertyDict(self, buffer.Buffer)
        serializer.serializeProperty(self, common.COPYRIGHT)
        serializer.serializeBoolean(self, 'loadRules', True)

    def postSerialize(self):
        """Perform post serialization initialization."""

        if self.loadRules:
            serializer = xmlreader.XmlReader('metadata/Rules/' + self.name.lower())
            serializer.serializeObjectPropertyDict(self, rule.RuleGroup)

        self.unchanged = 0
        self.updated = 0
        self.created = 0
        if self.rootDirectory:
            self.rootPath = "../%s/" % self.rootDirectory
            if not os.path.exists(self.rootPath): 
                os.makedirs(self.rootPath)

