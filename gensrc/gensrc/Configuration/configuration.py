
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

"""global configuration state for gensrc application."""

from gensrc.Utilities import common
from gensrc.Serialization import serializable

class Configuration(serializable.Serializable):

    def __init__(self):
        """initialize"""

        # load copyright buffer
        fileBuffer = open('stubs/stub.copyright')
        self.copyrightBuffer = fileBuffer.read()
        fileBuffer.close()

    def serialize(self, serializer):

        serializer.serializeList(self, 'categoryNames', 'categoryName')
        serializer.serializeProperty(self, 'libRootDirectory')
        serializer.serializeProperty(self, 'voRootDirectory')
        serializer.serializeProperty(self, 'loopRootDirectory')
        serializer.serializeProperty(self, 'excelRootDirectory')
        serializer.serializeProperty(self, common.NAMESPACE_OBJ)
        serializer.serializeProperty(self, common.NAMESPACE_LIB)
        serializer.serializeProperty(self, 'prefix')
        serializer.serializeBoolean(self, 'usingEnumerations')

    def postSerialize(self):

        # initialize paths
        self.excelFullPath = '../' + self.excelRootDirectory + '/'
        if self.libRootDirectory:
            self.libFullPath = '../' + self.libRootDirectory + '/'
        if self.voRootDirectory:
            self.voFullPath = '../' + self.voRootDirectory + '/'
        if self.loopRootDirectory:
            self.loopFullPath = '../' + self.loopRootDirectory + '/'

