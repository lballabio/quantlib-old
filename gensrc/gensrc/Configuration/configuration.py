
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
        """load/unload class state to/from serializer object."""

        serializer.serializeList(self, 'categoryNames', 'categoryName')
        serializer.serializeProperty(self, 'libRootDirectory')
        serializer.serializeProperty(self, common.NAMESPACE_OBJ)
        serializer.serializeProperty(self, common.NAMESPACE_LIB)
        serializer.serializeProperty(self, 'prefix')
        serializer.serializeBoolean(self, 'usingEnumerations')

    def postSerialize(self):
        """Perform post serialization initialization."""
        self.voRootDirectory = self.libRootDirectory + '/ValueObjects'
        self.loopRootDirectory = self.libRootDirectory + '/Loop'
        self.libFullPath = '../' + self.libRootDirectory + '/'

