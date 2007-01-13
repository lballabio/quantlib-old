
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

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

"""class to encapsulate state and behavior for a named file buffer."""

import common
import config
import serializable

def loadBuffer(bufferName):
    fileName = config.Config.getInstance().rootDir + '/stubs/' + bufferName
    fileBuffer = open(fileName)
    ret = fileBuffer.read()
    fileBuffer.close()
    return ret

class Buffer(serializable.Serializable):
    """class to encapsulate state and behavior for a named file buffer."""

    groupName = 'Buffers'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeAttribute(self, common.FILE_NAME)
        serializer.serializeAttributeBoolean(self, common.LOCAL)

    def postSerialize(self):
        """load the named buffer."""
        if self.local:
            fileBuffer = open('stubs/' + self.fileName)
        else:
            fileBuffer = open(config.Config.getInstance().rootDir + '/stubs/' + self.fileName)
        self.text = fileBuffer.read()
        fileBuffer.close()

