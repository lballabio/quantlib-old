
"""
 Copyright (C) 2005 Eric Ehlers

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
import serializable

class Buffer(serializable.Serializable):
    """class to encapsulate state and behavior for a named file buffer."""

    groupName = 'Buffers'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self.__dict__, common.NAME)
        serializer.serializeAttribute(self.__dict__, common.FILE_NAME)

    def postSerialize(self):
        """load the named buffer."""
        fileBuffer = open(self.fileName)
        self.text = fileBuffer.read()
        fileBuffer.close()

