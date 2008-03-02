
"""
 Copyright (C) 2005, 2006, 2007, 2008 Eric Ehlers

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

"""class to encapsulate state and behavior for a named file buffer."""

from gensrc.utilities import common
from gensrc.configuration import environment
from gensrc.serialization import serializable

def loadBuffer(bufferName):
    fileName = environment.Environment.instance().gensrcRootPath() + '/stubs/' + bufferName
    fileBuffer = open(fileName)
    ret = fileBuffer.read()
    fileBuffer.close()
    return ret

class Buffer(serializable.Serializable):
    """class to encapsulate state and behavior for a named file buffer."""

    #############################################
    # class variables
    #############################################

    groupName_ = 'Buffers'

    #############################################
    # public interface
    #############################################

    def text(self):
        return self.text_

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeAttribute(self, common.FILE_NAME)
        serializer.serializeAttributeBoolean(self, common.LOCAL)

    def postSerialize(self):
        """load the named buffer."""
        if self.local_:
            fileBuffer = open(environment.Environment.instance().addinConfigPath() + 'stubs/' + self.fileName_)
        else:
            fileBuffer = open(environment.Environment.instance().gensrcRootPath() + 'stubs/' + self.fileName_)
        self.text_ = fileBuffer.read()
        fileBuffer.close()

