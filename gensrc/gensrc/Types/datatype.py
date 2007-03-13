
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

from gensrc.Types import exceptions
from gensrc.Utilities import common
from gensrc.Serialization import serializable
import re

class DataType(serializable.Serializable):

    #############################################
    # class variables
    #############################################

    groupName_ = 'DataTypes'
    namespace_ = None
    classname_ = None
    RE_NAMESPACE = re.compile('(.*)::(.*)')

    #############################################
    # public interface
    #############################################

    def value(self):
        return self.value_

    def classname(self):
        return self.classname_

    def setSuperType(self, value):
        self.superType_ = value

    def superType(self):
        return self.superType_

    def setConversionSuffix(self, value):
        self.conversionSuffix_ = value

    def conversionSuffix(self):
        return self.conversionSuffix_

    def overrideNativeType(self, value):
        if self.nativeType_ == None:
            self.nativeType_ = value

    def nativeType(self):
        return self.nativeType_

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NATIVE_TYPE)
        serializer.serializeValue(self)

    def postSerialize(self):
        """Perform post serialization initialization."""
        m = DataType.RE_NAMESPACE.match(self.value_)
        if m:
            self.namespace_ = m.group(1)
            self.classname_ = m.group(2)

