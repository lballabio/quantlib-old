
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

"""class to encapsulate data and behavior 
required to generate addin source code."""

import common
import rule
import serializable
import buffer

class Addin(serializable.Serializable):
    """class to encapsulate data and behavior 
    required to generate addin source code."""

    stringConvert = '%s'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeProperty(self, common.PLATFORM_ID)
        serializer.serializeProperty(self, common.ROOT_DIRECTORY)
        serializer.serializeObjectPropertyDict(self, rule.RuleGroup)
        serializer.serializeObjectPropertyDict(self, buffer.Buffer)

    def generateConversions(self, parameters):
        """generate source code to convert datatypes."""
        returnValue = ''
        for parameter in parameters:
            if parameter.needsConversion:
                returnValue += self.conversions.apply(parameter)
        if returnValue: returnValue += '\n'
        return returnValue

