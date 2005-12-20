
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

'addins'

import common
import Rule
import Serializable
import Buffer

class Addin(Serializable.Serializable):
    'class to encapsulate data and behavior required to generate addin source code'

    def serialize(self, serializer):
        'load/unload class state to/from serializer object'
        serializer.serializeAttribute(self.__dict__, common.NAME)
        serializer.serializeProperty(self.__dict__, common.PLATFORM_ID)
        serializer.serializeProperty(self.__dict__, common.ROOT_DIRECTORY)
        serializer.serializeObjectPropertyDict(self.__dict__, Rule.RuleGroup)
        serializer.serializeObjectPropertyDict(self.__dict__, Buffer.Buffer)

    def generateCode(self, rule, parameters, skipFirst = False, skipIgnore = False):
        'generate source code relating to a list of function parameters'
        ret = ''
        i = 0
        eol = ''
        for parameter in parameters:
            i += 1
            if i == 1 and skipFirst: continue
            if parameter.ignore and skipIgnore: continue
            ret += eol + rule.apply(parameter)
            if i < len(parameters):
                eol = ',\n'
        if ret: ret = '\n' + ret
        return ret

    def generateConversions(self, parameters):
        'generate source code to convert datatypes'
        returnValue = ''
        for parameter in parameters:
            if parameter.needsConversion():
                returnValue += self.conversions.apply(parameter)
        return returnValue

