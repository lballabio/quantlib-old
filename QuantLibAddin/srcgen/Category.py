
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

'category'

import common
import Function
import Serializable

class Category(Serializable.Serializable):
    'class to represent a group of functions'

    def serialize(self, serializer):
        'load/unload class state to/from serializer object'
        serializer.serializeAttribute(self.__dict__, common.NAME)
        serializer.serializeProperty(self.__dict__, common.DISPLAY_NAME)
        serializer.serializeProperty(self.__dict__, common.DESCRIPTION)
        serializer.serializeBoolean(self.__dict__, common.HEADER_ONLY)
        serializer.serializeObjectDict(self.__dict__, Function.Function)

    def platformSupported(self, platformID):
        'determine whether this category supported for given platform'
        for function in self.Functions.values():
            if function.platformSupported(platformID):
                return True

    def getFunctions(self, platformId):
        'serve up functions alphabetically by name'
        for functionKey in self.FunctionKeys: 
            function = self.Functions[functionKey]
            if platformId == '*' or function.platformSupported(platformId):
                yield function

