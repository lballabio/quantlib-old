
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

'function'

import Parameter
import Serializable
import common
import utils

class Function(Serializable.Serializable):
    'encapsulate state and behavior required to generate source code for a function'

    groupName = 'Functions'

    def serialize(self, serializer):
        'load/unload class state to/from serializer object'
        serializer.serializeAttribute(self.__dict__, common.NAME)
        serializer.serializeProperty(self.__dict__, common.DESCRIPTION)
        serializer.serializeProperty(self.__dict__, common.LIBRARY_FUNCTION)
        serializer.serializeProperty(self.__dict__, common.FUNCTION_CATEGORY)
        serializer.serializeProperty(self.__dict__, common.PLATFORMS, '*')
        serializer.serializeBoolean(self.__dict__, common.CONSTRUCTOR)
        serializer.serializeAttributeBoolean(self.__dict__, common.GET_OBJECT)
        serializer.serializeObjectList(self.__dict__, Parameter.Parameter)
        serializer.serializeObject(self.__dict__, Parameter.ReturnValue)

    def postSerialize(self):
        'perform post serialization initialization'
        # FIXME the state of the Function object varies
        # depending on whether it's a constructor or a member
        # and this should be represented with subclasses

        #library class of first parameter of member function
        if not self.constructor and self.Parameters:
            self.libraryClass = self.Parameters[0].libraryClass

        #code snippet to access underlying QuantLib object
        if self.libraryFunction:
            if self.getObject:
                self.accessLibFunc = 'objectPointer->getObject().' + self.libraryFunction
            else:
                self.accessLibFunc = 'objectPointer->' + self.libraryFunction

    def platformSupported(self, platformID):
        'determine whether this function supported by given platform'
        if self.platforms == '*': return True
        return self.platforms.find(platformID) != -1

