
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

import Singleton
import Enumeration
import Function
import XmlReader
import sys

class Factory(Singleton.Singleton):
    'construct an object given its class name'

    creators = {
        'Constructor' : Function.Constructor,
        'Member' : Function.Member,
        'Utility' : Function.Utility,
        'Enumeration' : Enumeration.Enumeration,
        'EnumerationDefinition' : Enumeration.EnumerationDefinition,
    }

    def makeObject(self, className):
        'construct an object given its class name'
        if self.creators.has_key(className):
            return self.creators[className]()
        else:
            sys.exit('no creator function found for class ' + className)

    def serializeObject(self, objectClass, fileName = None):
        'instantiate an xml reader and load requested object'
        if not fileName: fileName = objectClass.__name__
        objectInstance = objectClass()
        serializer = XmlReader.XmlReader(fileName)
        objectInstance.serialize(serializer)
        return objectInstance

