
"""
 Copyright (C) 2005, 2006 Eric Ehlers

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

"""construct an object given its class name."""

import singleton
import enumeration
import function
import xmlreader
import sys

class Factory(singleton.Singleton):
    """construct an object given its class name."""

    creators = {
        'Constructor' : function.Constructor,
        'Member' : function.Member,
        'Procedure' : function.Procedure,
        'Enumeration' : enumeration.Enumeration,
        'EnumerationDefinition' : enumeration.EnumerationDefinition,
    }

    def makeObject(self, className):
        """construct an object given its class name."""
        if self.creators.has_key(className):
            return self.creators[className]()
        else:
            sys.exit('no creator function found for class ' + className)

    def serializeObject(self, objectClass, fileName = None):
        """instantiate an xml reader and load requested object."""
        if not fileName: fileName = objectClass.__name__.lower()
        objectInstance = objectClass()
        serializer = xmlreader.XmlReader(fileName)
        objectInstance.serialize(serializer)
        return objectInstance

