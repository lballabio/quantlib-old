
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

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

"""procedural utilities"""

from gensrc.Serialization import xmlreader
from gensrc.Configuration import environment

def serializeObject(objectClass, fileName = None):
    """instantiate an xml reader and load requested object."""
    if not fileName: 
        fileName = environment.Environment.instance().rootDirectory() + '/metadata/' + objectClass.__name__.lower()
    objectInstance = objectClass()
    serializer = xmlreader.XmlReader(fileName)
    objectInstance.serialize(serializer)
    objectInstance.postSerialize()
    return objectInstance

def serializeList(path, caller, listName, itemName):
    """instantiate an xml reader and load requested list."""
    if not path: 
        setattr(caller, listName+'_', [])
        return
    serializer = xmlreader.XmlReader(path)
    serializer.serializeList(caller, listName, itemName)


