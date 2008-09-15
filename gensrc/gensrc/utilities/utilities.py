
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

"""Procedural utilities."""

from gensrc.serialization import xmlreader
from gensrc.configuration import environment
from gensrc.utilities import exceptions

def serializeObjectImpl(objectClass, fileName):
    """Instantiate an XML reader and load requested object."""
    objectInstance = objectClass()
    serializer = xmlreader.XmlReader(fileName)
    objectInstance.serialize(serializer)
    objectInstance.postSerialize()
    return objectInstance

def serializeObject(objectClass, fileName = None):
    """Perform preprocessing and error handling for serializeObjectImpl"""
    if not fileName: 
        fileName = environment.Environment.instance().gensrcRootPath() + \
            '/metadata/' + objectClass.__name__.lower()
    try:
        return serializeObjectImpl(objectClass, fileName)
    except:
        raise exceptions.UtilitiesSerializationException(
            fileName, objectClass.__name__)

def serializeList(path, caller, listName, itemName):
    """Instantiate an XML reader and load requested list."""
    if not path: 
        setattr(caller, listName + '_', [])
        return
    try:
        serializer = xmlreader.XmlReader(path)
        serializer.serializeList(caller, listName, itemName)
    except:
        raise exceptions.UtilitiesSerializationListException(
            path, listName, itemName)

