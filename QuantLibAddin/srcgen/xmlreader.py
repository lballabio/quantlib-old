
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

"""A class which implements the (De)Serializer interface to deserialize
a (De)Serializable object from an XML stream."""

import serializer
import factory
import xml.dom.minidom
import sys
import common

class XmlReader(serializer.Serializer):
    """A class which implements the (De)Serializer interface to deserialize
    a (De)Serializable object from an XML stream."""

    def __init__(self, fileName):
        """Load indicated file into dom document."""
        self.documentName = fileName + '.xml'
        try:
            dom = xml.dom.minidom.parse(self.documentName)
        except:
            print 'Error processing XML document ' + self.documentName
            raise
        self.node = dom.documentElement

    def serializeAttribute(self, caller, attributeName, defaultValue = None):
        """Read a named attribute."""
        attributeValue = self.node.getAttribute(attributeName)
        if attributeValue:
            setattr(caller, attributeName, attributeValue)
        else:
            setattr(caller, attributeName, defaultValue)

    def serializeAttributeInteger(self, caller, attributeName, defaultValue = None):
        """Read a named integral attribute."""
        attributeValue = self.node.getAttribute(attributeName)
        if attributeValue:
            setattr(caller, attributeName, int(attributeValue))
        else:
            setattr(caller, attributeName, defaultValue)

    def serializeAttributeBoolean(self, caller, attributeName):
        """Read a named boolean attribute."""
        attributeValue = self.node.getAttribute(attributeName)
        if attributeValue:
            setattr(caller, attributeName, self.stringToBoolean(attributeValue))
        else:
            setattr(caller, attributeName, False)

    def serializeProperty(self, caller, propertyName, defaultValue = None):
        """Read a named property."""
        element = self.getChild(propertyName, True)
        if element:
            setattr(caller, propertyName, self.getNodeValue(element))
        else:
            setattr(caller, propertyName, defaultValue)

    def serializeBoolean(self, caller, propertyName, defaultValue = False):
        """Read a named boolean property."""
        element = self.getChild(propertyName, True)
        if element:
            setattr(caller, propertyName, self.stringToBoolean(self.getNodeValue(element)))
        else:
            setattr(caller, propertyName, defaultValue)

    def serializeList(self, caller, vectorName, itemName):
        """Read a list of elements."""
        vectorElement = self.getChild(vectorName)
        itemElements = vectorElement.getElementsByTagName(itemName)
        ret = []
        for itemElement in itemElements:
            ret.append(self.getNodeValue(itemElement))
        setattr(caller, vectorName, ret)

    def serializeDict(self, caller, dictName):
        """Read a named element in the document and set its values as
        attributes of the caller."""
        dictElement = self.getChild(dictName)
        ret = {}
        for childNode in dictElement.childNodes:
            if self.isTextNode(childNode):
                ret[childNode.nodeName] = self.getNodeValue(childNode)
        setattr(caller, dictName, ret)

    def serializeObject(self, caller, objectClass):
        """Load a Serializable object."""
        objectElement = self.getChild(objectClass.__name__)
        objectInstance = objectClass()
        self.node = objectElement
        objectInstance.serialize(self)
        objectInstance.postSerialize()
        self.node = self.node.parentNode
        setattr(caller, objectInstance.key(), objectInstance)

    def serializeObjectList(self, caller, objectClass):
        """Load a list of Serializable objects."""
        listElement = self.getChild(objectClass.groupName)
        itemElements = listElement.getElementsByTagName(objectClass.__name__)
        ret = []
        for itemElement in itemElements:
            objectInstance = objectClass()
            self.node = itemElement
            objectInstance.serialize(self)
            objectInstance.postSerialize()
            self.node = self.node.parentNode.parentNode
            ret.append(objectInstance)
        setattr(caller, objectClass.groupName, ret)
        setattr(caller, objectClass.__name__ + 'Count', len(ret))

    def serializeObjectDict(self, caller, objectClass):
        """Load a named element and write its data as a property of the caller.

        A client invokes this function as follows:
            serializer.serializeObjectDict(self, Foo)

        And after the call returns, the caller's dict is populated as follows:
            self.Foo - a dict of objects, keyed by object identifier
            self.FooKeys - a sorted list of the object identifiers
            self.FooCount - #/objects loaded

        These structures allow the caller to iterate through the objects
        in order by identifier."""

        dictElement = self.getChild(objectClass.groupName)
        dict = {}
        keys = []
        for childNode in dictElement.childNodes:
            if childNode.nodeName == '#text': continue
            objectInstance = factory.Factory.getInstance().makeObject(childNode.nodeName)
            self.node = childNode
            objectInstance.serialize(self)
            objectInstance.postSerialize()
            self.node = self.node.parentNode.parentNode
            dict[objectInstance.key()] = objectInstance
            keys.append(objectInstance.key())
        keys.sort()
        setattr(caller, objectClass.groupName, dict)
        setattr(caller, objectClass.__name__ + 'Keys', keys)
        setattr(caller, objectClass.__name__ + 'Count', len(keys))

    def serializeObjectPropertyDict(self, caller, objectClass):
        """Load a named element and write its children directly as
        properties of the calling object.

        A client invokes this function as follows:
            serializer.serializeObjectPropertyDict(self, Foo)

        For xml data formatted as follows:
            <FooList>
                <Foo name='fooIdentifier_1'/>
                <Foo name='fooIdentifier_2'/>
                ...
                <Foo name='fooIdentifier_n'/>
            </FooList>

        And after the call returns, the caller's dict is populated as follows:
            self.fooIdentifier_1
            self.fooIdentifier_2
            ...
            self.fooIdentifier_n

        This is appropriate when the caller knows at compile time the names
        of the objects to be loaded."""

        dictElement = self.getChild(objectClass.groupName)
        itemElements = dictElement.getElementsByTagName(objectClass.__name__)
        for itemElement in itemElements:
            objectInstance = objectClass()
            self.node = itemElement
            objectInstance.serialize(self)
            objectInstance.postSerialize()
            self.node = self.node.parentNode.parentNode
            setattr(caller, objectInstance.key(), objectInstance)

    def getChild(self, tagName, allowNone = False):
        """Get single named child of current node."""
        for childNode in self.node.childNodes:
            if childNode.nodeName == tagName:
                return childNode
        if not allowNone:
            self.abort('No element with name "%s"' % tagName)

    def getNodeValue(self, node):
        """Get value of text node."""
        if node.nodeType == xml.dom.Node.ELEMENT_NODE \
        and len(node.childNodes) == 1 \
        and node.firstChild.nodeType == xml.dom.Node.TEXT_NODE:
            return node.firstChild.nodeValue
        else:
            self.abort('Error processing node "%s"' % node.nodeName)

    def stringToBoolean(self, str):
        """Convert text string to boolean."""
        if str.lower() == common.TRUE:
            return True
        elif str.lower() == common.FALSE:
            return False
        else:
            self.abort('Unable to convert string "%s" to boolean' % str)

    def isTextNode(self, node):
        """Return True for element node with single child text node."""
        return node.nodeType == xml.dom.Node.ELEMENT_NODE \
        and len(node.childNodes) == 1 \
        and node.firstChild.nodeType == xml.dom.Node.TEXT_NODE

    def abort(self, errorMessage):
        """Identify the xml document before failing."""
        sys.exit('Error loading XML document %s : %s' 
            % (self.documentName, errorMessage))

