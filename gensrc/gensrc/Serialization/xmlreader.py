
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
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

"""A class which implements the (De)Serializer interface to (de)serialize
a (De)Serializable object from an XML stream."""

from gensrc.Serialization import serializer
from gensrc.Serialization import factory
import xml.dom.minidom
import sys
from gensrc.Utilities import common

class XmlReader(serializer.Serializer):
    """A class which implements the (De)Serializer interface to deserialize
    a (De)Serializable object from an XML stream."""

    ################################################
    # initialization
    ################################################

    def __init__(self, fileName):
        """Load indicated file into dom document."""
        self.documentName = fileName + '.xml'
        try:
            dom = xml.dom.minidom.parse(self.documentName)
        except:
            print 'Error processing XML document ' + self.documentName
            raise
        self.node = dom.documentElement

    ################################################
    # public interface
    ################################################

    def serializeValue(self, caller):
        """Read a named attribute."""
        setattr(caller, 'value', self.getNodeValue(self.node))

    #def serializeName(self, caller):
        #"""Read a named attribute."""
        #setattr(caller, 'name', self.node.nodeName)

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

    # FIXME for class Rule it would be better if defaultValue = None, would that
    # break anything else?
    def serializeAttributeBoolean(self, caller, attributeName, defaultValue = False):
        """Read a named boolean attribute."""
        attributeValue = self.node.getAttribute(attributeName)
        if attributeValue:
            setattr(caller, attributeName, self.stringToBoolean(attributeValue))
        else:
            setattr(caller, attributeName, defaultValue)

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

    def serializeList(self, caller, vectorName, itemName, allowNone = False):
        """Read a list of elements."""
        vectorElement = self.getChild(vectorName, allowNone)
        if not vectorElement:
            setattr(caller, vectorName, None)
            return
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
        for childNode in self.iterateChildren(dictElement):
            ret[childNode.nodeName] = self.getNodeValue(childNode)
        setattr(caller, dictName, ret)

    def serializeObject(self, caller, objectClass, allowNone = False):
        """Load a Serializable object."""
        objectElement = self.getChild(objectClass.__name__, allowNone)
        if not objectElement:
            setattr(caller, objectClass.__name__, None)
            return
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

    def serializeObjectDict(self, caller, objectClass, name='', allowNone = False):
        """Load a named element and write its data as a property of the caller.

        A client invokes this function as follows:
            serializer.serializeObjectDict(self, Foo)

        And after the call returns, the caller's dict is populated as follows:
            self.Foo - a dict of objects, keyed by object identifier
            self.FooKeys - a sorted list of the object identifiers
            self.FooCount - #/objects loaded

        These structures allow the caller to iterate through the objects
        in order by identifier."""

        dictElement = self.getChild(objectClass.groupName, allowNone)
        if not dictElement:
            setattr(caller, objectClass.groupName, None)
            return
        dict = {}
        keys = []
        for childNode in self.iterateChildren(dictElement):
            objectInstance = factory.Factory.instance().makeObject(childNode.nodeName)
            self.node = childNode
            objectInstance.serialize(self)
            objectInstance.postSerialize()
            self.node = self.node.parentNode.parentNode
            dict[objectInstance.key()] = objectInstance
            keys.append(objectInstance.key())
        keys.sort()
        if name:
            groupName = name
            keyName = name + 'Keys'
            countName = name + 'Count'
        else:
            groupName = objectClass.groupName
            keyName = objectClass.__name__ + 'Keys'
            countName = objectClass.__name__ + 'Count'
        setattr(caller, groupName, dict)
        setattr(caller, keyName, keys)
        setattr(caller, countName, len(keys))

    def serializeObjectContainer(self, caller, objectClass, allowNone = False):
        """Load a named element as an object, stored as a property of the caller.

        For XML formatted as follows:

            <FooContainer>
                <foo1>xxx</foo1>
                <foo2>xxx</foo2>
                ...
            <FooContainer>

        This function is called a follows:

            serializer.serializeObjectContainer(self, Foo)
        
        Where 
        - foo1, foo2, etc. are instances of class Foo
        - Foo.groupName = 'FooContainer'
        - FooContainer is a container class designed to hold items of class Foo

        After this function returns, the calling object is populated as follows:

            self.FooContainer

        Where FooContainer identifies an object of class FooContainer containing a
        dictionary of Foo objects."""

        dictElement = self.getChild(objectClass.groupName, allowNone)
        if not dictElement:
            setattr(caller, objectClass.groupName, None)
            return
        dict = {}
        for childNode in self.iterateChildren(dictElement):
            objectInstance = factory.Factory.instance().makeObject(objectClass.__name__)
            self.node = childNode
            objectInstance.serialize(self)
            objectInstance.postSerialize()
            self.node = self.node.parentNode.parentNode
            dict[childNode.nodeName] = objectInstance
        if not len(dict): 
            self.abort('dict element "%s" is empty' % objectClass.groupName)
        dictInstance = factory.Factory.instance().makeObject(objectClass.groupName)
        setattr(dictInstance, objectClass.groupName, dict)
        setattr(caller, objectClass.groupName, dictInstance)

    def serializeObjectPropertyDict(self, caller, objectClass):
        """Load a named element and write its children directly as
        properties of the calling object.

        A client invokes this function as follows:
            serializer.serializeObjectPropertyDict(self, Foo)

        On XML data formatted as follows:
            <FooCollection>
                <Foo name='fooIdentifier_1'/>
                <Foo name='fooIdentifier_2'/>
                ...
                <Foo name='fooIdentifier_n'/>
            </FooCollection>

        And after this function returns, the calling object is populated as follows:
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

    ################################################
    # private interface
    ################################################

    def getChild(self, tagName, allowNone = False):
        """Get single named child of current node."""
        for childNode in self.node.childNodes:
            if childNode.nodeName == tagName:
                return childNode
        if not allowNone:
            self.abort('No element with name "%s"' % tagName)

    def iterateChildren(self, node):
        """Iterate through children of given node, skipping child nodes which
        are not part of the application level XML."""
        for childNode in node.childNodes:
            if childNode.nodeName == '#text' \
            or childNode.nodeName == '#comment': continue
            yield childNode

    def isTextNode(self, node):
        """Return True for element node with single child text node."""
        return node.nodeType == xml.dom.Node.ELEMENT_NODE \
        and len(node.childNodes) == 1 \
        and node.firstChild.nodeType == xml.dom.Node.TEXT_NODE

    def getNodeValue(self, node):
        """Get value of text node."""
        if self.isTextNode(node):
            return node.firstChild.nodeValue
        else:
            return ''

    def stringToBoolean(self, str):
        """Convert text string to boolean."""
        if str.lower() == common.TRUE:
            return True
        elif str.lower() == common.FALSE:
            return False
        else:
            self.abort('Unable to convert string "%s" to boolean' % str)

    def abort(self, errorMessage):
        """Identify the XML document before failing."""
        sys.exit('Error loading XML document %s : %s' 
            % (self.documentName, errorMessage))

