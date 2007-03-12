
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
from gensrc.Serialization import exceptions
import xml.dom.minidom
from gensrc.Utilities import common
import re

class XmlReader(serializer.Serializer):
    """A class which implements the (De)Serializer interface to deserialize
    a (De)Serializable object from an XML stream."""

    ################################################
    # initialization
    ################################################

    def __init__(self, fileName):
        """Load indicated file into dom document."""
        # FIXME documentName_ retained as member variable to be passed to other exceptions -
        # no need to do this?
        self.documentName_ = fileName + '.xml'
        try:
            dom = xml.dom.minidom.parse(self.documentName_)
        except:
            raise exceptions.SerializationParseException(self.documentName_)
        self.node_ = dom.documentElement

    ################################################
    # public interface
    ################################################

    def serializeValue(self, caller, valueName = 'value'):
        """Read a named attribute."""
        identifierName = self.formatIdentifier(valueName)
        setattr(caller, identifierName, self.getNodeValue(self.node_))

    def serializeAttribute(self, caller, attributeName, defaultValue = None):
        """Read a named attribute."""
        attributeValue = self.node_.getAttribute(attributeName)
        identifierName = self.formatIdentifier(attributeName)
        if attributeValue:
            setattr(caller, identifierName, attributeValue)
        else:
            setattr(caller, identifierName, defaultValue)

    def serializeAttributeInteger(self, caller, attributeName, defaultValue = None):
        """Read a named integral attribute."""
        attributeValue = self.node_.getAttribute(attributeName)
        identifierName = self.formatIdentifier(attributeName)
        if attributeValue:
            setattr(caller, identifierName, int(attributeValue))
        else:
            setattr(caller, identifierName, defaultValue)

    # FIXME for class Rule it would be better if defaultValue = None,
    # would that break anything else?
    def serializeAttributeBoolean(self, caller, attributeName, defaultValue = False):
        """Read a named boolean attribute."""
        attributeValue = self.node_.getAttribute(attributeName)
        identifierName = self.formatIdentifier(attributeName)
        if attributeValue:
            setattr(caller, identifierName, self.stringToBoolean(attributeValue))
        else:
            setattr(caller, identifierName, defaultValue)

    def serializeProperty(self, caller, propertyName, defaultValue = None):
        """Read a named property."""
        element = self.getChild(propertyName, True)
        identifierName = self.formatIdentifier(propertyName)
        if element:
            setattr(caller, identifierName, self.getNodeValue(element))
        else:
            setattr(caller, identifierName, defaultValue)

    def serializeBoolean(self, caller, propertyName, defaultValue = False):
        """Read a named boolean property."""
        element = self.getChild(propertyName, True)
        identifierName = self.formatIdentifier(propertyName)
        if element:
            setattr(caller, identifierName, self.stringToBoolean(self.getNodeValue(element)))
        else:
            setattr(caller, identifierName, defaultValue)

    def serializeList(self, caller, vectorName, itemName, allowNone = False):
        """Read a list of elements."""
        identifierName = self.formatIdentifier(vectorName)
        vectorElement = self.getChild(vectorName, allowNone)
        if not vectorElement:
            setattr(caller, identifierName, None)
            return
        itemElements = vectorElement.getElementsByTagName(itemName)
        ret = []
        for itemElement in itemElements:
            ret.append(self.getNodeValue(itemElement))
        setattr(caller, identifierName, ret)

    def serializeDict(self, caller, dictName):
        """Read a named element in the document and set its values as
        attributes of the caller."""
        identifierName = self.formatIdentifier(dictName)
        dictElement = self.getChild(dictName)
        ret = {}
        for childNode in self.iterateChildren(dictElement):
            ret[childNode.nodeName] = self.getNodeValue(childNode)
        setattr(caller, identifierName, ret)

    def serializeObject(self, caller, objectClass):
        """Load a Serializable object."""
        objectElement = self.getChild(objectClass.__name__)
        objectInstance = objectClass()
        self.node_ = objectElement
        objectInstance.serialize(self)
        objectInstance.postSerialize()
        self.node_ = self.node_.parentNode
        identifierName = self.formatIdentifier(objectInstance.key())
        setattr(caller, identifierName, objectInstance)

    def serializeObjectList(self, caller, objectClass):
        """Load a list of Serializable objects."""
        listElement = self.getChild(objectClass.groupName_)
        itemElements = listElement.getElementsByTagName(objectClass.__name__)
        ret = []
        for itemElement in itemElements:
            objectInstance = objectClass()
            self.node_ = itemElement
            objectInstance.serialize(self)
            objectInstance.postSerialize()
            self.node_ = self.node_.parentNode.parentNode
            ret.append(objectInstance)
        identifierName = self.formatIdentifier(objectClass.groupName_)
        identifierCount = self.formatIdentifier(objectClass.__name__ + 'Count')
        setattr(caller, identifierName, ret)
        setattr(caller, identifierCount, len(ret))

    def serializeObjectDict(self, caller, objectClass):
        """Load a named element and write its data as a property of the caller.

        A client invokes this function as follows:
            serializer.serializeObjectDict(self, Foo)

        And after the call returns, the caller's dict is populated as follows:
            self.Foo - a dict of objects, keyed by object identifier
            self.FooKeys - a sorted list of the object identifiers

        These structures allow the caller to iterate through the objects
        in order by identifier."""

        dictElement = self.getChild(objectClass.groupName_)
        dict = {}
        keys = []
        for childNode in self.iterateChildren(dictElement):
            objectInstance = factory.Factory.instance().makeObject(childNode.nodeName)
            self.node_ = childNode
            objectInstance.serialize(self)
            objectInstance.postSerialize()
            self.node_ = self.node_.parentNode.parentNode
            dict[objectInstance.key()] = objectInstance
            keys.append(objectInstance.key())
        keys.sort()
        identifierName = self.formatIdentifier(objectClass.groupName_)
        identifierKeys = self.formatIdentifier(objectClass.__name__ + 'Keys')
        setattr(caller, identifierName, dict)
        setattr(caller, identifierKeys, keys)

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

        dictElement = self.getChild(objectClass.groupName_)
        itemElements = dictElement.getElementsByTagName(objectClass.__name__)
        for itemElement in itemElements:
            objectInstance = objectClass()
            self.node_ = itemElement
            objectInstance.serialize(self)
            objectInstance.postSerialize()
            self.node_ = self.node_.parentNode.parentNode
            identifierName = self.formatIdentifier(objectInstance.key())
            setattr(caller, identifierName, objectInstance)

    ################################################
    # private interface
    ################################################

    def getChild(self, tagName, allowNone = False):
        """Get single named child of current node."""
        for childNode in self.node_.childNodes:
            if childNode.nodeName == tagName:
                return childNode
        if not allowNone:
            raise exceptions.SerializationElementMissingException(self.documentName_, tagName)

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
            raise exceptions.SerializationConvertBooleanException(self.documentName_, str)

    def sub1(self, m):
        return m.group(1).lower() + m.group(2) + '_'

    REGEX_FORMAT_ID = re.compile(r'^(\w)(\w.*)$')

    def formatIdentifier(self, identifier):
        """Convert a general identifier to an identifier that complies
        to standards for a private variable name.

        This function accepts an identifier, converts the first character
        to lowercase, and suffixes an underscore.  For example the input
            VariableName
        yields the output
            variableName_"""
        return XmlReader.REGEX_FORMAT_ID.sub(self.sub1, identifier)

