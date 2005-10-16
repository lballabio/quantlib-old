
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

'xml parser'

import common
import xml.dom.minidom
import re
import os

# constants

PATTERN_XMLFILES = '(.*).xml'

# global variable

loadAllAsDict = False   # override default behavior of isListNode

# functions to derive node type
# by default a node with 0..n children of same name is treated as a list
# such nodes with 0 or 1 children could also be strings or dicts
# so the default can be overridden as follows:
#   loaded as list:
#       <node></node>
#   loaded as string:
#       <node>''</node>
#   loaded as dict:
#       <node loadasdict='true'></node>
#   loaded as list:
#       <node>
#           <item>val</item>
#       </node>
#   loaded as dict:
#       <node loadasdict='true'>
#           <item>val</item>
#       </node>
# or the attribute
#   loadallasdict='true'
# may appear anywhere in the doc to always override default behavior

def isEmptyNode(n):
    'return True for text node with neither children nor data'
    return n.nodeType == xml.dom.Node.TEXT_NODE \
    and not n.hasChildNodes() \
    and not n.nodeValue.strip()

def isTextNode(n):
    'return True for element node with single child text node'
    return n.nodeType == xml.dom.Node.ELEMENT_NODE \
    and len(n.childNodes) == 1 \
    and n.firstChild.nodeType == xml.dom.Node.TEXT_NODE

def isListNode(n):
    'return True for node containing 0..n children with same name'
    global loadAllAsDict
    if n.nodeType != xml.dom.Node.ELEMENT_NODE:
        return False
    nodeName = ''
    count = 0
    for c in n.childNodes:
        if isEmptyNode(c): continue
        count += 1
        if nodeName:
            if nodeName != c.nodeName: return False
        else:
            nodeName = c.nodeName
    if count < 3 and (loadAllAsDict or
        n.getAttribute(common.LOADASDICT) == 'true'):
        return False
    return True

def isDictNode(n):
    'return True for node whose children are all key/value pairs'
    if n.nodeType != xml.dom.Node.ELEMENT_NODE:
        return False
#    for c in n.childNodes:
#        if not isTextNode(c) and not isEmptyNode(c):
#            return False
    return True

def convertNode(n):
    'convert XML node into dict of strings/lists/dicts'
    global loadAllAsDict
    ret = {}
    if n.attributes:
        for key in n.attributes.keys():
            if key == common.LOADASDICT: continue
            if key == common.LOADALLASDICT:
                loadAllAsDict = True
                continue
            if not ret.has_key(common.ATTS): ret[common.ATTS] = {}
            ret[common.ATTS][key] = n.attributes[key].nodeValue
    for c in n.childNodes:
        if isTextNode(c):
            ret[c.nodeName] = c.firstChild.nodeValue
        elif isListNode(c):
            ret[c.nodeName] = []
            for cc in c.childNodes:
                if isTextNode(cc):
                    ret[c.nodeName].append(cc.firstChild.nodeValue)
                elif not isEmptyNode(cc):
                    ret[c.nodeName].append(convertNode(cc))
        elif isDictNode(c):
            ret[c.nodeName] = convertNode(c)
    return ret

def parseFile(fileName):
    'parse xml file into nested data structure'
    dom = xml.dom.minidom.parse(fileName + '.xml')
    return convertNode(dom.documentElement)

def parseFiles(fileNames):
    'parse list of xml files into nested data structure'
    ret = {}
    for fileName in fileNames:
        ret[fileName] = parseFile(fileName)
    return ret

