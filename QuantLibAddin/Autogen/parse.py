
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

def isDictNode(n):
    'return True for node whose children are all key/value pairs'
    if n.nodeType != xml.dom.Node.ELEMENT_NODE:
        return False
    for c in n.childNodes:
        if not isTextNode(c) and not isEmptyNode(c):
            return False
    return True

def isListNode(n):
    'return True for node containing 0..n children with same name'
    if n.nodeType != xml.dom.Node.ELEMENT_NODE:
        return False
    nodeName = ''
    for c in n.childNodes:
        if isEmptyNode(c): continue
        if nodeName:
            if nodeName != c.nodeName: return False
        else:
            nodeName = c.nodeName
    return True

def convertNode(n):
    'convert XML node into dict of strings/lists/dicts'
    ret = {}
    if n.attributes:
        ret[common.ATTS] = {}
        for key in n.attributes.keys():
            ret[common.ATTS][key] = n.attributes[key].nodeValue
    for c in n.childNodes:
        if isTextNode(c):
            ret[c.nodeName] = c.firstChild.nodeValue
        elif isDictNode(c):
            ret[c.nodeName] = convertNode(c)
        elif isListNode(c):
            ret[c.nodeName] = []
            for cc in c.childNodes:
                if not isEmptyNode(cc):
                    ret[c.nodeName].append(convertNode(cc))
    return ret

def parseFiles(patternMatch, patternExclude = ''):
    'parse list of xml files into nested data structure'
    ret = {}
    fileNames = os.listdir('.')
    for fileName in fileNames:
        if not re.match(patternMatch, fileName): continue
        if patternExclude and re.match(patternExclude, fileName): continue
        matchName = re.match(PATTERN_XMLFILES, fileName)
        if not matchName: continue
        groupName = matchName.group(1)
        dom = xml.dom.minidom.parse(fileName)
        ret[groupName] = convertNode(dom.documentElement)
    return ret

