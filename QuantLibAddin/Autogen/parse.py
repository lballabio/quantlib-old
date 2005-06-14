'xml parser'

import common
import xml.dom.minidom
import re
import os

# global constants

XMLSUFFIX = r'(.*).xml\Z'

def getText(parentNode):
    'convert a node to a string'
    t = ''
    if parentNode != None:
        for node in parentNode.childNodes:
            if node.nodeType == node.TEXT_NODE:
                t += node.data
    return t

def getBoolean(parentNode):
    'convert a node to a boolean value'
    t = getText(parentNode).upper()
    if t == 'TRUE':
        return True
    elif t == 'FALSE':
        return False
    else:
        raise ValueError, 'invalid boolean: ' + t

def getParameter(paramNode):
    'derive the definition for the given parameter'
    param = {}
    nameNode   = paramNode.getElementsByTagName(common.NAME)[0]
    typeNode   = paramNode.getElementsByTagName(common.TYPE)[0]
    tensorNode = paramNode.getElementsByTagName(common.TENSOR)[0]
    classNode  = typeNode.getAttributeNode(common.CLASS)
    ignoreNode = nameNode.getAttributeNode(common.IGNORE)
    descNode   = paramNode.getElementsByTagName(common.DESC)[0]
    param[common.NAME]   = getText(nameNode)
    param[common.TYPE]   = getText(typeNode)
    param[common.TENSOR] = getText(tensorNode)
    param[common.DESC]   = getText(descNode)
    param[common.CLASS]  = getText(classNode)
    param[common.IGNORE] = getText(ignoreNode)
    return param

def getParameters(paramsNode):
    'derive the definition for a list of parameters'
    paramList = []
    paramNodes = paramsNode.getElementsByTagName(common.PARAM)
    for paramNode in paramNodes:
        param = getParameter(paramNode)
        paramList.append(param)
    return paramList

def getReturnVal(retvalNode):
    'derive the definition for the given return value'
    returnVal  = {}
    typeNode   = retvalNode.getElementsByTagName(common.TYPE)[0]
    tensorNode = retvalNode.getElementsByTagName(common.TENSOR)[0]
    descNode   = retvalNode.getElementsByTagName(common.DESC)[0]
    returnVal[common.TYPE]   = getText(typeNode)
    returnVal[common.TENSOR] = getText(tensorNode)
    returnVal[common.DESC]   = getText(descNode)
    return returnVal

def getFunction(functionNode):
    'derive the definition for the given function'
    function = {}
    nameNode     = functionNode.getElementsByTagName(common.NAME)[0]
    codeNameNode = functionNode.getElementsByTagName(common.CODENAME)[0]
    descNode     = functionNode.getElementsByTagName(common.DESC)[0]
    handleNode   = functionNode.getElementsByTagName(common.CTOR)[0]
    qlfNode      = functionNode.getElementsByTagName(common.QLFUNC)[0]
    paramsNode   = functionNode.getElementsByTagName(common.PARAMS)
    if paramsNode:
        function[common.PARAMS] = getParameters(paramsNode[0])
    else:
        function[common.PARAMS] = ''
    retvalNode = functionNode.getElementsByTagName(common.RETVAL)[0]
    function[common.NAME]     = getText(nameNode)
    function[common.CODENAME] = getText(codeNameNode)
    function[common.DESC]     = getText(descNode)
    function[common.QLFUNC]   = getText(qlfNode)
    function[common.CTOR]     = getBoolean(handleNode)
    function[common.RETVAL]   = getReturnVal(retvalNode)
    return function

def getFunctionGroup(doc):
    'derive the definition for a group of functions'
    functionGroup = {}
    functionList = []
    functionNodes = doc.getElementsByTagName(common.FUNC)
    for functionNode in functionNodes:
        function = getFunction(functionNode)
        functionList.append(function)
    hdronlyNode = doc.getElementsByTagName(common.HDRONLY)[0]
    descNode    = doc.getElementsByTagName(common.DESC)[0]
    displayNode = doc.getElementsByTagName(common.DISPLAYNAME)[0]
    functionGroup[common.FUNCLIST]    = functionList
    functionGroup[common.HDRONLY]     = getBoolean(hdronlyNode)
    functionGroup[common.DESC]        = getText(descNode)
    functionGroup[common.DISPLAYNAME] = getText(displayNode)
    return functionGroup

def getFunctionDefs():
    'parse function metadata into the functionDefs dict'
    functionDefs = {}
    functionGroups = {}
    fileNames = os.listdir('.')
    functionDefs[common.NUMFUNC] = 0
    for fileName in fileNames:
        matchName = re.match(XMLSUFFIX, fileName)
        if not matchName: continue
        groupName = matchName.group(1)
        fileObj = file(fileName)
        dom = xml.dom.minidom.parse(fileObj)
        functionGroups[groupName] = getFunctionGroup(dom.documentElement)
        functionDefs[common.NUMFUNC] += len(functionGroups[groupName][common.FUNCLIST])
    functionDefs[common.FUNCGROUPS] = functionGroups
    return functionDefs


