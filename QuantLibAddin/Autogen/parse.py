'xml parser'

import common
import xml.dom.minidom
import re
import os

def getText(parentNode):
    t = ''
    if parentNode != None:
        for node in parentNode.childNodes:
            if node.nodeType == node.TEXT_NODE:
                t = t + node.data
    return t

def getBoolean(parentNode):
    t = getText(parentNode).upper()
    if t == 'TRUE':
        return True
    elif t == 'FALSE':
        return False
    else:
        raise ValueError, 'invalid boolean: ' + t

def getParameter(paramNode):
    param = {}
    nameNode = paramNode.getElementsByTagName(common.NAME)[0]
    typeNode = paramNode.getElementsByTagName(common.TYPE)[0]
    tensorNode = paramNode.getElementsByTagName(common.TENSOR)[0]
    classNode = typeNode.getAttributeNode(common.CLASS)
    descNode = paramNode.getElementsByTagName(common.DESC)[0]
    param[common.NAME] = getText(nameNode)
    param[common.TYPE] = getText(typeNode)
    param[common.TENSOR] = getText(tensorNode)
    param[common.DESC] = getText(descNode)
    param[common.CLASS] = getText(classNode)
    return param

def getParameters(paramsNode):
    paramList = []
    paramNodes = paramsNode.getElementsByTagName(common.PARAM)
    for paramNode in paramNodes:
        param = getParameter(paramNode)
        paramList.append(param)
    return paramList

def getReturnVal(retvalNode):
    returnVal = {}
    typeNode = retvalNode.getElementsByTagName(common.TYPE)[0]
    descNode = retvalNode.getElementsByTagName(common.DESC)[0]
    returnVal[common.TYPE] = getText(typeNode)
    returnVal[common.DESC] = getText(descNode)
    return returnVal

def getFunction(functionNode):
    function = {}
    nameNode = functionNode.getElementsByTagName(common.NAME)[0]
    codeNameNode = functionNode.getElementsByTagName(common.CODENAME)[0]
    descNode = functionNode.getElementsByTagName(common.DESC)[0]
    qlfNode = functionNode.getElementsByTagName(common.QLFUNC)[0]
    handleNode = functionNode.getElementsByTagName(common.CTOR)[0]
    paramsNode = functionNode.getElementsByTagName(common.PARAMS)[0]
    retvalNode = functionNode.getElementsByTagName(common.RETVAL)[0]
    function[common.NAME] = getText(nameNode)
    function[common.CODENAME] = getText(codeNameNode)
    function[common.DESC] = getText(descNode)
    function[common.QLFUNC] = getText(qlfNode)
    function[common.CTOR] = getBoolean(handleNode)
    function[common.PARAMS] = getParameters(paramsNode)
    function[common.RETVAL] = getReturnVal(retvalNode)
    return function

def getFunctionGroup(doc):
    functionGroup = {}
    functionList = []
    functionNodes = doc.getElementsByTagName(common.FUNC)
    for functionNode in functionNodes:
        function = getFunction(functionNode)
        functionList.append(function)
    hdronlyNode = doc.getElementsByTagName(common.HDRONLY)[0]
    functionGroup[common.FUNCLIST] = functionList
    functionGroup[common.HDRONLY] = getBoolean(hdronlyNode)
    return functionGroup

def getFunctionDefs():
    functionDefs = {}
    functionGroups = {}
    fileNames = os.listdir('.')
    functionDefs[common.NUMFUNC] = 0
    for fileName in fileNames:
        matchName = re.match(common.XMLSUFFIX, fileName)
        if not matchName: continue
        groupName = matchName.group(1)
        fileObj = file(fileName)
        dom = xml.dom.minidom.parse(fileObj)
        functionGroups[groupName] = getFunctionGroup(dom.documentElement)
        functionDefs[common.NUMFUNC] += len(functionGroups[groupName][common.FUNCLIST])
    functionDefs[common.FUNCGROUPS] = functionGroups
    return functionDefs

