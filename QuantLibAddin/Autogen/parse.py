'xml parser'

import common
import xml.dom.minidom
import re
import os

def getText(parentNode):
	t = ""
	for node in parentNode.childNodes:
		if node.nodeType == node.TEXT_NODE:
			t = t + node.data
	return t

def getParameter(parmNode):
	parm = {}
	nameNode = parmNode.getElementsByTagName(common.NAME)[0]
	typeNode = parmNode.getElementsByTagName(common.TYPE)[0]
	descNode = parmNode.getElementsByTagName(common.DESC)[0]
	name = getText(nameNode)
	type = getText(typeNode)
	desc = getText(descNode)
	parm[common.NAME] = name
	parm[common.TYPE] = type
	parm[common.DESC] = desc
	return parm

def getParameters(parmsNode):
	parmList = []
	parmNodes = parmsNode.getElementsByTagName(common.PARM)
	for parmNode in parmNodes:
		parm = getParameter(parmNode)
		parmList.append(parm)
	return parmList

def getFunction(functionNode):
	function = {}
	nameNode = functionNode.getElementsByTagName(common.NAME)[0]
	codeNameNode = functionNode.getElementsByTagName(common.CODENAME)[0]
	descNode = functionNode.getElementsByTagName(common.DESC)[0]
	parmsNode = functionNode.getElementsByTagName(common.PARMS)[0]
	function[common.NAME] = getText(nameNode)
	function[common.CODENAME] = getText(codeNameNode)
	function[common.DESC] = getText(descNode)
	function[common.PARMS] = getParameters(parmsNode)
	return function

def getFunctionList(functionNodes):
	functionList = []
	for functionNode in functionNodes:
		function = getFunction(functionNode)
		functionList.append(function)
	return functionList

def getFunctionLists():
	functionLists = {}
	xmlSuffix = re.compile(r'(.*).xml\Z')
	fileNames = os.listdir('.')
	for fileName in fileNames:
		m = xmlSuffix.match(fileName)
		if not m: continue
		listName = m.group(1)
		fileObj = file(fileName)
		dom = xml.dom.minidom.parse(fileObj)
		functionNodes = dom.documentElement.getElementsByTagName(common.FUNC)
		functionList = getFunctionList(functionNodes)
		functionLists[listName] = functionList
		common.NUMFUNC += len(functionList)
	return functionLists
