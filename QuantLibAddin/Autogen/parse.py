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

def getParameters(parmNodes):
	parmList = []
	for parmNode in parmNodes:
		parm = getParameter(parmNode)
		parmList.append(parm)
	return parmList

def getFunction(functionNode):
	function = {}
	nameNode = functionNode.getElementsByTagName(common.NAME)[0]
	descNode = functionNode.getElementsByTagName(common.DESC)[0]
	parmNodes = functionNode.getElementsByTagName(common.PARMS)
	name = getText(nameNode)
	desc = getText(descNode)
	parmList = getParameters(parmNodes)
	function[common.NAME] = name
	function[common.DESC] = desc
	function[common.PARMS] = parmList
	return function

def getFunctionList(functionNodes):
	functionList = []
	for functionNode in functionNodes:
		function = getFunction(functionNode)
		functionList.append(function)
	return functionList

def getFunctionLists():
	functionLists = {}
	xmlSuffix = re.compile('.*\.xml$')
	fileNames = os.listdir('.')
	for fileName in fileNames:
		if not xmlSuffix.match(fileName): continue
		listName = os.path.basename(fileName)
		fileObj = file(fileName)
		dom = xml.dom.minidom.parse(fileObj)
		functionNodes = dom.documentElement.getElementsByTagName(common.FUNC)
		functionList = getFunctionList(functionNodes)
		functionLists[listName] = functionList
	return functionLists

def debug(functionLists):
	for listName in functionLists.keys():
		print "list name =", listName
		functionList = functionLists[listName]
		for function in functionList:
			print "\tname =", function[common.NAME]
			print "\tdesc =", function[common.DESC]
			parms = function[common.PARMS]
			for parm in parms:
				print "\t\tname =", parm[common.NAME]
				print "\t\ttype =", parm[common.TYPE]
				print "\t\tdesc =", parm[common.DESC]
