'output excel source files'

import common
import utils

def generateParamString(function):
	paramStr = '" R'
	for param in function[common.PARAMS]:
		type = param[common.TYPE]
		if type == 'string':
			paramStr += 'C'
		elif type == 'double':
			paramStr += 'E'
		elif type == 'long':
			paramStr += 'N'
		else:
			raise ValueError, 'unknown datatype: ' + type
	if function[common.HANDLE]:
		paramStr += '#'
	paramStr += '",'
	return paramStr

def generateFuncDec(function):
	paramStr = generateParamString(function)
	paramList = utils.generateParamList(function[common.PARAMS], \
		0, False, '', '', '', '', '')
	if len(paramList) >= 255:
		raise ValueError, 'list of parameter names exceeds max Excel length of 255:\n' \
			+ paramList
	paramList = '" ' + paramList + '",'
	codeName = '" %s",' % function[common.CODENAME]
	funcName = '" %s",' % function[common.NAME]
	ret = '\t{ %-21s %-12s %s \n\t\t%s " 1", " QuantLib"},\n' \
		% (codeName, paramStr, funcName, paramList)
	return ret

def generateFuncHeaders(functionDefs):
	fileHeader = file(common.XL_ROOT + common.XL_FUNC, 'w')
	utils.printHeader(fileHeader)
	fileHeader.write('#define NUM_FUNCS %d\n' % functionDefs[common.NUMFUNC])
	fileHeader.write('#define NUM_ATTS 6\n\n')
	fileHeader.write('static LPSTR func[NUM_FUNCS][NUM_ATTS] = {\n')
	functionGroups = functionDefs[common.FUNCGROUPS]
	for groupName in functionGroups.keys():
		fileHeader.write('\t// %s\n' % groupName)
		for function in functionGroups[groupName][common.FUNCLIST]:
			fileHeader.write(generateFuncDec(function))
	fileHeader.write('};\n')
	fileHeader.close()

def generateFuncDef(fileFunc, function):
	paramList1 = utils.generateParamList(function[common.PARAMS], \
		2, True, '', 'char', '', '', '\n', '*')
	paramList2 = utils.generateParamList(function[common.PARAMS], \
		2, False, '', '', '', 'std::string(%s)', '\n', '*')
	if function[common.HANDLE]:
		handle1 = '\t\tstd::string handle = getCaller();\n'
		handle2 = '\t\tstd::string(handle),\n'
	else:
		handle1 = ''
		handle2 = ''
	fileFunc.write(common.XL_SOURCE % \
		(function[common.CODENAME], paramList1, handle1,\
		function[common.NAME], handle2, paramList2, function[common.NAME]))

def generateFuncDefs(functionGroups):
	for groupName in functionGroups.keys():
		functionGroup = functionGroups[groupName]
		if functionGroup[common.HDRONLY]:
			continue
		fileFunc = file(common.XL_ROOT + groupName + '.cpp', 'w')
		utils.printHeader(fileFunc)
		fileFunc.write(common.XL_INCLUDE)
		for function in functionGroup[common.FUNCLIST]:
			generateFuncDef(fileFunc, function)
		fileFunc.close()

def generateExports(functionGroups):
	fileExps = file(common.XL_ROOT + common.EXPORTFILE, 'w')
	utils.printTimeStamp(fileExps, ';')
	fileExps.write(common.EXPORTHEADER)
	for groupName in functionGroups.keys():
		fileExps.write(';\t%s\n\n' % groupName)
		functionGroup = functionGroups[groupName]
		for function in functionGroup[common.FUNCLIST]:
			fileExps.write('\t%s\n' % function[common.CODENAME])
		fileExps.write('\n')

def generate(functionDefs):
	generateFuncHeaders(functionDefs)
	generateFuncDefs(functionDefs[common.FUNCGROUPS])
	generateExports(functionDefs[common.FUNCGROUPS])

