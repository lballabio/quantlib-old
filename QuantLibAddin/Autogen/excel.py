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

def generateFuncDec(fileHeader, function):
	# FIXME validation below to be moved into parse.py
	if len(function[common.PARAMS]) > common.XLMAXPARAM:
		raise ValueError, \
			'number of parameters exceeds Excel max of %d' % common.XLMAXPARAM
	paramStr = generateParamString(function)
	paramList = utils.generateParamList(function[common.PARAMS], \
		0, False, '', '', '', '', '')
	if len(paramList) >= 255:
		raise ValueError, 'list of parameter names exceeds max Excel length of 255:\n' \
			+ paramList
	paramList =    '" ' + paramList                 + '",'
	funcCodeName = '" ' + function[common.CODENAME] + '",'
	funcName =     '" ' + function[common.NAME]     + '",'
	funcDesc =     '" ' + function[common.DESC]     + '",'
	fileHeader.write(common.FUNCDEC % (\
		funcCodeName, paramStr, funcName, paramList, funcDesc))
	i = 0
	for param in function[common.PARAMS]:
		paramDesc = '" ' + param[common.DESC] + '",'
		fileHeader.write('        %-30s// param %d\n' % (paramDesc, i))
		i+=1
	fileHeader.write('        // unused params:\n        ')
	while i < common.XLMAXPARAM:
		fileHeader.write('" "')
		if i < common.XLMAXPARAM - 1:
			fileHeader.write(', ')
		i+=1
	fileHeader.write('\n    },\n\n')

def generateFuncHeaders(functionDefs):
	fileName = common.XL_ROOT + common.XL_FUNC
	utils.logMessage('    generating file ' + fileName + '...')
	fileHeader = file(fileName, 'w')
	utils.printHeader(fileHeader)
	fileHeader.write('#define NUM_FUNCS %d\n' % functionDefs[common.NUMFUNC])
	fileHeader.write('#define NUM_ATTS %d\n\n' % (common.XLMAXPARAM + 9))
	fileHeader.write('static LPSTR func[NUM_FUNCS][NUM_ATTS] = {\n')
	functionGroups = functionDefs[common.FUNCGROUPS]
	for groupName in functionGroups.keys():
		fileHeader.write('    // %s\n' % groupName)
		for function in functionGroups[groupName][common.FUNCLIST]:
			generateFuncDec(fileHeader, function)
	fileHeader.write('};\n')
	fileHeader.close()

def generateFuncDef(fileFunc, function):
	paramList1 = utils.generateParamList(function[common.PARAMS], \
		2, True, '', 'char', '', '', '\n', '*')
	paramList2 = utils.generateParamList(function[common.PARAMS], \
		3, False, '', '', '', 'std::string(%s)', '\n', '*')
	if function[common.HANDLE]:
		handle1 = 8 * ' ' + 'std::string handle = getCaller();\n'
		handle2 = 12 * ' ' + 'std::string(handle),\n'
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
		fileName = common.XL_ROOT + groupName + '.cpp'
		utils.logMessage('    generating file ' + fileName + '...')
		fileFunc = file(fileName, 'w')
		utils.printHeader(fileFunc)
		fileFunc.write(common.XL_INCLUDE)
		for function in functionGroup[common.FUNCLIST]:
			generateFuncDef(fileFunc, function)
		fileFunc.close()

def generateExports(functionGroups):
	fileName = common.XL_ROOT + common.EXPORTFILE
	utils.logMessage('    generating file ' + fileName + '...')
	fileExps = file(fileName, 'w')
	utils.printTimeStamp(fileExps, ';')
	fileExps.write(common.EXPORTHEADER)
	for groupName in functionGroups.keys():
		fileExps.write(';    %s\n\n' % groupName)
		functionGroup = functionGroups[groupName]
		for function in functionGroup[common.FUNCLIST]:
			fileExps.write('    %s\n' % function[common.CODENAME])
		fileExps.write('\n')

def generate(functionDefs):
	utils.logMessage('  begin generating Excel ...')
	generateFuncHeaders(functionDefs)
	generateFuncDefs(functionDefs[common.FUNCGROUPS])
	generateExports(functionDefs[common.FUNCGROUPS])
	utils.logMessage('  done generating Excel.')

