'output C source files'

import common
import utils

def generateFuncHeader(fileHeader, function, suffix):
	fileHeader.write('int %s_C(\n' % function[common.NAME])
	if function[common.HANDLE]:
		fileHeader.write('\t\tconst char *handle,\n')
	fileHeader.write(utils.generateParamList(function[common.PARAMS], \
		2, True, 'const ', 'char*', ''))
	fileHeader.write(',\n\t\tVariesList *result)%s\n' % suffix)

def generateFuncHeaders(groupName, functionGroup):
	fileHeader = file(common.C_ROOT + groupName + '.h', 'w')
	utils.printHeader(fileHeader)
	fileHeader.write('#ifndef %s_h\n' % groupName)
	fileHeader.write('#define %s_h\n\n' % groupName)
	for function in functionGroup[common.FUNCLIST]:
		generateFuncHeader(fileHeader, function, ';\n')
	fileHeader.write('#endif\n')
	fileHeader.close()

def generateFuncDefs(groupName, functionGroup):
	fileFunc = file(common.C_ROOT + groupName + '_c.cpp', 'w')
	utils.printHeader(fileFunc)
	fileFunc.write(common.C_INCLUDES % groupName)
	for function in functionGroup[common.FUNCLIST]:
		generateFuncHeader(fileFunc, function, ' {')
		paramList = utils.generateParamList(function[common.PARAMS], 3, False)
		if function[common.HANDLE]:
			handle = '\t\t\thandle,\n'
		else:
			handle = ''
		fileFunc.write(common.C_BODY % \
			(function[common.NAME], handle, paramList, function[common.NAME]))
	fileFunc.close()

def generate(functionDefs):
	functionGroups = functionDefs[common.FUNCGROUPS]
	for groupName in functionGroups.keys():
		functionGroup = functionGroups[groupName]
		if functionGroup[common.HDRONLY]:
			continue
		generateFuncHeaders(groupName, functionGroup)
		generateFuncDefs(groupName, functionGroup)
