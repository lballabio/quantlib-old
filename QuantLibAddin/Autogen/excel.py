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
	paramList = utils.generateParamList(function[common.PARAMS])
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
	return

def generateFuncDef(fileFunc, function):
	fileFunc.write('LPXLOPER %s(' % function[common.CODENAME])
	i = 0
	for param in function[common.PARAMS]:
		if param[common.TYPE] == 'string':
			type = 'char'
		else:
			type = param[common.TYPE]
		fileFunc.write('\n\t\t%s *%s' % (type, param[common.NAME]))
		i += 1
		if i < len(function[common.PARAMS]):
			fileFunc.write(',')
	fileFunc.write(') {\n')
	fileFunc.write('\ttry {\n')
	if function[common.HANDLE]:
		fileFunc.write('\t\tstd::string handle = getCaller();\n')
	fileFunc.write('\t\tProperties properties = %s(' % function[common.NAME])
	if function[common.HANDLE]:
		fileFunc.write('handle,')
	i = 0
	for param in function[common.PARAMS]:
		if param[common.TYPE] == 'string':
			fileFunc.write('\n\t\t\tstd::string(%s)' % param[common.NAME])
		else:
			fileFunc.write('\n\t\t\t*' + param[common.NAME])
		i += 1
		if i < len(function[common.PARAMS]):
			fileFunc.write(',')
	fileFunc.write(');\n')
	fileFunc.write('\t\tstatic XLOPER xRet;\n')
	fileFunc.write('\t\tsetValues(&xRet, properties, handle);\n')
	fileFunc.write('\t\treturn &xRet;\n')
	fileFunc.write('\t} catch (const exception &e) {\n')
	fileFunc.write('\t\tQL_LOGMESSAGE(std::string("ERROR: %s: ") + e.what());\n' \
		% function[common.NAME])
	fileFunc.write('\t\treturn 0;\n')
	fileFunc.write('\t}\n')
	fileFunc.write('}\n\n')
	return

def generateFuncDefs(functionGroups):
	for groupName in functionGroups.keys():
		functionGroup = functionGroups[groupName]
		if functionGroup[common.HDRONLY]:
			continue
		fileFunc = file(common.XL_ROOT + groupName + '.cpp', 'w')
		utils.printHeader(fileFunc)
		fileFunc.write('#include <QuantLibAddin/qladdin.hpp>\n')
		fileFunc.write('#include <Addins/Excel/utilities.hpp>\n')
		fileFunc.write('using namespace ObjHandler;\n')
		fileFunc.write('using namespace QuantLibAddin;\n\n')
		for function in functionGroup[common.FUNCLIST]:
			generateFuncDef(fileFunc, function)
		fileFunc.close()
	return

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
	return

def generate(functionDefs):
	generateFuncHeaders(functionDefs)
	generateFuncDefs(functionDefs[common.FUNCGROUPS])
	generateExports(functionDefs[common.FUNCGROUPS])
	return

