'output excel source files'

import common
import utils

def generateParmString(parmList):
	parmStr = '" R'
	for parm in parmList:
		type = parm[common.TYPE]
		if type == 'string':
			parmStr += 'C'
		elif type == 'double':
			parmStr += 'E'
		elif type == 'long':
			parmStr += 'N'
		elif type == 'handle':
			parmStr += ''
		else:
			raise ValueError, 'unknown datatype: ' + type
	parmStr += '",'
	return parmStr

def generateFuncDef(function):
	parmStr = generateParmString(function[common.PARMS])
	codeName = '" %s",' % function[common.CODENAME]
	funcName = '" %s",' % function[common.NAME]
	ret = '\t{ %-21s %-12s %-24s " ", " 1", " QuantLib"},\n' \
	% (codeName, parmStr, funcName)
	return ret

def generateFuncDefs(functionLists):
	fileFunc = file(common.XL_ROOT + common.XL_FUNC, 'w')
	fileFunc.write(common.CR_BUFFER)
	fileFunc.write(utils.timeStamp())
	fileFunc.write('#define NUM_FUNCS %d\n' % common.NUMFUNC)
	fileFunc.write('#define NUM_ATTS 6\n\n')
	fileFunc.write('static LPSTR func[NUM_FUNCS][NUM_ATTS] = {\n')
	for funcName in functionLists.keys():
		fileFunc.write('\t// %s\n' % funcName)
		for function in functionLists[funcName]:
			fileFunc.write(generateFuncDef(function))
	fileFunc.write('};\n')
	fileFunc.close()
	return

def generate(functionLists):
	generateFuncDefs(functionLists)
	return
