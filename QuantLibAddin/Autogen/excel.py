'output excel source files'

import common
import utils

def generateParamString(function):
    'generate string to register function parameters'
    paramStr = 'R'
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
    if function[common.CTOR]:
        paramStr += '#'
    return paramStr

def generateFuncRegister(fileHeader, function):
    'generate call to xlfRegister for given function'
    params = function[common.PARAMS]
    numParams = len(params)
    numParamsTotal = numParams + 11 # 11 extra params to register the function
    # FIXME validation below to be moved into parse.py
    if numParamsTotal > common.XL_MAXPARAM:
        raise ValueError, common.XL_MAXPARMERR % common.XL_MAXPARAM
    paramStr = generateParamString(function)
    paramList = utils.generateParamList(params, suffix = '')
    if len(paramList) >= common.XL_MAXLEN:
        raise ValueError, common.XL_MAXLENERR % (common.XL_MAXLEN, paramList)
    regLine = '        TempStr(" %s"),\n'
    fileHeader.write('    Excel(xlfRegister, 0, %d, &xDll,\n' % numParamsTotal)
    fileHeader.write(regLine % function[common.CODENAME])
    fileHeader.write(regLine % paramStr)
    fileHeader.write(regLine % function[common.NAME])
    fileHeader.write(regLine % paramList)
    fileHeader.write(regLine % '1')
    fileHeader.write(regLine % 'QuantLib')
    fileHeader.write(regLine % '')
    fileHeader.write(regLine % '')
    fileHeader.write(regLine % function[common.DESC])
    fileHeader.write(regLine % function[common.CODENAME])
    i = 0
    for param in params:
        fileHeader.write('        TempStr(" ' + param[common.DESC] + '")')
        i += 1
        if i < numParams:
            fileHeader.write(',\n')
    fileHeader.write(');\n\n')

def generateFuncRegisters(functionDefs):
    'generate source code to register functions'
    fileName = common.XL_ROOT + common.XL_ADDIN
    utils.logMessage('    generating file ' + fileName + '...')
    fileHeader = file(fileName, 'w')
    utils.printHeader(fileHeader)
    bufRegHead = utils.loadBuffer(common.XL_REGHEAD)
    bufRegFoot = utils.loadBuffer(common.XL_REGFOOT)
    fileHeader.write(bufRegHead)
    functionGroups = functionDefs[common.FUNCGROUPS]
    for groupName in functionGroups.keys():
        fileHeader.write('    // %s\n\n' % groupName)
        for function in functionGroups[groupName][common.FUNCLIST]:
            generateFuncRegister(fileHeader, function)
    fileHeader.write(bufRegFoot)
    fileHeader.close()

def generateFuncDef(fileFunc, function):
    'generate source code for body of given function'
    paramList1 = utils.generateParamList(function[common.PARAMS],
        2, True, '', 'char', dereference = '*')
    paramList2 = utils.generateParamList(function[common.PARAMS],
        3, reformatString = 'std::string(%s)', dereference = '*')
    if function[common.CTOR]:
        handle1 = 8 * ' ' + 'std::string handle = getCaller();\n'
        handle2 = 12 * ' ' + 'handle,\n'
    else:
        handle1 = ''
        handle2 = ''
    fileFunc.write(common.XL_BODY_BUF %
        (function[common.CODENAME], paramList1, handle1,
        function[common.NAME], handle2, paramList2, function[common.NAME]))

def generateFuncDefs(functionGroups):
    'generate source code for function bodies'
    common.XL_BODY_BUF = utils.loadBuffer(common.XL_BODY)
    bufExcelIncludes = utils.loadBuffer(common.XL_INCLUDES)
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        if functionGroup[common.HDRONLY]:
            continue
        fileName = common.XL_ROOT + groupName + '.cpp'
        utils.logMessage('    generating file ' + fileName + '...')
        fileFunc = file(fileName, 'w')
        utils.printHeader(fileFunc)
        fileFunc.write(bufExcelIncludes)
        for function in functionGroup[common.FUNCLIST]:
            generateFuncDef(fileFunc, function)
        fileFunc.close()

def generate(functionDefs):
    'generate source code for Excel addin'
    utils.logMessage('  begin generating Excel ...')
    generateFuncRegisters(functionDefs)
    generateFuncDefs(functionDefs[common.FUNCGROUPS])
    utils.logMessage('  done generating Excel.')
