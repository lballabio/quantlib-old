'output QuantLibAddin source files'

import common
import utils

# constants

ROOT = '../qla/functions/'
INCLUDES = 'stub.qla.includes'
HEADER = 'stub.qla.header'
CTOR = 'stub.qla.constructor'
FUNC = 'stub.qla.function'
CONV = 'stub.qla.conversion'

def generateFuncHeader(fileHeader, function, suffix):
    'generate function prototype'
    fileHeader.write('    const ObjHandler::Properties& %s(\n'
        % function[common.NAME])
    if function[common.CTOR]:
        fileHeader.write('            const std::string &handleObject,\n')
    fileHeader.write(utils.generateParamList(function[common.PARAMS],
        3, True, 'const ', 'std::string', dereference = '&'))
    fileHeader.write(')%s\n' % suffix)

def generateFuncHeaders(groupName, functionGroup):
    'generate function prototypes'
    fileName = ROOT + groupName + '.hpp'
    utils.logMessage('    generating file ' + fileName + '...')
    fileHeader = file(fileName, 'w')
    utils.printHeader(fileHeader)
    bufInclude = utils.loadBuffer(INCLUDES)
    fileHeader.write(bufInclude % (groupName, groupName))
    for function in functionGroup[common.FUNCLIST]:
        generateFuncHeader(fileHeader, function, ';\n')
    fileHeader.write('}\n\n#endif\n')
    fileHeader.close()

def generateFuncDef(function, body, fileFunc):
    'generate source code for body of function'
    firstParam = function[common.PARAMS][0]
    paramName = firstParam[common.NAME]
    paramClass = firstParam[common.CLASS]
    paramList = utils.generateParamList(function[common.PARAMS], 
        suffix = ' ', skipFirst = True)
    fileFunc.write(body %
        (paramClass, paramClass, paramName, 
         function[common.NAME], paramName,
         function[common.QLFUNC], paramList))

def generateConversions(function, conv):
    'generate code to convert handles into objects'
    conversions = ''
    for param in function[common.PARAMS]:
        if param[common.CLASS] == '':
            continue
        varName = common.HANDLE + param[common.CLASS]
        conversions += conv % (
            param[common.CLASS],
            varName,
            param[common.CLASS],
            param[common.NAME],
            varName,
            function[common.NAME],
            param[common.NAME])
    return conversions

def generateCtorDef(function, body, conv, fileFunc):
    'generate source code for body of constructor'
    conversions = generateConversions(function, conv)
    paramList = utils.generateParamList(function[common.PARAMS], 
        4, xlateNames = True)
    fileFunc.write(body %
        (conversions, function[common.QLFUNC], paramList))

def generateFuncDefs(groupName, functionGroup):
    'generate source code for function bodies'
    fileName = ROOT + groupName + '.cpp'
    utils.logMessage('    generating file ' + fileName + '...')
    fileFunc = file(fileName, 'w')
    utils.printHeader(fileFunc)
    bufHeader = utils.loadBuffer(HEADER)
    bufCtor = utils.loadBuffer(CTOR)
    bufFunc = utils.loadBuffer(FUNC)
    bufConv = utils.loadBuffer(CONV)
    fileFunc.write(bufHeader % (groupName, groupName))
    for function in functionGroup[common.FUNCLIST]:
        generateFuncHeader(fileFunc, function, ' {')
        if function[common.CTOR]:
            generateCtorDef(function, bufCtor, bufConv, fileFunc)
        else:
            generateFuncDef(function, bufFunc, fileFunc)
    fileFunc.write('}\n\n')
    fileFunc.close()

def generate(functionDefs):
    'generate source code for QuantLibAddin functions'
    utils.logMessage('  begin generating QuantLibAddin ...')
    functionGroups = functionDefs[common.FUNCGROUPS]
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        if functionGroup[common.HDRONLY]:
            continue
        generateFuncHeaders(groupName, functionGroup)
        generateFuncDefs(groupName, functionGroup)
    utils.logMessage('  done generating QuantLibAddin.')

