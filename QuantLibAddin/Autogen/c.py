'output C source files'

import common
import utils

# constants

ROOT = common.ADDIN_ROOT + 'C/'
INCLUDES = 'stub.C.includes'
BODY = 'stub.C.body'

def generateFuncHeader(fileHeader, function, suffix):
    fileHeader.write('int %s_C(\n' % function[common.NAME])
    if function[common.CTOR]:
        fileHeader.write('        const char *handle,\n')
    fileHeader.write(utils.generateParamList(function[common.PARAMS],
        2, True, 'const ', 'char*'))
    fileHeader.write(',\n        VariesList *result)%s\n' % suffix)

def generateFuncHeaders(groupName, functionGroup):
    fileName = ROOT + groupName + '.h'
    utils.logMessage('    generating file ' + fileName + '...')
    fileHeader = file(fileName, 'w')
    utils.printHeader(fileHeader)
    fileHeader.write('#ifndef %s_h\n' % groupName)
    fileHeader.write('#define %s_h\n\n' % groupName)
    for function in functionGroup[common.FUNCLIST]:
        generateFuncHeader(fileHeader, function, ';\n')
    fileHeader.write('#endif\n')
    fileHeader.close()

def generateFuncDefs(groupName, functionGroup):
    fileName = ROOT + groupName + '_c.cpp'
    utils.logMessage('    generating file ' + fileName + '...')
    fileFunc = file(fileName, 'w')
    utils.printHeader(fileFunc)
    bufInclude = utils.loadBuffer(INCLUDES)
    bufBody = utils.loadBuffer(BODY)
    fileFunc.write(bufInclude % groupName)
    for function in functionGroup[common.FUNCLIST]:
        generateFuncHeader(fileFunc, function, ' {')
        paramList = utils.generateParamList(function[common.PARAMS], 3)
        if function[common.CTOR]:
            handle = 12 * ' ' + 'handle,\n'
        else:
            handle = ''
        fileFunc.write(bufBody %
            (function[common.NAME], handle, paramList, function[common.NAME]))
    fileFunc.close()

def generate(functionDefs):
    utils.logMessage('  begin generating C ...')
    functionGroups = functionDefs[common.FUNCGROUPS]
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        if functionGroup[common.HDRONLY]:
            continue
        generateFuncHeaders(groupName, functionGroup)
        generateFuncDefs(groupName, functionGroup)
    utils.logMessage('  done generating C.')

