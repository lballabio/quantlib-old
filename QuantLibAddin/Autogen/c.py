'output C source files'

import common
import utils

# constants

ROOT = common.ADDIN_ROOT + 'C/'
INCLUDES = 'stub.C.includes'
BODY = 'stub.C.body'

def generateFuncHeader(fileHeader, function, suffix):
    'generate source for prototype of given function'
    fileHeader.write('int %s(\n' % function[common.NAME])
    if function[common.CTOR]:
        fileHeader.write('        char* handle,\n')
    fileHeader.write(utils.generateParamList(function[common.PARAMS],
        2, True, convertString = 'char*', arrayCount = True,
        convertMatStr = 'char*'))
    fileHeader.write(',\n        VariesList *result)%s\n' % suffix)

def generateFuncHeaders(groupName, functionGroup):
    'generate source for function prototypes'
    fileName = ROOT + groupName + '.h'
    utils.logMessage('    generating file ' + fileName + '...')
    fileHeader = file(fileName, 'w')
    utils.printHeader(fileHeader)
    fileHeader.write('#ifndef qla_%s_h\n' % groupName)
    fileHeader.write('#define qla_%s_h\n\n' % groupName)
    for function in functionGroup[common.FUNCLIST]:
        generateFuncHeader(fileHeader, function, ';\n')
    fileHeader.write('#endif\n\n')
    fileHeader.close()

def generateFuncDefs(groupName, functionGroup):
    'generate source for function implementations'
    fileName = ROOT + groupName + '.cpp'
    utils.logMessage('    generating file ' + fileName + '...')
    fileFunc = file(fileName, 'w')
    utils.printHeader(fileFunc)
    bufInclude = utils.loadBuffer(INCLUDES)
    bufBody = utils.loadBuffer(BODY)
    fileFunc.write(bufInclude % groupName)
    for function in functionGroup[common.FUNCLIST]:
        generateFuncHeader(fileFunc, function, ' {')
        paramList = utils.generateParamList(function[common.PARAMS], 
            3, convertString = 'char*', arrayCount = True, convertMatStr = 'char*')
        if function[common.CTOR]:
            handle = 12 * ' ' + 'handle,\n'
            fName = 'QL_MAKE_OBJECT(%s)' % function[common.QLFUNC]
            fileFunc.write(bufBody % (fName, handle,
                paramList, function[common.NAME]))
        else:
            handle = ''
            fName = 'QuantLibAddin::' + function[common.NAME]
            fileFunc.write(bufBody % (fName, handle,
                paramList, function[common.NAME]))
    fileFunc.close()

def generate(functionDefs):
    'generate source code for C addin'
    utils.logMessage('  begin generating C ...')
    functionGroups = functionDefs[common.FUNCGROUPS]
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        if functionGroup[common.HDRONLY]:
            continue
        generateFuncHeaders(groupName, functionGroup)
        generateFuncDefs(groupName, functionGroup)
    utils.logMessage('  done generating C.')

