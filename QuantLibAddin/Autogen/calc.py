'output calc source files'

import common
import utils

# constants

ROOT = common.ADDIN_ROOT + 'Calc/'
MAPFILE = 'funcdef.cpp'
MAPLINE='    funcMap[ STRFROMANSI( "%s" ) ]\n\
        =  STRFROMANSI( "%s" );\n'
AUTOHDR = 'autogen.hpp'
IDL = 'QuantLibAddin.idl'
MAP = 'stub.Calc.map'
INCLUDES = 'stub.Calc.includes'
BODY = 'stub.Calc.body'
IDL_HEAD = 'stub.Calc.idlhead'
IDL_FOOT = 'stub.Calc.idlfoot'
IDL_FUNC = 'stub.Calc.idlfunc'

def generateFuncMap(functionGroups):
    fileName = ROOT + MAPFILE
    utils.logMessage('    generating file ' + fileName + '...')
    fileMap = file(fileName, 'w')
    utils.printHeader(fileMap)
    bufCalcMap = utils.loadBuffer(MAP)
    fileMap.write(bufCalcMap)
    for groupName in functionGroups.keys():
        fileMap.write('\n    //%s\n\n' % groupName)
        functionGroup = functionGroups[groupName]
        for function in functionGroup[common.FUNCLIST]:
            fileMap.write(MAPLINE
                % (function[common.CODENAME], function[common.NAME]))
    fileMap.write('\n}\n')
    fileMap.close()

def generateAutoHeader(functionGroups):
    fileName = ROOT + AUTOHDR
    utils.logMessage('    generating file ' + fileName + '...')
    fileHeader = file(fileName, 'w')
    utils.printHeader(fileHeader)
    for groupName in functionGroups.keys():
        fileHeader.write('#include <Addins/Calc/%s.hpp>\n' % groupName)
    fileHeader.write('\n')
    fileHeader.close()

def generateHeader(fileHeader, function, suffix):
    if function[common.CTOR]:
        fileHeader.write('        const STRING & handle,\n')
    fileHeader.write(utils.generateParamList(function[common.PARAMS],
        2, True, '', 'const STRING &', 'sal_Int32'))
    fileHeader.write(') THROWDEF_RTE_IAE%s\n' % suffix)

def getReturnTypeCalc(function):
    returnType = function[common.RETVAL][common.TYPE]
    if returnType == common.PROPVEC:
        return 'SEQSEQ(ANY)'
    elif returnType == 'string':
        return 'STRING'
    else:
        raise ValueError, 'unexpected return type: ' + returnType

def generateHeaders(functionGroups):
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        fileName = ROOT + groupName + '.hpp'
        utils.logMessage('    generating file ' + fileName + '...')
        fileHeader = file(fileName, 'w')
        utils.printHeader(fileHeader)
        for function in functionGroup[common.FUNCLIST]:
            returnTypeCalc = getReturnTypeCalc(function)
            fileHeader.write('    virtual %s SAL_CALL %s(\n'
                % (returnTypeCalc, function[common.CODENAME]))
            generateHeader(fileHeader, function, ';')
            fileHeader.write('\n')
    fileHeader.close()

def generateFuncSource(fileFunc, function, bufBody):
    fileFunc.write('SEQSEQ( ANY ) SAL_CALL QLAddin::%s(\n' 
        % function[common.CODENAME])
    generateHeader(fileFunc, function, ' {')
    if function[common.CTOR]:
        handle = '\n' + 12 * ' ' + 'OUStringToString(handle),'
    else:
        handle = ''
    paramList = utils.generateParamList(function[common.PARAMS],
        3, False, reformatString = 'OUStringToString(%s)')
    fileFunc.write(bufBody % (
        function[common.NAME],
        handle,
        paramList,
        function[common.NAME]))

def generateFuncSources(functionGroups):
    bufInclude = utils.loadBuffer(INCLUDES)
    bufBody = utils.loadBuffer(BODY)
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        if functionGroup[common.HDRONLY]:
            continue
        fileName = ROOT + groupName + '.cpp'
        utils.logMessage('    generating file ' + fileName + '...')
        fileFunc = file(fileName, 'w')
        utils.printHeader(fileFunc)
        fileFunc.write(bufInclude)
        for function in functionGroup[common.FUNCLIST]:
            generateFuncSource(fileFunc, function, bufBody)
        fileFunc.close()

def getReturnTypeCalcIDL(function):
    returnType = function[common.RETVAL][common.TYPE]
    if returnType == common.PROPVEC:
        return 'sequence < sequence < any > >'
    elif returnType == 'string':
        return 'string'
    else:
        raise ValueError, 'unexpected return type: ' + returnType

def generateIDLSource(functionGroups):
    fileName = ROOT + IDL
    utils.logMessage('    generating file ' + fileName + '...')
    fileIDL = file(fileName, 'w')
    utils.printTimeStamp(fileIDL, '//')
    bufIDLHead = utils.loadBuffer(IDL_HEAD)
    fileIDL.write(bufIDLHead)
    bufIDLFunc = utils.loadBuffer(IDL_FUNC)
    for groupName in functionGroups.keys():
        fileIDL.write('                // %s\n\n' % groupName)
        functionGroup = functionGroups[groupName]
        for function in functionGroup[common.FUNCLIST]:
            if function[common.CTOR]:
                handle = 24 * ' ' + '[in] string handle,\n'
            else:
                handle = ''
            returnTypeIDL = getReturnTypeCalcIDL(function)
            paramList = utils.generateParamList(function[common.PARAMS],
                 6, True, '[in] ')
            fileIDL.write(bufIDLFunc %
                (returnTypeIDL, function[common.CODENAME], handle, paramList))
    bufIDLFoot = utils.loadBuffer(IDL_FOOT)
    fileIDL.write(bufIDLFoot)
    fileIDL.close()

def generate(functionDefs):
    utils.logMessage('  begin generating Calc ...')
    functionGroups = functionDefs[common.FUNCGROUPS]
    generateFuncMap(functionGroups)
    generateAutoHeader(functionGroups)
    generateHeaders(functionGroups)
    generateFuncSources(functionGroups)
    generateIDLSource(functionGroups)
    utils.logMessage('  done generating Calc.')

