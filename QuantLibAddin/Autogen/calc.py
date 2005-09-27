
"""
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

'output calc source files'

import common
import utils
import params

# constants

AUTOHDR         = 'autogen.hpp'
BUF_CTOR        = 'stub.calc.constructor'
BUF_MEMBER      = 'stub.calc.member'
CALC_ANY        = 'ANY'
CALC_BOOL       = 'sal_Bool'
CALC_BOOL_IDL   = 'boolean'
CALC_LONG       = 'sal_Int32'
CALC_MATRIX     = 'SEQSEQ( %s )'
CALC_MATRIX_IDL = 'sequence < sequence < %s > >'
CALC_STRING     = 'STRING'
CONV_BOOL       = 'static_cast < bool > ( %s )'
CONV_HANDLE     = 'OUStringToStlString(handle)'
CONV_STRING     = 'OUStringToStlString(%s)'
FORMAT_TENSOR   = 'const SEQSEQ( %s )'
FORMAT_TENSOR_IDL = 'sequence < sequence < %s > > '
IDL             = 'QuantLibAddin.idl'
IDL_FOOT        = 'stub.calc.idlfoot'
IDL_FUNC        = 'stub.calc.idlfunc'
IDL_HEAD        = 'stub.calc.idlhead'
INCLUDES        = 'stub.calc.includes'
MAP             = 'stub.calc.map'
MAPFILE         = 'funcdef.cpp'
MAPLINE         = """    %s[ STRFROMANSI( "%s" ) ]\n
        =  STRFROMANSI( "%s" );\n\n"""
PARMLINE        = '    %s[ STRFROMANSI( "%s" ) ].push_back( STRFROMANSI( "%s" ) );\n'
ROOT            = common.ADDIN_ROOT + 'Calc/'

# global parameter list objects

plHeader = params.ParameterDeclare(2, replaceString = CALC_STRING,
    replaceLong = CALC_LONG, derefString = '&', replaceTensorStr = CALC_ANY,
    formatVector = FORMAT_TENSOR, formatMatrix = FORMAT_TENSOR,
    derefTensor = '&', replaceAny = CALC_ANY, derefAny = '&',
    prefixString = 'const', prefixAny = 'const', replaceBool = CALC_LONG)

def generateFuncMap(functionGroups):
    'generate help text for function wizard'
    fileName = ROOT + MAPFILE + common.TEMPFILE
    fileMap = file(fileName, 'w')
    utils.printHeader(fileMap)
    bufCalcMap = utils.loadBuffer(MAP)
    fileMap.write(bufCalcMap)
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        fileMap.write('    // %s\n\n' % functionGroup[common.DISPLAYNAME])
        for function in functionGroup[common.FUNCS]:
            if not utils.checkFunctionPlatform(function, common.PLATFORM_CALC):
                continue
            fileMap.write('    // %s\n\n' % function[common.NAME])
            fileMap.write(MAPLINE
                % ('funcMap', function[common.CODENAME], function[common.NAME]))
            fileMap.write(MAPLINE
                % ('funcDesc', function[common.CODENAME], function[common.DESC]))
            if function[common.CTOR] == common.TRUE:
                fileMap.write(PARMLINE
                    % ('argName', function[common.CODENAME], 'handle'))
                fileMap.write(PARMLINE
                    % ('argDesc', function[common.CODENAME], 
                       'handle of newly constructed ' + function[common.QLFUNC] + ' object'))
            for param in function[common.PARAMS]:
                fileMap.write(PARMLINE
                    % ('argName', function[common.CODENAME], param[common.NAME]))
                fileMap.write(PARMLINE
                    % ('argDesc', function[common.CODENAME], param[common.DESC]))
            fileMap.write('\n')
    fileMap.write('}\n\n')
    fileMap.close()
    utils.updateIfChanged(fileName)

def generateAutoHeader(functionGroups):
    'generate header file that lists all other headers'
    fileName = ROOT + AUTOHDR + common.TEMPFILE
    fileHeader = file(fileName, 'w')
    utils.printHeader(fileHeader)
    fileHeader.write('#ifndef qla_calc_auto_hpp\n')
    fileHeader.write('#define qla_calc_auto_hpp\n\n')
    for groupName in functionGroups.keys():
        fileHeader.write('#include <Addins/Calc/%s.hpp>\n' % groupName)
    fileHeader.write('\n#endif\n\n')
    fileHeader.close()
    utils.updateIfChanged(fileName)

def generateHeader(fileHeader, function, declararion = True):
    'generate implementation for given function'
    if declararion:
        prototype = '    virtual %s SAL_CALL %s('
        suffix = ';\n'
    else:
        prototype = '%s SAL_CALL QLAddin::%s(' 
        suffix = ' {'
    returnType = utils.getReturnType(function[common.RETVAL], 
        formatVector = CALC_MATRIX, formatMatrix = CALC_MATRIX, 
        replaceLong = CALC_LONG, replaceString = CALC_STRING, 
        replaceBool = CALC_LONG, replaceAny = CALC_ANY)
    fileHeader.write(prototype % (returnType, function[common.CODENAME]))
    if function[common.CTOR] == common.TRUE:
        fileHeader.write('\n        const STRING &handle,')
    paramList = plHeader.generateCode(function[common.PARAMS])
    fileHeader.write(paramList)
    fileHeader.write(') THROWDEF_RTE_IAE%s\n' % suffix)

def generateHeaders(functionGroups):
    'generate source for function prototypes'
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        fileName = ROOT + groupName + '.hpp' + common.TEMPFILE
        fileHeader = file(fileName, 'w')
        utils.printHeader(fileHeader)
        fileHeader.write('#ifndef qla_calc_%s_hpp\n' % groupName)
        fileHeader.write('#define qla_calc_%s_hpp\n\n' % groupName)
        for function in functionGroup[common.FUNCS]:
            if not utils.checkFunctionPlatform(function, common.PLATFORM_CALC):
                continue
            generateHeader(fileHeader, function)
        fileHeader.write('#endif\n\n')
        fileHeader.close()
        utils.updateIfChanged(fileName)

def getReturnCall(returnDef):
    'generate code to convert datatype of return value'
    if returnDef[common.TENSOR] == common.SCALAR:
        if returnDef[common.TYPE] == common.STRING:
            return 'stlStringToOuString(returnValue)'
        elif returnDef[common.TYPE] == common.ANY:
            return 'boostAnyToCalcAny(returnValue)'
        else:
            return 'returnValue'
    else:
        return returnDef[common.TENSOR].capitalize() \
            + returnDef[common.TYPE].capitalize() \
            + 'ToSeqSeq(returnValue)'

def generateMember(fileFunc, function, bufMember, plMember):
    'generate source for given function'
    generateHeader(fileFunc, function, False)
    conversions = utils.generateConversions(function[common.PARAMS], 
        'SeqSeq', sourceTypeAny = 'calcAny')
    className = function[common.PARAMS][0][common.ATTS][common.CLASS]
    returnType = utils.getReturnType(function[common.RETVAL], 
        replaceAny = 'boost::any', replaceString = 'std::string')
    functionName = utils.generateFuncCall(function)
    paramList = plMember.generateCode(function[common.PARAMS])
    returnCall = getReturnCall(function[common.RETVAL])
    fileFunc.write(bufMember % (conversions, className, className, returnType, 
        functionName, paramList, returnCall, function[common.NAME]))

def generateConstructor(fileFunc, function, bufCtor, plCtor):
    generateHeader(fileFunc, function, False)
    paramList = plCtor.generateCode(function[common.PARAMS])
    conversions = utils.generateConversions(function[common.PARAMS], 
        sourceTypeOther = 'SeqSeq', sourceTypeAny = 'calcAny')
    fileFunc.write(bufCtor % (conversions, function[common.QLFUNC], 
        paramList, function[common.NAME]))

def generateFuncSources(functionGroups):
    'generate source for function implementations'
    plCtor = params.ParameterPass(3, convertString = CONV_STRING,
        appendTensor = True, appendScalar = True,
        convertBool = CONV_BOOL)
    plMember = params.ParameterPass(3, convertString = CONV_STRING,
        skipFirst = True, appendTensor = True)
    bufInclude = utils.loadBuffer(INCLUDES)
    bufCtor = utils.loadBuffer(BUF_CTOR)
    bufMember = utils.loadBuffer(BUF_MEMBER)
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        if functionGroup[common.HDRONLY] == common.TRUE:
            continue
        fileName = ROOT + groupName + '.cpp' + common.TEMPFILE
        fileFunc = file(fileName, 'w')
        utils.printHeader(fileFunc)
        bufIncludeFull = bufInclude % groupName
        fileFunc.write(bufIncludeFull)
        for function in functionGroup[common.FUNCS]:
            if not utils.checkFunctionPlatform(function, common.PLATFORM_CALC):
                continue
            if function[common.CTOR] == common.TRUE:
                generateConstructor(fileFunc, function, bufCtor, plCtor)
            else:
                generateMember(fileFunc, function, bufMember, plMember)
        fileFunc.close()
        utils.updateIfChanged(fileName)

def generateIDLSource(functionGroups):
    'generate the IDL file for the addin'
    fileName = ROOT + IDL + common.TEMPFILE
    fileIDL = file(fileName, 'w')
    utils.printTimeStamp(fileIDL)
    bufIDLHead = utils.loadBuffer(IDL_HEAD)
    fileIDL.write(bufIDLHead)
    bufIDLFunc = utils.loadBuffer(IDL_FUNC)
    for groupName in functionGroups.keys():
        fileIDL.write('                // %s\n\n' % groupName)
        functionGroup = functionGroups[groupName]
        plIdl = params.ParameterDeclare(6, prefix = '[in] ', replaceBool = common.LONG,
            formatVector = FORMAT_TENSOR_IDL, formatMatrix = FORMAT_TENSOR_IDL,
            replaceTensorStr = common.ANY)
        for function in functionGroup[common.FUNCS]:
            if not utils.checkFunctionPlatform(function, common.PLATFORM_CALC):
                continue
            paramList = plIdl.generateCode(function[common.PARAMS])
            if function[common.CTOR] == common.TRUE:
                handle = '\n' + 24 * ' ' + '[in] string handle'
                if paramList:
                    handle += ','
            else:
                handle = ''
            returnTypeIDL = utils.getReturnType(function[common.RETVAL], 
                formatVector = CALC_MATRIX_IDL, formatMatrix = CALC_MATRIX_IDL, 
                replaceBool = common.LONG)
            fileIDL.write(bufIDLFunc % (returnTypeIDL, 
                function[common.CODENAME], handle, paramList))
    bufIDLFoot = utils.loadBuffer(IDL_FOOT)
    fileIDL.write(bufIDLFoot)
    fileIDL.close()
    utils.updateIfChanged(fileName)

def generate(functionGroups):
    'generate source code for Calc addin'
    utils.logMessage('  begin generating Calc ...')
    generateFuncMap(functionGroups)
    generateAutoHeader(functionGroups)
    generateHeaders(functionGroups)
    generateFuncSources(functionGroups)
    generateIDLSource(functionGroups)
    utils.logMessage('  done generating Calc.')

