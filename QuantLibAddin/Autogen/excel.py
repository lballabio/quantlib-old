
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

'output excel source files'

import common
import utils
import params

# constants

ADDIN           = 'qladdin.cpp'
BUF_CTOR        = 'stub.excel.constructor'
BUF_INCLUDES    = 'stub.excel.includes'
BUF_MEMBER      = 'stub.excel.member'
BUF_REGISTER    = 'stub.excel.regheader'
LPXLOPER        = 'LPXLOPER'
MAXLEN          = 255    # max length of excel string
MAXLENERR       = 'string length exceeds Excel maximum of %d:\n' % MAXLEN
MAXPARAM        = 30     # max #/params to an Excel function
MAXPARMERR      = 'number of function parameters exceeds max of %d' % MAXPARAM
NUMDESC         = 10     # #/params to describe a function
REGFOOT         = '\
    Excel(xlFree, 0, 1, &xDll);\n\
    return 1;\n\
}\n\n'
REGLINE         = '        TempStrNoSize("\\x%02X""%s")%s'
RET_PROP        = '\
        static XLOPER xRet;\n\
        propertyVectorToXloper(&xRet, returnValue, handle);\n\
        return &xRet;'
RET_STRING      = '\
        static char c[XL_MAX_STR_LEN];\n\
        stringToChar(c, returnValue);\n\
        return c;'
RET_XLOPER      = '\
        static XLOPER xRet;\n\
        %sToXloper(xRet, returnValue);\n\
        return &xRet;'
ROOT            = common.ADDIN_ROOT + 'Excel/'

def formatLine(text, comment, lastParameter = False):
    'format a line of text for the function register code'
    if len(text) >= MAXLEN:
        raise ValueError, MAXLENERR + text
    if lastParameter:
        suffix = ');'
    else:
        suffix = ','
    str1 = REGLINE % (len(text), text, suffix)
    return '%-40s// %s\n' % (str1, comment)

def generateParamChar(param):
    'derive the Excel char code corresponding to parameter datatype'
    if param[common.TENSOR] == common.VECTOR or \
       param[common.TENSOR] == common.MATRIX or \
       param[common.TYPE]   == common.ANY:
        return 'R'
    else:
        if param[common.TYPE]   == common.STRING:
            return 'C'
        elif param[common.TYPE] == common.DOUBLE:
            return 'E'
        elif param[common.TYPE] == common.LONG:
            return 'N'
        elif param[common.TYPE] == common.BOOL:
            return 'L'
        else:
            raise ValueError, 'unknown datatype: ' + type

def generateParamString(function):
    'generate string to register function parameters'
    paramStr = generateParamChar(function[common.RETVAL])
    if function[common.CTOR] == common.TRUE:
        paramStr += 'C'
    for param in function[common.PARAMS]:
        paramStr += generateParamChar(param)
    return paramStr

def generateFuncRegister(fileHeader, function, plExcel):
    'generate call to xlfRegister for given function'
    funcParams = function[common.PARAMS]
    numParams = len(funcParams)
    # We call xlfRegister with NUMDESC parameters to describe the function
    # +1 additional parm to describe each parm in function being registered.
    # NB if a function ever exceeds the MAXPARAM limit then the code below
    # could be rewritten to dispense with the parameter descriptions
    numParamsTotal = NUMDESC + numParams
    if function[common.CTOR] == common.TRUE:
        numParamsTotal += 1            # extra parameter for object handle
    # FIXME validation below to be moved into parse.py?
    if numParamsTotal > MAXPARAM:
        raise ValueError, MAXPARMERR
    paramStr = generateParamString(function)
    paramList = plExcel.generateCode(funcParams)
    if function[common.CTOR] == common.TRUE:
        paramList = "handle," + paramList
    fileHeader.write('    Excel(xlfRegister, 0, %d, &xDll,\n' % numParamsTotal)
    fileHeader.write(formatLine(function[common.CODENAME], 'function code name'))
    fileHeader.write(formatLine(paramStr, 'parameter codes'))
    fileHeader.write(formatLine(function[common.NAME], 'function display name'))
    fileHeader.write(formatLine(paramList, 'comma-delimited list of parameters'))    
    fileHeader.write(formatLine('1', 'function type (0 = hidden function, 1 = worksheet function, 2 = command macro)'))
    fileHeader.write(formatLine('QuantLib', 'function category'))
    fileHeader.write(formatLine('', 'shortcut text (command macros only)'))
    fileHeader.write(formatLine('', 'path to help file'))
    if funcParams:
        fileHeader.write(formatLine(function[common.DESC], 'function description'))
        i = 0
        if function[common.CTOR] == common.TRUE:
            fileHeader.write(formatLine('handle of new object', 'description param 0'))
            i += 1
        j = 1
        for param in funcParams:
            if j < numParams:
                lastParameter = False
            else:
                lastParameter = True
            fileHeader.write(formatLine(param[common.DESC], 'description param %d' % i, lastParameter))
            i += 1
            j += 1
    else:
        fileHeader.write(formatLine(function[common.DESC], 'function description', True))
    fileHeader.write('\n')

def generateFuncRegisters(functionDefs):
    'generate source code to register functions'
    plExcel = params.ParameterPass(0, delimiter = ',', prependEol = False)
    fileName = ROOT + ADDIN + common.TEMPFILE
    fileHeader = file(fileName, 'w')
    utils.printHeader(fileHeader)
    bufHead = utils.loadBuffer(BUF_REGISTER)
    fileHeader.write(bufHead)
    for group in functionDefs.itervalues():
        fileHeader.write('    // %s\n\n' % group[common.DISPLAYNAME])
        for function in group[common.FUNCS]:
            if not utils.checkFunctionPlatform(function, common.PLATFORM_EXCEL):
                continue
            generateFuncRegister(fileHeader, function, plExcel)
    fileHeader.write(REGFOOT)
    fileHeader.close()
    utils.updateIfChanged(fileName)

def getReturnCall(returnDef):
    if returnDef[common.TYPE] == common.PROPERTY:
        if returnDef[common.TENSOR] == common.VECTOR:
            return RET_PROP
        else:
            raise ValueError, 'type property can only be combined with tensorrank vector'

    if returnDef[common.TENSOR] == common.SCALAR:
        if returnDef[common.TYPE] == common.STRING:
            return RET_STRING
        elif returnDef[common.TYPE] == common.ANY:
            pass
        else:
            return '        return &returnValue;'

    if returnDef[common.TYPE] == common.LONG:
        type = 'Long'
    elif returnDef[common.TYPE] == common.DOUBLE:
        type = 'Double'
    elif returnDef[common.TYPE] == common.BOOL:
        type = 'Bool'
    elif returnDef[common.TYPE] == common.STRING:
        type = 'String'
    elif returnDef[common.TYPE] == common.ANY:
        type = 'Any'
    else:
        raise ValueError, 'unsupported type'

    if returnDef[common.TENSOR] == common.SCALAR:
        return RET_XLOPER % ('scalar' + type)
    elif returnDef[common.TENSOR] == common.VECTOR:
        return RET_XLOPER % ('vector' + type)
    elif returnDef[common.TENSOR] == common.MATRIX:
        return RET_XLOPER % ('matrix' + type)

def generateConstructor(fileFunc, function, bufCtor, plHeader, plCtor):
    paramList1 = plHeader.generateCode(function[common.PARAMS])
    paramList2 = plCtor.generateCode(function[common.PARAMS])
    conversions = utils.generateConversions(function[common.PARAMS], 
        nativeDataType = 'xloper', anyConversion = 'xloperToScalarAny')
    fileFunc.write(bufCtor % (function[common.CODENAME], paramList1, conversions, 
        function[common.QLFUNC], paramList2, function[common.NAME]))

def generateMember(fileFunc, function, bufMember, plHeader, plMember):
    'generate source code for body of given function'
    paramList1 = plHeader.generateCode(function[common.PARAMS])
    paramList2 = plMember.generateCode(function[common.PARAMS])
    functionReturnType = utils.getReturnType(function[common.RETVAL],
        replaceVector = LPXLOPER, replaceMatrix = LPXLOPER, replaceAny = LPXLOPER,
        replaceLong = 'long*', replaceDouble = 'double*', replaceBool = 'bool*',
        replaceString = 'char*')
    returnType = utils.getReturnType(function[common.RETVAL], replaceLong = 'long',
        prefixScalar = 'static', replaceString = 'std::string', replaceAny = 'boost::any',
        replacePropertyVector = 'ObjHandler::Properties')
    returnCall = getReturnCall(function[common.RETVAL])
    className = function[common.PARAMS][0][common.ATTS][common.CLASS]
    functionName = utils.generateFuncCall(function)
    conversions = utils.generateConversions(function[common.PARAMS], 
        nativeDataType = 'xloper', anyConversion = 'xloperToScalarAny')
    fileFunc.write(bufMember %
        (functionReturnType, function[common.CODENAME], paramList1, conversions, 
        className, className, returnType, functionName, 
        paramList2, returnCall, function[common.NAME]))

def generateFuncDefs(functionGroups):
    'generate source code for function bodies'
    bufCtor = utils.loadBuffer(BUF_CTOR)
    bufMember = utils.loadBuffer(BUF_MEMBER)
    bufInclude = utils.loadBuffer(BUF_INCLUDES)
    plHeader = params.ParameterDeclare(2, replaceString = 'char',
        replaceTensor = LPXLOPER, derefString = '*',
        derefOther = '*', replaceAny = LPXLOPER)
    plMember = params.ParameterPass(3, skipFirst = True, derefOther = '*', 
        appendTensor = True)
    plCtor = params.ParameterPass(3, appendTensor = True,
        derefOther = '*', appendScalar = True)
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        if functionGroup[common.HDRONLY] == common.TRUE:
            continue
        fileName = ROOT + groupName + '.cpp' + common.TEMPFILE
        fileFunc = file(fileName, 'w')
        utils.printHeader(fileFunc)
        fileFunc.write(bufInclude % groupName)
        for function in functionGroup[common.FUNCS]:
            if not utils.checkFunctionPlatform(function, common.PLATFORM_EXCEL):
                continue
            if function[common.CTOR] == common.TRUE:
                generateConstructor(fileFunc, function, bufCtor, plHeader, plCtor)
            else:
                generateMember(fileFunc, function, bufMember, plHeader, plMember)
        fileFunc.close()
        utils.updateIfChanged(fileName)

def generate(functionDefs):
    'generate source code for Excel addin'
    utils.logMessage('  begin generating Excel ...')
    generateFuncRegisters(functionDefs)
    generateFuncDefs(functionDefs)
    utils.logMessage('  done generating Excel.')

