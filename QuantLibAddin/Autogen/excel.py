
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

ADDIN        = 'qladdin.cpp'
BUF_CTOR     = 'stub.excel.constructor'
BUF_INCLUDES = 'stub.excel.includes'
BUF_MEMBER   = 'stub.excel.member'
BUF_REGISTER = 'stub.excel.regheader'
MAXLEN       = 255    # max length of excel string
MAXLENERR    = 'string length exceeds Excel maximum of %d:\n' % MAXLEN
MAXPARAM     = 30     # max #/params to an Excel function
MAXPARAMERR  = 'number of function parameters exceeds max of %d' % MAXPARAM
NUMDESC      = 10     # #/params to describe a function
REGFOOT      = """    Excel(xlFree, 0, 1, &xDll);\n
    return 1;
}\n\n"""
REGLINE      = '        TempStrNoSize("\\x%02X""%s")%s'
RET_STRING   = """        static char ret[XL_MAX_STR_LEN];
        stringToChar(ret, returnValue);
        return ret;"""
RET_XLOPER   = """        static XLOPER xRet;
        %sToXloper(xRet, returnValue);
        return &xRet;"""
ROOT         = common.ADDIN_ROOT + 'Excel/'

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

def generateParamChar(param, returnVal = False):
    'derive the Excel char code corresponding to parameter datatype'
    if returnVal \
    and (param[common.TYPE]   == common.ANY
    or   param[common.TENSOR] == common.VECTOR
    or   param[common.TENSOR] == common.MATRIX):
        return 'R'
    elif utils.paramIsOptional(param):
        return 'P'
    elif param[common.TENSOR] == common.SCALAR:
        if param[common.TYPE] == common.LONG:
            return 'N'
        elif param[common.TYPE] == common.DOUBLE:
            return 'E'
        elif param[common.TYPE] == common.BOOL:
            return 'L'
        elif param[common.TYPE] == common.STRING:
            return 'C'
        elif param[common.TYPE] == common.ANY:
            return 'P'
        else:
            raise ValueError, 'unknown datatype: ' + param[common.TYPE]
    else:   # vector or matrix
        if param[common.TYPE] == common.LONG \
        or param[common.TYPE] == common.DOUBLE:
            return 'K'
        else:
            return 'P'

def generateParamString(function):
    'generate string to register function parameters'
    paramStr = generateParamChar(function[common.RETVAL], True)
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
    numRegisterParams = NUMDESC + numParams
    paramStr = generateParamString(function)
    paramList = plExcel.generateCode(funcParams)
    if function[common.CTOR] == common.TRUE:    # extra parameter for object handle
        paramList = "handle," + paramList
        numRegisterParams += 1
    # FIXME validation below to be moved into parse.py?
    if numRegisterParams > MAXPARAM:
        raise ValueError, MAXPARAMERR
    fileHeader.write('    Excel(xlfRegister, 0, %d, &xDll,\n' % numRegisterParams)
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
        lastParameter = False
        for param in funcParams:
            desc = param[common.DESC]
            if j >= numParams:                
                lastParameter = True
                # append 2 spaces to description of last parameter to work around bug in Excel
                # which causes description to be corrupted when displayed in the Function Wizard
                desc += '  '
            fileHeader.write(formatLine(desc, 'description param %d' % i, lastParameter))
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
    if returnDef[common.TENSOR] == common.SCALAR:
        if returnDef[common.TYPE] == common.STRING:
            return RET_STRING
        elif returnDef[common.TYPE] == common.ANY:
            pass
        else:
            return '        return &returnValue;'
    return RET_XLOPER % (returnDef[common.TENSOR] + returnDef[common.TYPE].capitalize())

def generateConstructor(fileFunc, function, bufCtor, plHeader, plCtor):
    'generate source code for body of constructor function'
    paramList1 = plHeader.generateCode(function[common.PARAMS])
    paramList2 = plCtor.generateCode(function[common.PARAMS])
    conversions = utils.generateConversions(function[common.PARAMS], 
        'oper', 'fp', 'oper')
    fileFunc.write(bufCtor % (function[common.CODENAME], paramList1, conversions, 
        function[common.QLFUNC], paramList2, function[common.NAME]))

def generateMember(fileFunc, function, bufMember, plHeader, plMember):
    'generate source code for body of member function'
    paramList1 = plHeader.generateCode(function[common.PARAMS])
    paramList2 = plMember.generateCode(function[common.PARAMS])
    functionReturnType = utils.getReturnType(function[common.RETVAL],
        replaceVector = 'XLOPER', replaceMatrix = 'XLOPER', replaceAny = 'XLOPER',
        replaceString = 'char', deref = '*')
    returnType = utils.getReturnType(function[common.RETVAL], prefixScalar = 'static',
        replaceString = 'std::string', replaceAny = 'boost::any')
    returnCall = getReturnCall(function[common.RETVAL])
    className = function[common.PARAMS][0][common.ATTS][common.CLASS]
    functionName = utils.generateFuncCall(function)
    conversions = utils.generateConversions(function[common.PARAMS], 
        'oper', 'fp', 'oper')
    fileFunc.write(bufMember %
        (functionReturnType, function[common.CODENAME], paramList1, conversions, 
        className, className, returnType, functionName, 
        paramList2, returnCall, function[common.NAME]))

def generateFuncDefs(functionGroups):
    'generate source code for function bodies'
    bufCtor = utils.loadBuffer(BUF_CTOR)
    bufMember = utils.loadBuffer(BUF_MEMBER)
    bufInclude = utils.loadBuffer(BUF_INCLUDES)
    plHeader = params.ParameterDeclare(2, derefAll = '*',
        replaceString = 'char', replaceTensorNum = 'FP',
        replaceTensor = 'OPER', replaceAny = 'OPER', 
        replaceOptional = 'OPER')
    plMember = params.ParameterPass(3, derefOther = '*', skipFirst = True,
        appendTensor = True, appendScalar = True, appendOptional = True)
    plCtor = params.ParameterPass(3, derefOther = '*',
        appendTensor = True, appendScalar = True, appendOptional = True)
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

