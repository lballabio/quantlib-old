'output excel source files'

import common
import utils
import params

# constants

ADDIN      = 'qladdin.cpp'
BODY       = 'stub.Excel.body'
BODY_BUF   = ''
INCLUDES   = 'stub.Excel.includes'
LPXLOPER   = 'LPXLOPER'
MAXLEN     = 255    # max length of excel string
MAXLENERR  = 'list of parameter names exceeds max Excel length of %d:\n%s'
MAXPARAM   = 30     # max #/params to an Excel function
MAXPARMERR = 'number of function parameters exceeds max of %d'
NUMDESC    = 10     # #/params to describe a function
RET_PROP   = '\
        static XLOPER xRet;\n\
        propertyVectorToXloper(&xRet, returnValue, handle);\n\
        return &xRet;'
RET_XLOPER = '\
        static XLOPER xRet;\n\
        %sToXloper(xRet, returnValue);\n\
        return &xRet;'
RET_STRING = '\
        static char c[XL_MAX_STR_LEN];\n\
        stringToChar(c, returnValue);\n\
        return c;'
REGFOOT    = '\
    Excel(xlFree, 0, 1, &xDll);\n\
    return 1;\n\
}\n\n'
REGHEAD    = 'stub.Excel.regheader'
REGLINE    = '        TempStrNoSize("%s")%s'
ROOT       = common.ADDIN_ROOT + 'Excel/'

# global variables

# parameter list objects
plHeader    = ''    # function prototypes
plCtor      = ''    # constructors
plMember    = ''    # member functions
plExcel     = ''    # Excel registration

def generateExcelStringLiteral(str): 
    'prepend hexadecimal byte count to Excel string'
    return '\\x%02X""%s' % (len(str), str)

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
    if function[common.CTOR]:
        paramStr += 'C'
    for param in function[common.PARAMS]:
        paramStr += generateParamChar(param)
    return paramStr

def formatLine(text, comment, lastParameter = False):
    'format a line of text for the function register code'
    if lastParameter:
        suffix = ');'
    else:
        suffix = ','
    str1 = REGLINE % (generateExcelStringLiteral(text), suffix)
    return '%-40s // %s\n' % (str1, comment)

def generateFuncRegister(fileHeader, function):
    'generate call to xlfRegister for given function'
    global plExcel
    funcParams = function[common.PARAMS]
    numParams = len(funcParams)
    # We call xlfRegister with NUMDESC parameters to describe the function
    # +1 additional parm to describe each parm in function being registered.
    # NB if a function ever exceeds the MAXPARAM limit then the code below
    # could be rewritten to dispense with the parameter descriptions
    numParamsTotal = NUMDESC + numParams
    if function[common.CTOR]:
        numParamsTotal += 1            # extra parameter for object handle
    # FIXME validation below to be moved into parse.py?
    if numParamsTotal > MAXPARAM:
        raise ValueError, MAXPARMERR % MAXPARAM
    paramStr = generateParamString(function)
    paramList = plExcel.generateCode(funcParams)
    if function[common.CTOR]:
        paramList = "handle," + paramList
    if len(paramList) >= MAXLEN:
        raise ValueError, MAXLENERR % (MAXLEN, paramList)
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
        if function[common.CTOR]:
            fileHeader.write(formatLine('handle of new object', 'description param 0'))
            i = i + 1
        j = 1
        for param in funcParams:
            if j < numParams:
                lastParameter = False
            else:
                lastParameter = True
            fileHeader.write(formatLine(param[common.DESC], 'description param %d' % i, lastParameter))
            i = i + 1
            j = j + 1
    else:
        fileHeader.write(formatLine(function[common.DESC], 'function description', True))
    fileHeader.write('\n')

def generateFuncRegisters(functionDefs):
    'generate source code to register functions'
    global plExcel
    plExcel = params.ParameterPass(0, delimiter = ',', prependEol = False)
    fileName = ROOT + ADDIN + common.TEMPFILE
    fileHeader = file(fileName, 'w')
    utils.printHeader(fileHeader)
    bufHead = utils.loadBuffer(REGHEAD)
    fileHeader.write(bufHead)
    for group in functionDefs[common.FUNCGROUPS].itervalues():
        fileHeader.write('    // %s\n\n' % group[common.DISPLAYNAME])
        for function in group[common.FUNCLIST]:
            generateFuncRegister(fileHeader, function)
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

def generateFuncDef(fileFunc, function, bufBody):
    'generate source code for body of given function'
    global plHeader, plCtor, plMember
    paramList1 = plHeader.generateCode(function[common.PARAMS])
    functionReturnType = utils.getReturnType(function[common.RETVAL],
        replaceVector = LPXLOPER, replaceMatrix = LPXLOPER, replaceAny = LPXLOPER,
        replaceLong = 'long*', replaceDouble = 'double*', replaceBool = 'bool*',
        replaceString = 'char*')
    returnType = utils.getReturnType(function[common.RETVAL], replaceLong = 'long',
        prefixScalar = 'static', replaceString = 'std::string', replaceAny = 'boost::any',
        replacePropertyVector = 'Properties')
    returnCall = getReturnCall(function[common.RETVAL])
    if function[common.CTOR]:
        handle = '\n' + 8 * ' ' + 'char *handleStub,'
        args = common.ARGLINE + plCtor.generateCode(function[common.PARAMS])
        functionBody = 8 * ' ' + 'std::string handle = getHandleFull(handleStub);\n'
        functionName = common.MAKE_FUNCTION
        paramList2 = common.MAKE_ARGS % (function[common.QLFUNC], 'handle')
    else:
        className = function[common.PARAMS][0][common.CLASS]
        handle = ''
        args = ''
        functionBody = common.FUNC_BODY % (className, className, 'std::string(handle)',
            function[common.NAME], 'std::string(handle)')
        functionName = utils.generateFuncCall(function)
        paramList2 = plMember.generateCode(function[common.PARAMS])
    conversions = utils.generateConversions(
        function[common.PARAMS], 
        nativeDataType = 'xloper',
        anyConversion = 'xloperToScalarAny')
    fileFunc.write(bufBody %
        (functionReturnType, function[common.CODENAME], handle, paramList1, 
            conversions, args, functionBody, returnType, functionName, 
            paramList2, returnCall, function[common.NAME]))

def generateFuncDefs(functionGroups):
    'generate source code for function bodies'
    global plHeader, plCtor, plMember
    bufBody = utils.loadBuffer(BODY)
    bufInclude = utils.loadBuffer(INCLUDES)
    plHeader = params.ParameterDeclare(2, replaceString = 'char',
        replaceTensor = LPXLOPER, derefString = '*',
        derefOther = '*', replaceAny = LPXLOPER)
    plMember = params.ParameterPass(3, skipFirst = True, derefOther = '*', 
        appendTensor = True)
    plCtor = params.ParameterPass(2, convertString = 'std::string(%s)',
        delimiter = ';\n', appendTensor = True, derefOther = '*',
        wrapFormat = 'args.push(%s)', delimitLast = True, appendScalar = True,
        prependEol = False)
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        if functionGroup[common.HDRONLY]:
            continue
        fileName = ROOT + groupName + '.cpp' + common.TEMPFILE
        fileFunc = file(fileName, 'w')
        utils.printHeader(fileFunc)
        bufIncludeFull = bufInclude % groupName
        fileFunc.write(bufIncludeFull)
        for function in functionGroup[common.FUNCLIST]:
            generateFuncDef(fileFunc, function, bufBody)
        fileFunc.close()
        utils.updateIfChanged(fileName)

def generate(functionDefs):
    'generate source code for Excel addin'
    utils.logMessage('  begin generating Excel ...')
    generateFuncRegisters(functionDefs)
    generateFuncDefs(functionDefs[common.FUNCGROUPS])
    utils.logMessage('  done generating Excel.')


