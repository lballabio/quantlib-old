'output calc source files'

import common
import utils
import params

# constants

AUTOHDR         = 'autogen.hpp'
BODY            = 'stub.Calc.body'
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
FUNC_PROTOTYPE  = '%s SAL_CALL QLAddin::%s(' 
IDL             = 'QuantLibAddin.idl'
IDL_FOOT        = 'stub.Calc.idlfoot'
IDL_FUNC        = 'stub.Calc.idlfunc'
IDL_HEAD        = 'stub.Calc.idlhead'
INCLUDES        = 'stub.Calc.includes'
MAP             = 'stub.Calc.map'
MAPFILE         = 'funcdef.cpp'
MAPLINE         = '    %s[ STRFROMANSI( "%s" ) ]\n\
        =  STRFROMANSI( "%s" );\n\n'
PARMLINE        = '    %s[ STRFROMANSI( "%s" ) ].push_back( STRFROMANSI( "%s" ) );\n'
ROOT            = common.ADDIN_ROOT + 'Calc/'

# global variables

# parameter list objects
plHeader    = ''    # function prototypes
plCtor      = ''    # constructors
plMember    = ''    # member functions

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
        for function in functionGroup[common.FUNCLIST]:
            fileMap.write('    // %s\n\n' % function[common.NAME])
            fileMap.write(MAPLINE
                % ('funcMap', function[common.CODENAME], function[common.NAME]))
            fileMap.write(MAPLINE
                % ('funcDesc', function[common.CODENAME], function[common.DESC]))
            if function[common.CTOR]:
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

def generateHeader(fileHeader, function, suffix):
    global plHeader
    'generate implementation for given function'
    if function[common.CTOR]:
        fileHeader.write('\n        const STRING &handle')
        if function[common.PARAMS]:
            fileHeader.write(',')
    if function[common.PARAMS]:
        paramList = plHeader.generateCode(function[common.PARAMS])
        fileHeader.write('\n')
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
        for function in functionGroup[common.FUNCLIST]:
            returnTypeCalc = utils.getFunctionReturnType(function[common.RETVAL], 
                formatVector = CALC_MATRIX, formatMatrix = CALC_MATRIX, 
                replaceLong = CALC_LONG, replaceString = CALC_STRING, 
                replaceBool = CALC_LONG, replaceAny = CALC_ANY) 
            fileHeader.write('    virtual %s SAL_CALL %s('
                % (returnTypeCalc, function[common.CODENAME]))
            generateHeader(fileHeader, function, ';')
            fileHeader.write('\n')
        fileHeader.write('#endif\n\n')
        fileHeader.close()
        utils.updateIfChanged(fileName)

def getReturnCall(returnDef):
    if returnDef[common.TYPE] == common.PROPERTY:
        if returnDef[common.TENSOR] == common.VECTOR:
            return 'propertyVectorToSeqSeq(returnValue, handle)'
        else:
            raise ValueError, 'type property can only be combined with tensorrank vector'

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

    if returnDef[common.TENSOR] == common.SCALAR:
        if returnDef[common.TYPE] == common.STRING:
            return 'stlStringToOuString(returnValue)'
        elif returnDef[common.TYPE] == common.ANY:
            return 'boostAnyToCalcAny(returnValue)'
        else:
            return 'returnValue'
    elif returnDef[common.TENSOR] == common.VECTOR:
        return 'Vector' + type + 'ToSeqSeq(returnValue)'
    elif returnDef[common.TENSOR] == common.MATRIX:
        return 'Matrix' + type + 'ToSeqSeq(returnValue)'

def generateFuncSource(fileFunc, function, bufBody):
    'generate source for given function'
    global plCtor, plMember
    returnTypeCalc = utils.getFunctionReturnType(function[common.RETVAL], 
        formatVector = CALC_MATRIX, formatMatrix = CALC_MATRIX, 
        replaceLong = CALC_LONG, replaceString = CALC_STRING, 
        replaceBool = CALC_LONG, replaceAny = CALC_ANY)
    fileFunc.write(FUNC_PROTOTYPE % (returnTypeCalc, function[common.CODENAME]))
    generateHeader(fileFunc, function, ' {')
    returnType = utils.getReturnType(
        function[common.RETVAL], 
        replaceAny = 'boost::any',
        replaceString = 'std::string')
    returnCall = getReturnCall(function[common.RETVAL])
    if function[common.CTOR]:
        functionBody = common.ARGLINE + plCtor.generateCode(function[common.PARAMS])
        functionName = common.MAKE_FUNCTION
        paramList = common.MAKE_ARGS % (function[common.QLFUNC], CONV_HANDLE)
    else:
        className = function[common.PARAMS][0][common.CLASS]
        functionBody = common.FUNC_BODY % (className, className, CONV_HANDLE,
            function[common.NAME], CONV_HANDLE)
        functionName = 'objectPointer->' + function[common.QLFUNC]
        paramList = plMember.generateCode(function[common.PARAMS])
    conversions = utils.generateConversions(
        function[common.PARAMS], 
        nativeDataType = 'SeqSeq',
        anyConversion = 'calcAnyToBoostAny')
    fileFunc.write(bufBody % (conversions, functionBody, returnType,
        functionName, paramList, returnCall, function[common.NAME]))

def generateFuncSources(functionGroups):
    'generate source for function implementations'
    global plCtor, plMember
    bufInclude = utils.loadBuffer(INCLUDES)
    bufBody = utils.loadBuffer(BODY)
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        if functionGroup[common.HDRONLY]:
            continue
        fileName = ROOT + groupName + '.cpp' + common.TEMPFILE
        fileFunc = file(fileName, 'w')
        utils.printHeader(fileFunc)
        bufIncludeFull = bufInclude % groupName
        fileFunc.write(bufIncludeFull)
        plCtor = params.ParameterPass(2, convertString = CONV_STRING,
            delimiter = ';\n', appendTensor = True, appendScalar = True,
            wrapFormat = 'args.push(%s)', delimitLast = True,
            convertBool = CONV_BOOL)
        plMember = params.ParameterPass(3, convertString = CONV_STRING,
            skipFirst = True, appendTensor = True)
        for function in functionGroup[common.FUNCLIST]:
            generateFuncSource(fileFunc, function, bufBody)
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
        plIdl = params.ParameterDeclare(6, prefix = '[in] ', replaceBool = 'long',
            formatVector = FORMAT_TENSOR_IDL, formatMatrix = FORMAT_TENSOR_IDL,
            replaceTensorStr = common.ANY)
        for function in functionGroup[common.FUNCLIST]:
            paramList = plIdl.generateCode(function[common.PARAMS])
            if function[common.CTOR]:
                handle = '\n' + 24 * ' ' + '[in] string handle'
                if paramList:
                    handle += ',\n'
            else:
                if paramList:
                    handle = '\n'
                else:
                    handle = ''
            returnTypeIDL = utils.getFunctionReturnType(function[common.RETVAL], 
                formatVector = CALC_MATRIX_IDL, formatMatrix = CALC_MATRIX_IDL, 
                replaceBool = 'long')
            fileIDL.write(bufIDLFunc % (returnTypeIDL, 
                function[common.CODENAME], handle, paramList))
    bufIDLFoot = utils.loadBuffer(IDL_FOOT)
    fileIDL.write(bufIDLFoot)
    fileIDL.close()
    utils.updateIfChanged(fileName)

def generate(functionDefs):
    global plHeader
    'generate source code for Calc addin'
    utils.logMessage('  begin generating Calc ...')
    plHeader = params.ParameterDeclare(2, replaceString = CALC_STRING,
        replaceLong = CALC_LONG, derefString = '&', replaceTensorStr = CALC_ANY,
        formatVector = FORMAT_TENSOR, formatMatrix = FORMAT_TENSOR,
        derefTensor = '&', replaceAny = CALC_ANY, derefAny = '&',
        prefixString = 'const', prefixAny = 'const', replaceBool = CALC_LONG)
    functionGroups = functionDefs[common.FUNCGROUPS]
    generateFuncMap(functionGroups)
    generateAutoHeader(functionGroups)
    generateHeaders(functionGroups)
    generateFuncSources(functionGroups)
    generateIDLSource(functionGroups)
    utils.logMessage('  done generating Calc.')

