'output calc source files'

import common
import utils
import params

# constants

AUTOHDR         = 'autogen.hpp'
BODY            = 'stub.Calc.body'
CALC_BOOL       = 'sal_Bool'
CALC_BOOL_IDL   = 'boolean'
CALC_LONG       = 'sal_Int32'
CALC_MATRIX     = 'SEQSEQ( ANY )'
CALC_MATRIX_IDL = 'sequence < sequence < any > >'
CALC_STRING     = 'STRING'
CONV_HANDLE     = 'OUStringToString(handle)'
FORMAT_TENSOR   = 'const SEQSEQ( %s )'
FORMAT_TENSOR_IDL = 'sequence < sequence < %s > > '
FUNC_PROTOTYPE  = 'SEQSEQ( ANY ) SAL_CALL QLAddin::%s(' 
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
STR_FMT         = 'OUStringToString(%s)'

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
            if function[common.PARAMS]:
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
        fileHeader.write('\n        const STRING &handle,')
    if function[common.PARAMS]:
        paramList = plHeader.generateCode(function[common.PARAMS])
        fileHeader.write('\n')
        fileHeader.write(paramList)
    fileHeader.write(') THROWDEF_RTE_IAE%s\n' % suffix)

def getReturnTypeCalc(retVal):
    'derive return type for function'
    if retVal[common.TENSOR] == common.VECTOR or \
       retVal[common.TENSOR] == common.MATRIX or \
       retVal[common.TYPE]   == common.ANY:
        return CALC_MATRIX
    elif retVal[common.TYPE] == common.BOOL:
        return CALC_BOOL
    elif retVal[common.TYPE] == common.STRING:
        return CALC_STRING
    elif retVal[common.TYPE] == common.LONG:
        return CALC_LONG
    elif retVal[common.TYPE] == common.DOUBLE:
        return common.DOUBLE
    else:
        raise ValueError, 'unexpected return type: ' + retVal[common.TYPE]

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
            returnTypeCalc = getReturnTypeCalc(function[common.RETVAL])
            fileHeader.write('    virtual %s SAL_CALL %s('
                % (returnTypeCalc, function[common.CODENAME]))
            generateHeader(fileHeader, function, ';')
            fileHeader.write('\n')
        fileHeader.write('#endif\n\n')
        fileHeader.close()
        utils.updateIfChanged(fileName)

def generateConversions(paramList):
    'generate code to convert arrays to vectors/matrices'
    ret = ''
    indent = 8 * ' ';
    bigIndent = 12 * ' ';
    for param in paramList:
        if param[common.TYPE] == common.STRING:
            type = 'std::string'
            funcArray = 'convertStrVector'
        else:
            type = param[common.TYPE]
            funcArray = 'convertVector'
        if param[common.TENSOR] == common.VECTOR: 
            nmArray = param[common.NAME] + 'Vector'
            ret += indent + 'std::vector < ' + type + ' >' + nmArray + '\n' \
                + bigIndent + '= Conversion< ' + type + ' >::' \
                + funcArray + '(' + param[common.NAME] + ');\n'
        elif param[common.TENSOR] == common.MATRIX: 
            nmMatrix = param[common.NAME] + 'Matrix'
            ret += indent + 'std::vector < std::vector < ' + type + ' > >' \
                + nmMatrix + '\n' \
                + bigIndent + '= Conversion< ' + type + ' >::' \
                + 'convertMatrix(' + param[common.NAME] + ');\n'            
    return ret

def generateFuncSource(fileFunc, function, bufBody):
    'generate source for given function'
    global plCtor, plMember
    fileFunc.write(FUNC_PROTOTYPE % function[common.CODENAME])
    generateHeader(fileFunc, function, ' {')
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
    conversions = generateConversions(function[common.PARAMS])
    fileFunc.write(bufBody % (conversions, functionBody, functionName,
        paramList, function[common.NAME]))

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
        fileFunc.write(bufInclude)
        plCtor = params.ParameterPass(2, convertString = STR_FMT,
            delimiter = ';\n', appendTensor = True,
            wrapFormat = 'args.push(%s)', delimitLast = True)
        plMember = params.ParameterPass(3, convertString = STR_FMT,
            skipFirst = True)
        for function in functionGroup[common.FUNCLIST]:
            generateFuncSource(fileFunc, function, bufBody)
        fileFunc.close()
        utils.updateIfChanged(fileName)

def getReturnTypeCalcIDL(retVal):
    'derive return type for function'
    if retVal[common.TENSOR] == common.VECTOR or \
       retVal[common.TENSOR] == common.MATRIX or \
       retVal[common.TYPE]   == common.ANY:
        return CALC_MATRIX_IDL
    elif retVal[common.TYPE] == common.BOOL:
        return CALC_BOOL_IDL
    elif retVal[common.TYPE] == common.STRING:
        return common.STRING
    elif retVal[common.TYPE] == common.LONG:
        return common.LONG
    elif retVal[common.TYPE] == common.DOUBLE:
        return common.DOUBLE
    else:
        raise ValueError, 'unexpected return type: ' + returnType

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
        plIdl = params.ParameterDeclare(6, prefix = '[in] ',
            replaceTensorStr = 'any',
            formatVector = FORMAT_TENSOR_IDL, 
            formatMatrix = FORMAT_TENSOR_IDL)
        for function in functionGroup[common.FUNCLIST]:
            if function[common.CTOR]:
                handle = 24 * ' ' + '[in] string handle,\n'
            else:
                handle = ''
            returnTypeIDL = getReturnTypeCalcIDL(function[common.RETVAL])
            paramList = plIdl.generateCode(function[common.PARAMS])
            if paramList == '':
                carriageReturn = ''
            else:
                carriageReturn = '\n'
            fileIDL.write(bufIDLFunc % (returnTypeIDL, 
                function[common.CODENAME], carriageReturn, handle, paramList))
    bufIDLFoot = utils.loadBuffer(IDL_FOOT)
    fileIDL.write(bufIDLFoot)
    fileIDL.close()
    utils.updateIfChanged(fileName)

def generate(functionDefs):
    global plHeader
    'generate source code for Calc addin'
    utils.logMessage('  begin generating Calc ...')
    plHeader = params.ParameterDeclare(2, replaceString = 'const STRING',
        replaceLong = CALC_LONG, replaceTensorStr = 'ANY', derefString = '&',
        formatVector = FORMAT_TENSOR, formatMatrix = FORMAT_TENSOR,
        derefTensor = '&')
    functionGroups = functionDefs[common.FUNCGROUPS]
    generateFuncMap(functionGroups)
    generateAutoHeader(functionGroups)
    generateHeaders(functionGroups)
    generateFuncSources(functionGroups)
    generateIDLSource(functionGroups)
    utils.logMessage('  done generating Calc.')

