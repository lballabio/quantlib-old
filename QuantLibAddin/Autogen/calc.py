'output calc source files'

import common
import utils

# constants

ROOT            = common.ADDIN_ROOT + 'Calc/'
MAPFILE         = 'funcdef.cpp'
MAPLINE         ='    funcMap[ STRFROMANSI( "%s" ) ]\n\
        =  STRFROMANSI( "%s" );\n'
AUTOHDR         = 'autogen.hpp'
IDL             = 'QuantLibAddin.idl'
MAP             = 'stub.Calc.map'
INCLUDES        = 'stub.Calc.includes'
BODY            = 'stub.Calc.body'
IDL_HEAD        = 'stub.Calc.idlhead'
IDL_FOOT        = 'stub.Calc.idlfoot'
IDL_FUNC        = 'stub.Calc.idlfunc'
CALC_BOOL       = 'sal_Bool'
CALC_BOOL_IDL   = 'boolean'
CALC_LONG       = 'sal_Int32'
CALC_MATRIX     = 'SEQSEQ(ANY)'
CALC_MATRIX_IDL = 'sequence < sequence < any > >'
CALC_STRING     = 'STRING'
STR_FMT         = 'OUStringToString(%s)'

def generateFuncMap(functionGroups):
    'generate array that lists all functions in the addin'
    fileName = ROOT + MAPFILE + common.TEMPFILE
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
    fileMap.write('\n}\n\n')
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
    'generate implementation for given function'
    if function[common.CTOR]:
        fileHeader.write('\n        const STRING & handle,')
    paramList = utils.generateParamList(function[common.PARAMS], 2, True,
        '', 'const STRING &', CALC_LONG,
        convertVec = 'const SEQSEQ(%s)& ',
        convertMat = 'const SEQSEQ(%s)& ', 
		convertMatStr = 'ANY')
    if paramList != '':
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
    fileFunc.write('SEQSEQ( ANY ) SAL_CALL QLAddin::%s(' 
        % function[common.CODENAME])
    generateHeader(fileFunc, function, ' {')
    if function[common.CTOR]:
        handle = 12 * ' ' + 'OUStringToString(handle), args'
        fName = 'OH_OBJECT_MAKE(QuantLibAddin::%s)' % function[common.QLFUNC]
        args = utils.generateArgList(function[common.PARAMS], 
            reformatString = STR_FMT)
        paramList = ''
    else:
        handle = ''
        fName = 'QuantLibAddin::' + function[common.NAME]
        args = ''
        paramList = utils.generateParamList(function[common.PARAMS], 3,
            reformatString = STR_FMT, arrayCount = True, 
			appendTensor = True,
            convertMatStr = 'ANY')
    conversions = generateConversions(function[common.PARAMS])
    fileFunc.write(bufBody % (conversions, args, fName, handle,
        paramList, function[common.NAME]))

def generateFuncSources(functionGroups):
    'generate source for function implementations'
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
            returnTypeIDL = getReturnTypeCalcIDL(function[common.RETVAL])
            paramList = utils.generateParamList(function[common.PARAMS],
                 6, True, '[in] ', 'string',
                convertVec = 'sequence < sequence < %s > > ',
                convertMat = 'sequence < sequence < %s > > ',
                convertMatStr = 'any')
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
    'generate source code for Calc addin'
    utils.logMessage('  begin generating Calc ...')
    functionGroups = functionDefs[common.FUNCGROUPS]
    generateFuncMap(functionGroups)
    generateAutoHeader(functionGroups)
    generateHeaders(functionGroups)
    generateFuncSources(functionGroups)
    generateIDLSource(functionGroups)
    utils.logMessage('  done generating Calc.')

