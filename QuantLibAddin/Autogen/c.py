'output C source files'

import common
import utils
import params

# constants

BODY = 'stub.C.body'
CONV_HANDLE = 'std::string(handle)'
CONV_BOOL = '%s == TRUE'
CONV_STR = 'std::string(%s)'
INCLUDES = 'stub.C.includes'
ROOT = common.ADDIN_ROOT + 'C/'

# global variables

# parameter list objects
plHeader    = ''    # function prototypes

def generateFuncHeader(fileHeader, function, suffix):
    global plHeader
    'generate source for prototype of given function'
    returnType = utils.getReturnType(function[common.RETVAL], 
        replaceString = 'char', replaceAny = 'Varies', replaceBool = 'Boolean',
        replaceTensorAny = 'VariesList *', replacePropertyVector = 'VariesList *',
        formatScalar = '%s *', formatVector = '%s **', formatMatrix = '%s **')
    fileHeader.write('int %s(' % function[common.NAME])
    if function[common.CTOR]:
        fileHeader.write('\n        char *handle,')
    fileHeader.write(plHeader.generateCode(function[common.PARAMS]))
    fileHeader.write('        %sresult)%s' % (returnType, suffix))

def generateFuncHeaders(groupName, functionGroup):
    'generate source for function prototypes'
    fileName = ROOT + groupName + '.h' + common.TEMPFILE
    fileHeader = file(fileName, 'w')
    utils.printHeader(fileHeader)
    fileHeader.write('#ifndef qla_%s_h\n' % groupName)
    fileHeader.write('#define qla_%s_h\n\n' % groupName)
    for function in functionGroup[common.FUNCLIST]:
        generateFuncHeader(fileHeader, function, ';\n\n')
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
        elif param[common.TYPE] == common.ANY:
            type = 'boost::any'
        else:
            type = param[common.TYPE]
        if param[common.TENSOR] == common.SCALAR \
        and param[common.TYPE] == common.ANY:
            ret += indent + 'boost::any ' + param[common.NAME] \
            + 'Scalar = variesToBoostAny(' + param[common.NAME] + ');\n'
        elif param[common.TENSOR] == common.VECTOR: 
            nmArray = param[common.NAME] + 'Vector'
            nmSize = param[common.NAME] + 'Size'
            ret += indent + 'std::vector < ' + type + ' >' + nmArray + '\n' \
                + bigIndent + '= Conversion< ' + type + ' >::' \
                + 'convertVector(' + param[common.NAME] \
                + ', ' + nmSize + ');\n'
        elif param[common.TENSOR] == common.MATRIX: 
            nmMatrix = param[common.NAME] + 'Matrix'
            nmRows = param[common.NAME] + 'Rows'
            nmCols = param[common.NAME] + 'Cols'
            ret += indent + 'std::vector < std::vector < ' + type + ' > >' \
                + nmMatrix + '\n' \
                + bigIndent + '= Conversion< ' + type + ' >::' \
                + 'convertMatrix(' + param[common.NAME] \
                + ', ' + nmRows + ', ' + nmCols + ');\n'            
    return ret

def getReturnCall(returnDef):
    'generate code to convert datatype of return value'
    if returnDef[common.TYPE] == common.PROPERTY:
        if returnDef[common.TENSOR] == common.VECTOR:
            return 'propertyVectorToVariesList(returnValue, result)'
        else:
            raise ValueError, 'type property can only be combined with tensorrank vector'

    if returnDef[common.TYPE] == common.ANY:
        if returnDef[common.TENSOR] == common.SCALAR:
            return 'boostAnyToVaries(returnValue, result)'
        elif returnDef[common.TENSOR] == common.VECTOR:
            return 'boostAnyVectorToVaries(returnValue, result)'
        else:
            return 'boostAnyMatrixToVaries(returnValue, result)'

    if returnDef[common.TYPE] == common.BOOL \
    and returnDef[common.TENSOR] == common.SCALAR:
        return '*result = (Boolean) returnValue'

    if returnDef[common.TYPE] == common.STRING:
        if returnDef[common.TENSOR] == common.SCALAR:
            return 'strcpy(result, returnValue.c_str());'
        else:
            type = 'std::string'
    else:
        type = returnDef[common.TYPE]

    if returnDef[common.TENSOR] == common.SCALAR:
        return '*result = returnValue'
    elif returnDef[common.TENSOR] == common.VECTOR:
        return 'Conversion< %s >::convertArray(returnValue, result)' % type
    else:
        return 'Conversion< %s >::convertArrayArray(returnValue, result)' % type

def generateFuncSources(groupName, functionGroup):
    'generate source for function implementations'
    plCtor = params.ParameterPass(2, convertString = CONV_STR, convertBool = CONV_BOOL,
        delimiter = ';\n', appendTensor = True, appendScalar = True,
        wrapFormat = 'args.push(%s)', delimitLast = True, prependEol = False)
    plMember = params.ParameterPass(3, convertString = CONV_STR, convertBool = CONV_BOOL,
        skipFirst = True, appendTensor = True, appendScalar = True)
    fileName = ROOT + groupName + '.cpp' + common.TEMPFILE
    fileFunc = file(fileName, 'w')
    utils.printHeader(fileFunc)
    bufInclude = utils.loadBuffer(INCLUDES)
    bufBody = utils.loadBuffer(BODY)
    fileFunc.write(bufInclude % (groupName, groupName))
    for function in functionGroup[common.FUNCLIST]:
        generateFuncHeader(fileFunc, function, ' {\n')
        conversions = generateConversions(function[common.PARAMS])
        returnType = utils.getReturnType(function[common.RETVAL],
            replacePropertyVector = 'Properties', replaceString = 'std::string',
            replaceAny = 'boost::any')
        returnCall = getReturnCall(function[common.RETVAL])
        if function[common.CTOR]:
            functionBody = common.ARGLINE + plCtor.generateCode(function[common.PARAMS])
            functionName = common.MAKE_FUNCTION
            paramList = common.MAKE_ARGS % (function[common.QLFUNC], common.HANDLE)
        else:
            className = function[common.PARAMS][0][common.CLASS]
            functionBody = common.FUNC_BODY % (className, className, CONV_HANDLE,
                function[common.NAME], CONV_HANDLE)
            functionName = 'objectPointer->' + function[common.QLFUNC]
            paramList = plMember.generateCode(function[common.PARAMS])
        fileFunc.write(bufBody % (conversions, functionBody, returnType,
            functionName, paramList, returnCall, function[common.NAME]))
    fileFunc.close()
    utils.updateIfChanged(fileName)

def generate(functionDefs):
    'generate source code for C addin'
    global plHeader
    plHeader = params.ParameterDeclare(2, replaceString = 'char',
        replaceTensorStr = 'char', arrayCount = True, derefString = '*',
        derefTensorString = '*', replaceAny = 'Varies', delimitLast = True,
        replaceTensorAny = 'VariesList', replaceBool = 'Boolean')
    utils.logMessage('  begin generating C ...')
    functionGroups = functionDefs[common.FUNCGROUPS]
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        if functionGroup[common.HDRONLY]:
            continue
        generateFuncHeaders(groupName, functionGroup)
        generateFuncSources(groupName, functionGroup)
    utils.logMessage('  done generating C.')

