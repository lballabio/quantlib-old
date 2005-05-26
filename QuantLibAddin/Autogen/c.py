'output C source files'

import common
import utils
import params

# constants

BODY = 'stub.C.body'
CONV_HANDLE = 'std::string(handle)'
CONV_STR = 'std::string(%s)'
INCLUDES = 'stub.C.includes'
ROOT = common.ADDIN_ROOT + 'C/'

# global variables

# parameter list objects
plHeader    = ''    # function prototypes

def generateFuncHeader(fileHeader, function, suffix):
    global plHeader
    'generate source for prototype of given function'
    fileHeader.write('int %s(\n' % function[common.NAME])
    if function[common.CTOR]:
        fileHeader.write('        char *handle,\n')
    fileHeader.write(plHeader.generateCode(function[common.PARAMS]))
    fileHeader.write(',\n        VariesList *result)' + suffix)

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
        else:
            type = param[common.TYPE]
        if param[common.TENSOR] == common.VECTOR: 
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

def generateFuncDefs(groupName, functionGroup):
    'generate source for function implementations'
    plCtor = params.ParameterPass(2, convertString = CONV_STR,
        delimiter = ';\n', appendTensor = True,
        wrapFormat = 'args.push(%s)', delimitLast = True)
    plMember = params.ParameterPass(3, convertString = CONV_STR, skipFirst = True)
    fileName = ROOT + groupName + '.cpp' + common.TEMPFILE
    fileFunc = file(fileName, 'w')
    utils.printHeader(fileFunc)
    bufInclude = utils.loadBuffer(INCLUDES)
    bufBody = utils.loadBuffer(BODY)
    fileFunc.write(bufInclude % groupName)
    for function in functionGroup[common.FUNCLIST]:
        generateFuncHeader(fileFunc, function, ' {\n')
        conversions = generateConversions(function[common.PARAMS])
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
        fileFunc.write(bufBody % (conversions, functionBody, functionName,
            paramList, function[common.NAME]))
    fileFunc.close()
    utils.updateIfChanged(fileName)

def generate(functionDefs):
    'generate source code for C addin'
    global plHeader
    plHeader = params.ParameterDeclare(2, replaceString = 'char',
        replaceTensorStr = 'char', arrayCount = True, derefString = '*',
        derefTensorString = '*')
    utils.logMessage('  begin generating C ...')
    functionGroups = functionDefs[common.FUNCGROUPS]
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        if functionGroup[common.HDRONLY]:
            continue
        generateFuncHeaders(groupName, functionGroup)
        generateFuncDefs(groupName, functionGroup)
    utils.logMessage('  done generating C.')

