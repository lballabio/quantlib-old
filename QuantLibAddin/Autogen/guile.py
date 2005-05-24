'output Guile source files'

import common
import utils

# constants

ROOT = common.ADDIN_ROOT + 'Guile/'
INCLUDES = 'stub.Guile.includes'
BODY = 'stub.Guile.body'
INITFUNC = 'stub.Guile.initfunc'
FUNC_BODY       = '\
        boost::shared_ptr<QuantLibAddin::%s> objectPointer =\n\
            OH_GET_OBJECT(QuantLibAddin::%s, handle);\n\
        if (!objectPointer)\n\
            QL_FAIL("%s: error retrieving object " + handle);\n'

def generateFuncHeader(fileHeader, function, suffix):
    'generate source for prototype of given function'
    fileHeader.write('SCM %s(' % function[common.CODENAME])
    fileHeader.write('SCM x')
    fileHeader.write(')%s\n' % suffix)

def generateFuncHeaders(groupName, functionGroup):
    'generate source for function prototypes'
    fileName = ROOT + groupName + '.h' + common.TEMPFILE
    fileHeader = file(fileName, 'w')
    utils.printHeader(fileHeader)
    fileHeader.write('#ifndef qla_%s_h\n' % groupName)
    fileHeader.write('#define qla_%s_h\n\n' % groupName)
    fileHeader.write('#include <guile/gh.h>\n\n')
    for function in functionGroup[common.FUNCLIST]:
        # member functions not supported for now
#        if function[common.CTOR]:
        generateFuncHeader(fileHeader, function, ';\n')
    fileHeader.write('#endif\n\n')
    fileHeader.close()
    utils.updateIfChanged(fileName)

def generateRegistrations(functionGroup):
    ret = '    /* ' + functionGroup[common.DISPLAYNAME] + ' */\n'
    stub = '    gh_new_procedure("%s", %s, 1, 0, 0);\n'
    funcList = functionGroup[common.FUNCLIST]
    for function in funcList:
        name = function[common.CODENAME]
        ret += stub % (name, name)
    return ret

def generateInitFunc(functionDefs):
    'generate initialisation function'
    fileName = ROOT + 'qladdin.c' + common.TEMPFILE
    fileInit = file(fileName, 'w')
    fileStub = utils.loadBuffer(INITFUNC)
    utils.printHeader(fileInit)
    headers = ''
    registrations = ''
    i = 0
    functionGroups = functionDefs[common.FUNCGROUPS]
    for groupName in functionGroups.keys():
        i += 1
        functionGroup = functionGroups[groupName]
        headers += '#include <' + groupName + '.h>\n'
        registrations += generateRegistrations(functionGroup)
        if i < len(functionGroups):
            registrations += '\n'
    fileInit.write(fileStub % (headers, registrations))
    fileInit.close()
    utils.updateIfChanged(fileName)

def generateArgList(paramList, indent):
    ret = ''
    for param in paramList:
        name = ''
        if param[common.TENSOR] == common.SCALAR:
            if param[common.TYPE] == common.BOOL:
                name = 'x = GET_ARGUMENT(x, args, scalar, bool);       '
            elif param[common.TYPE] == common.DOUBLE:
                name = 'x = GET_ARGUMENT(x, args, scalar, double);     '
            elif param[common.TYPE] == common.LONG:
                name = 'x = GET_ARGUMENT(x, args, scalar, long);       '
            elif param[common.TYPE] == common.STRING:
                name = 'x = GET_ARGUMENT(x, args, scalar, std::string);'
        elif param[common.TENSOR] == common.VECTOR:
            if param[common.TYPE] == common.BOOL:
                name = 'x = GET_ARGUMENT(x, args, vector, bool);       '
            elif param[common.TYPE] == common.DOUBLE:
                name = 'x = GET_ARGUMENT(x, args, vector, double);     '
            elif param[common.TYPE] == common.LONG:
                name = 'x = GET_ARGUMENT(x, args, vector, long);       '
            elif param[common.TYPE] == common.STRING:
                name = 'x = GET_ARGUMENT(x, args, vector, std::string);'
        elif param[common.TENSOR] == common.MATRIX:
            if param[common.TYPE] == common.BOOL:
                name = 'x = GET_ARGUMENT(x, args, matrix, bool);       '
            elif param[common.TYPE] == common.DOUBLE:
                name = 'x = GET_ARGUMENT(x, args, matrix, double);     '
            elif param[common.TYPE] == common.LONG:
                name = 'x = GET_ARGUMENT(x, args, matrix, long);       '
            elif param[common.TYPE] == common.STRING:
                name = 'x = GET_ARGUMENT(x, args, matrix, std::string);'
        ret += indent + name + ' // ' + param[common.NAME] + '\n'
    return ret

def getConversions(paramList):
    ret = ''
    firstItem = True
    for param in paramList:
        if param[common.TYPE] == common.STRING:
            type = 'std::string'
        else:
            type = param[common.TYPE]
        if firstItem:
            func = 'gh_car'
            firstItem = False
        else:
            func = 'gh_cadr'
        ret += 8 * ' ' + '%s %s = Convert<%s>::scalar(%s(x));\n' % (
            type, param[common.NAME], type, func)
    return ret

def generateFuncDefs(groupName, functionGroup):
    'generate source for function implementations'
    fileName = ROOT + groupName + '.cpp' + common.TEMPFILE
    fileFunc = file(fileName, 'w')
    utils.printHeader(fileFunc)
    bufInclude = utils.loadBuffer(INCLUDES)
    bufBody = utils.loadBuffer(BODY)
    fileFunc.write(bufInclude % groupName)
    for function in functionGroup[common.FUNCLIST]:
        generateFuncHeader(fileFunc, function, ' {')
        indent = 8 * ' ';
        if function[common.CTOR]:
            args = indent + 'ArgumentStack args;\n'
            args += generateArgList(function[common.PARAMS], indent);
            args += indent + 'x = GET_ARGUMENT(x, args, scalar, std::string); // handleObject \n'
            args += indent + 'std::string handle = OH_POP_ARGUMENT(std::string, args);\n'
            fName = 'OH_MAKE_OBJECT(QuantLibAddin::%s, handle, args)' % function[common.QLFUNC]
        else:
            args = getConversions(function[common.PARAMS])
            className = function[common.PARAMS][0][common.CLASS]
            args += FUNC_BODY % (className, className, function[common.NAME])
            paramList = utils.generateParamList(function[common.PARAMS], 3,
                arrayCount = True, appendTensor = True, skipFirst = True)
            fName = 'objectPointer->%s(\n%s)' % (function[common.QLFUNC], 
                paramList)
        fileFunc.write(bufBody % (args, fName, function[common.CODENAME]))
    fileFunc.close()
    utils.updateIfChanged(fileName)

def generate(functionDefs):
    'generate source code for Guile addin'
    utils.logMessage('  begin generating Guile ...')
    functionGroups = functionDefs[common.FUNCGROUPS]
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        generateFuncHeaders(groupName, functionGroup)
        if functionGroup[common.HDRONLY]:
            continue
        generateFuncDefs(groupName, functionGroup)
    generateInitFunc(functionDefs)
    utils.logMessage('  done generation Guile.')

