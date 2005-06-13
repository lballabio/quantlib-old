'output Guile source files'

import common
import utils
import params

# constants

ROOT = common.ADDIN_ROOT + 'Guile/'
INCLUDES = 'stub.Guile.includes'
BODY = 'stub.Guile.body'
INITFUNC = 'stub.Guile.initfunc'

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
        tensor = param[common.TENSOR]
        type = param[common.TYPE]
        if param[common.TYPE] == common.STRING:
            type = 'std::string'
        elif param[common.TYPE] == common.ANY:
            type = 'boost::any'
        stub = 'args.push(GetChop<%s>::%s(x)); // %s\n'
        ret += indent + (stub % (type, tensor, param[common.NAME]))
    return ret

def getConversions(paramList):
    ret = ''
    firstItem = True
    for param in paramList:
        if param[common.TYPE] == common.STRING:
            type1 = 'std::string'
        elif param[common.TYPE] == common.ANY:
            type1 = 'boost::any'
        else:
            type1 = param[common.TYPE]
        type2 = type1
        if param[common.TENSOR] == common.VECTOR:
            type2 = 'std::vector<%s>' % type1
        elif param[common.TENSOR] == common.MATRIX:
            type2 = 'std::vector<std::vector<%s> >' % type1
        ret += 8 * ' ' + '%s %s = GetChop<%s>::%s(x);\n' % (
            type2, param[common.NAME], type1, param[common.TENSOR])
    return ret

def generateReturnCall(returnDef):
    if returnDef[common.TYPE] == common.PROPERTY:
        if returnDef[common.TENSOR] == common.VECTOR:
            return 'propertiesToAList(returnValue)'
        else:
            raise ValueError, 'type property can only be combined with tensorrank vector'
    arg = 'returnValue'
    if returnDef[common.TENSOR] == common.SCALAR:
        arg = 'boost::any(returnValue)'
    tensor = returnDef[common.TENSOR]
    if returnDef[common.TYPE] == common.STRING:
        type = 'std::string'
    elif returnDef[common.TYPE] == common.ANY:
        type = 'boost::any'
    else:
        type = returnDef[common.TYPE]
    return ('Nat2Scm<%s>::%s(%s)' % (type, tensor, arg))

def generateFuncDefs(groupName, functionGroup):
    'generate source for function implementations'
    fileName = ROOT + groupName + '.cpp' + common.TEMPFILE
    fileFunc = file(fileName, 'w')
    utils.printHeader(fileFunc)
    bufInclude = utils.loadBuffer(INCLUDES)
    bufBody = utils.loadBuffer(BODY)
    fileFunc.write(bufInclude % groupName)
    plMember = params.ParameterPass(3, skipFirst = True)
    for function in functionGroup[common.FUNCLIST]:
        generateFuncHeader(fileFunc, function, ' {')
        indent = 8 * ' ';
        if function[common.CTOR]:
            args  = indent + 'std::string handle = GetChop<std::string>::scalar(x);\n'
            args += indent + 'ArgumentStack args;\n'
            args += generateArgList(function[common.PARAMS], indent);
            fName = 'OH_MAKE_OBJECT(%s, handle, args)' % function[common.QLFUNC]
        else:
            args = getConversions(function[common.PARAMS])
            className = function[common.PARAMS][0][common.CLASS]
            args += common.FUNC_BODY % (className, className, 'handle',
                function[common.CODENAME], 'handle')
            paramList = plMember.generateCode(function[common.PARAMS])
            fName = 'objectPointer->%s(%s)' % (function[common.QLFUNC], 
                paramList)
        retType = utils.getReturnType(function[common.RETVAL], replacePropertyVector = 'Properties',
            replaceString = 'std::string', replaceAny = 'boost::any')
        retCall = generateReturnCall(function[common.RETVAL])
        fileFunc.write(bufBody % (args, retType, fName, retCall, function[common.CODENAME]))
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

