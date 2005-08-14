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

'output Guile source files'

import common
import utils
import params

# constants

BUF_CTOR    = 'stub.guile.constructor'
BUF_MEMBER  = 'stub.guile.member'
INCLUDES    = 'stub.guile.includes'
INITFUNC    = 'stub.guile.initfunc'
ROOT        = common.ADDIN_ROOT + 'Guile/'

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
    for function in functionGroup[common.FUNCS]:
        if not utils.checkFunctionPlatform(function, common.PLATFORM_GUILE):
            continue
        generateFuncHeader(fileHeader, function, ';\n')
    fileHeader.write('#endif\n\n')
    fileHeader.close()
    utils.updateIfChanged(fileName)

def generateRegistrations(functionGroup):
    ret = '    /* ' + functionGroup[common.DISPLAYNAME] + ' */\n'
    stub = '    gh_new_procedure("%s", %s, 1, 0, 0);\n'
    funcList = functionGroup[common.FUNCS]
    for function in funcList:
        if not utils.checkFunctionPlatform(function, common.PLATFORM_GUILE):
            continue
        name = function[common.CODENAME]
        ret += stub % (name, name)
    return ret

def generateInitFunc(functionGroups):
    'generate initialisation function'
    fileName = ROOT + 'qladdin.c' + common.TEMPFILE
    fileInit = file(fileName, 'w')
    fileStub = utils.loadBuffer(INITFUNC)
    utils.printHeader(fileInit)
    headers = ''
    registrations = ''
    i = 0
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

def generateConstructor(fileFunc, function, bufCtor, plCtor):
    conversions = getConversions(function[common.PARAMS])
    paramList = plCtor.generateCode(function[common.PARAMS])
    fileFunc.write(bufCtor % (conversions, function[common.QLFUNC], 
        paramList, function[common.NAME]))

def generateMember(fileFunc, function, bufMember, plMember):
    conversions = getConversions(function[common.PARAMS])
    className = function[common.PARAMS][0][common.ATTS][common.CLASS]
    returnType = utils.getReturnType(function[common.RETVAL], replacePropertyVector = 'ObjHandler::Properties',
        replaceString = 'std::string', replaceAny = 'boost::any')
    functionName = utils.generateFuncCall(function)
    paramList = plMember.generateCode(function[common.PARAMS])
    returnCall = generateReturnCall(function[common.RETVAL])
    fileFunc.write(bufMember % (conversions, className, className, returnType, 
        functionName, paramList, returnCall, function[common.NAME]))

def generateFuncDefs(functionGroups):
    'generate source for function implementations'
    bufInclude = utils.loadBuffer(INCLUDES)
    bufCtor = utils.loadBuffer(BUF_CTOR)
    bufMember = utils.loadBuffer(BUF_MEMBER)
    plCtor = params.ParameterPass(3)
    plMember = params.ParameterPass(3, skipFirst = True)
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        generateFuncHeaders(groupName, functionGroup)
        if functionGroup[common.HDRONLY] == common.TRUE:
            continue
        fileName = ROOT + groupName + '.cpp' + common.TEMPFILE
        fileFunc = file(fileName, 'w')
        utils.printHeader(fileFunc)
        fileFunc.write(bufInclude % (groupName, groupName))
        for function in functionGroup[common.FUNCS]:
            if not utils.checkFunctionPlatform(function, common.PLATFORM_GUILE):
                continue
            generateFuncHeader(fileFunc, function, ' {')
            if function[common.CTOR] == common.TRUE:
                generateConstructor(fileFunc, function, bufCtor, plCtor)
            else:
                generateMember(fileFunc, function, bufMember, plMember)
        fileFunc.close()
        utils.updateIfChanged(fileName)

def generate(functionDefs):
    'generate source code for Guile addin'
    utils.logMessage('  begin generating Guile ...')
    generateInitFunc(functionDefs)
    generateFuncDefs(functionDefs)
    utils.logMessage('  done generation Guile.')

