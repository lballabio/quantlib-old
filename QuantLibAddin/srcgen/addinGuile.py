
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

'guile addin'

import addin
import common
import utils
import category
import rule

# constants

BUF_CTOR    = 'stub.guile.constructor'
BUF_MEMBER  = 'stub.guile.member'
INCLUDES    = 'stub.guile.includes'
INITFUNC    = 'stub.guile.initfunc'

class AddinGuile(addin.Addin):

    def __init__(self,
            categories):
        super(AddinGuile, self).__init__('guile', categories)
        self.bufInclude = utils.loadBuffer(INCLUDES)
        self.bufCtor = utils.loadBuffer(BUF_CTOR)
        self.bufMember = utils.loadBuffer(BUF_MEMBER)

    def setRules(self, config):
        self.ruleLibraryReturnType = rule.Rule(config[common.LIB_RET])
        self.ruleLibraryCall = rule.Rule(config[common.LIB_CALL])

    def generate(self):
        'generate source code for Guile addin'
        utils.logMessage('  begin generating Guile ...')
        self.generateInitFunc()
        self.generateFuncDefs()
        utils.logMessage('  done generation Guile.')

    def generateFuncHeader(self, fileHeader, function, suffix):
        'generate source for prototype of given function'
        fileHeader.write('SCM %s(' % function.name)
        fileHeader.write('SCM x')
        fileHeader.write(')%s\n' % suffix)

    def generateFuncHeaders(self, category):
        'generate source for function prototypes'
        fileName = self.rootDir + category.name + '.h' + common.TEMPFILE
        fileHeader = file(fileName, 'w')
        utils.printHeader(fileHeader)
        fileHeader.write('#ifndef qla_%s_h\n' % category.name)
        fileHeader.write('#define qla_%s_h\n\n' % category.name)
        fileHeader.write('#include <guile/gh.h>\n\n')
        for functionKey in category.functions[common.KEYS]:
            function = category.functions[common.DICT][functionKey]
            if function.platformSupported(self.platformId):
                self.generateFuncHeader(fileHeader, function, ';\n')
        fileHeader.write('#endif\n\n')
        fileHeader.close()
        utils.updateIfChanged(fileName)

    def generateRegistrations(self, cat):
        ret = '    /* ' + cat.displayName + ' */\n'
        stub = '    gh_new_procedure("%s", %s, 1, 0, 0);\n'
        for functionKey in cat.functions[common.KEYS]:
            function = cat.functions[common.DICT][functionKey]
            if function.platformSupported(self.platformId):
                ret += stub % (function.name, function.name)
        return ret

    def generateInitFunc(self):
        'generate initialisation function'
        fileName = self.rootDir + 'qladdin.c' + common.TEMPFILE
        fileInit = file(fileName, 'w')
        fileStub = utils.loadBuffer(INITFUNC)
        utils.printHeader(fileInit)
        headers = ''
        registrations = ''
        i = 0
        for categoryKey in self.categories[common.KEYS]:
            category = self.categories[common.DICT][categoryKey]
            i += 1
            headers += '#include <' + category.name + '.h>\n'
            registrations += self.generateRegistrations(category)
            if i < len(self.categories[common.KEYS]):
                registrations += '\n'
        fileInit.write(fileStub % (headers, registrations))
        fileInit.close()
        utils.updateIfChanged(fileName)

    def generateConversions(self, paramList):
        ret = ''
        firstItem = True
        for param in paramList:
            if param.ignore:
                continue
            if param.type == common.STRING:
                type1 = 'std::string'
            elif param.type == common.ANY:
                type1 = 'boost::any'
            else:
                type1 = param.type
            type2 = type1
            if param.tensorRank == common.VECTOR:
                type2 = 'std::vector<%s>' % type1
            elif param.tensorRank == common.MATRIX:
                type2 = 'std::vector<std::vector<%s> >' % type1
            ret += 8 * ' ' + '%s %s = GetChop<%s>::%s(x);\n' % (
                type2, param.name, type1, param.tensorRank)
        return ret

    def getReturnCommand(self, returnValue):
        if returnValue.tensorRank == common.SCALAR:
            arg = 'boost::any(returnValue)'
        else:
            arg = 'returnValue'
        tensor = returnValue.tensorRank
        if returnValue.type == common.STRING:
            type = 'std::string'
        elif returnValue.type == common.ANY:
            type = 'boost::any'
        else:
            type = returnValue.type
        return ('Nat2Scm<%s>::%s(%s)' % (type, tensor, arg))

    def generateConstructor(self, fileFunc, function):
        'generate source code for body of constructor function'
        libraryCall = self.generateCode(self.ruleLibraryCall, function.parameters)
        conversions = self.generateConversions(function.parameters)
        fileFunc.write(self.bufCtor % (conversions, function.libFunction,
            libraryCall, function.name))

    def generateMember(self, fileFunc, function):
        conversions = self.generateConversions(function.parameters)
        libraryReturnType = self.ruleLibraryReturnType.apply(function.returnValue)
        libraryCall = self.generateCode(self.ruleLibraryCall, function.parameters, True, True)
        libraryFunctionName = utils.getLibFuncName(function)
        functionReturnCommand = self.getReturnCommand(function.returnValue)
        fileFunc.write(self.bufMember % (conversions, function.className, function.className,
            libraryReturnType, libraryFunctionName, libraryCall, functionReturnCommand, function.name))

    def generateFuncDefs(self):
        'generate source for function implementations'
        for categoryKey in self.categories[common.KEYS]:
            category = self.categories[common.DICT][categoryKey]
            self.generateFuncHeaders(category)
            if category.headerOnly:
                continue
            fileName = self.rootDir + category.name + '.cpp' + common.TEMPFILE
            fileFunc = file(fileName, 'w')
            utils.printHeader(fileFunc)
            fileFunc.write(self.bufInclude % (category.name, category.name))
            for functionKey in category.functions[common.KEYS]:
                function = category.functions[common.DICT][functionKey]
                if not function.platformSupported(self.platformId):
                    continue
                self.generateFuncHeader(fileFunc, function, ' {')
                if function.isConstructor:
                    self.generateConstructor(fileFunc, function)
                else:
                    self.generateMember(fileFunc, function)
            fileFunc.close()
            utils.updateIfChanged(fileName)

