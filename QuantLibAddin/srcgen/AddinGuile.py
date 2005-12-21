
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

import Addin
import Function
import Config
import OutputFile
import common
import Log

class AddinGuile(Addin.Addin):
    'generate source code for Guile addin'

    def generate(self):
        'generate source code for Guile addin'
        Log.Log.getInstance().logMessage('  begin generating Guile ...')
        self.generateInitFunc()
        self.generateFuncDefs()
        Log.Log.getInstance().logMessage('  done generation Guile.')

    def generateFuncHeader(self, fileHeader, function, suffix):
        'generate source for prototype of given function'
        fileHeader.write('SCM %s(' % function.name)
        fileHeader.write('SCM x')
        fileHeader.write(')%s\n' % suffix)

    def generateFuncHeaders(self, category):
        'generate source for function prototypes'
        fileHeader = OutputFile.OutputFile(self.rootDirectory + category.name + '.h')
        fileHeader.write('#ifndef qla_%s_h\n' % category.name)
        fileHeader.write('#define qla_%s_h\n\n' % category.name)
        fileHeader.write('#include <guile/gh.h>\n\n')
        for function in category.getFunctions(self.platformId): 
            self.generateFuncHeader(fileHeader, function, ';\n')
        fileHeader.write('#endif\n\n')
        fileHeader.close()

    def generateRegistrations(self, category):
        'generate code to register function'
        ret = '    /* ' + category.displayName + ' */\n'
        stub = '    gh_new_procedure("%s", %s, 1, 0, 0);\n'
        for function in category.getFunctions(self.platformId): 
            ret += stub % (function.name, function.name)
        return ret

    def generateInitFunc(self):
        'generate initialisation function'
        fileInit = OutputFile.OutputFile(self.rootDirectory + 'qladdin.c')
        headers = ''
        registrations = ''
        i = 0
        for category in Config.Config.getInstance().getCategories(self.platformId):
            i += 1
            headers += '#include <' + category.name + '.h>\n'
            registrations += self.generateRegistrations(category)
            if i < len(Config.Config.getInstance().categoryDict):
                registrations += '\n'
        fileInit.write(self.bufferInitFunc.text % (headers, registrations))
        fileInit.close()

    def generateConversions(self, paramList):
        'generate code to convert datatypes'
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
        'generate source code for function return command'
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
        libraryCall = self.generateCode(self.libraryCall, function.Parameters)
        conversions = self.generateConversions(function.Parameters)
        fileFunc.write(self.bufferConstructor.text % (conversions, 
            function.libraryFunction, libraryCall, function.name))

    def generateMember(self, fileFunc, function):
        'generate source code for body of member function'
        conversions = self.generateConversions(function.Parameters)
        libraryReturnType = self.libraryReturnType.apply(function.returnValue)
        libraryCall = self.generateCode(self.libraryCall, 
            function.Parameters, True, True)
        functionReturnCommand = self.getReturnCommand(function.returnValue)
        fileFunc.write(self.bufferMember.text % (conversions, function.libraryClass, 
            function.libraryClass, libraryReturnType, function.accessLibFunc, 
            libraryCall, functionReturnCommand, function.name))

    def generateFuncDefs(self):
        'generate source for function implementations'
        for category in Config.Config.getInstance().getCategories(self.platformId):
            self.generateFuncHeaders(category)
            if category.headerOnly: continue
            fileFunc = OutputFile.OutputFile(self.rootDirectory + category.name + '.cpp')
            fileFunc.write(self.bufferIncludes.text % (category.name, category.name))
            for function in category.getFunctions(self.platformId): 
                self.generateFuncHeader(fileFunc, function, ' {')
                if isinstance(function, Function.Constructor):
                    self.generateConstructor(fileFunc, function)
                else:
                    self.generateMember(fileFunc, function)
            fileFunc.close()

