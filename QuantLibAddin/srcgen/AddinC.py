
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

'C addin'

import Addin
import Function
import Config
import OutputFile
import common
import utils

class AddinC(Addin.Addin):
    'generate source code for C addin'

    def generate(self):
        'generate source code for C addin'
        utils.logMessage('  begin generating C ...')
        for category in Config.Config.getInstance().getCategories(self.platformId):
            if category.headerOnly: continue
            self.generateHeaders(category)
            self.generateFuncSources(category)
        utils.logMessage('  done generating C.')

    def generateHeader(self, fileHeader, function, suffix):
        'generate source for prototype of given function'
        functionReturnType = self.functionReturnType.apply(function.returnValue)
        fileHeader.write('int %s(' % function.name)
        if isinstance(function, Function.Constructor):
            fileHeader.write('\n        char *handle,')
        functionDeclaration = self.generateCode(self.functionDeclaration, 
            function.Parameters)
        if functionDeclaration:
            functionDeclaration += ',\n'
        fileHeader.write(functionDeclaration)
        fileHeader.write('        %sresult)%s' % (functionReturnType, suffix))

    def generateHeaders(self, category):
        'generate source for function prototypes'
        fileHeader = OutputFile.OutputFile(self.rootDirectory + category.name + '.h')
        fileHeader.write('#ifndef qla_%s_h\n' % category.name)
        fileHeader.write('#define qla_%s_h\n\n' % category.name)
        for function in category.getFunctions(self.platformId): 
            self.generateHeader(fileHeader, function, ';\n\n')
        fileHeader.write('#endif\n\n')
        fileHeader.close()

    def getReturnCommand(self, returnValue):
        'generate code to convert datatype of return value'
        if returnValue.tensorRank == common.VECTOR \
        or returnValue.tensorRank == common.MATRIX \
        or returnValue.type == common.ANY:
            return returnValue.tensorRank + \
                'ToVaries(result, returnValue)'
        if returnValue.type == common.STRING:
            return 'strcpy(result, returnValue.c_str())'
        else:
            return '*result = returnValue'

    def generateConstructor(self, fileFunc, function):
        conversions = self.generateConversions(function.Parameters)
        libraryCall = self.generateCode(self.libraryCall, function.Parameters)
        fileFunc.write(self.bufferConstructor.text % (conversions, 
            function.libraryFunction, libraryCall, function.name))

    def generateMember(self, fileFunc, function):
        conversions = self.generateConversions(function.Parameters)
        libraryCall = self.generateCode(self.libraryCall, 
            function.Parameters, True, True)
        libraryReturnType = self.libraryReturnType.apply(function.returnValue)
        functionReturnCommand = self.getReturnCommand(function.returnValue)
        fileFunc.write(self.bufferMember.text % (conversions, function.libraryClass, 
            function.libraryClass, libraryReturnType, function.accessLibFunc, 
            libraryCall, functionReturnCommand, function.name))

    def generateFuncSources(self, category):
        'generate source for function implementations'
        fileFunc = OutputFile.OutputFile(self.rootDirectory + category.name + '.cpp')
        fileFunc.write(self.bufferIncludes.text % (category.name, category.name))
        for function in category.getFunctions(self.platformId): 
            self.generateHeader(fileFunc, function, ' {\n')
            if isinstance(function, Function.Constructor):
                self.generateConstructor(fileFunc, function)
            else:
                self.generateMember(fileFunc, function)
        fileFunc.close()

