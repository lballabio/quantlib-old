
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

"""generate source code for C addin."""

import addin
import function
import config
import outputfile
import common
import log

class AddinC(addin.Addin):
    """generate source code for C addin."""

    def generate(self):
        """generate source code for C addin."""
        log.Log.getInstance().logMessage('  begin generating C ...')
        for category in config.Config.getInstance().getCategories(self.platformId):
            if category.headerOnly: continue
            self.generateHeaders(category)
            self.generateFuncSources(category)
        log.Log.getInstance().logMessage('  done generating C.')

    def generateHeader(self, fileHeader, func, suffix):
        """generate source for prototype of given function."""
        functionReturnType = self.functionReturnType.apply(func.returnValue)
        fileHeader.write('int %s(' % func.name)
        if isinstance(func, function.Constructor):
            fileHeader.write('\n        char *handle,')
        functionDeclaration = self.generateCode(self.functionDeclaration, 
            func.Parameters)
        if functionDeclaration:
            functionDeclaration += ',\n'
        fileHeader.write(functionDeclaration)
        fileHeader.write('        %sresult)%s' % (functionReturnType, suffix))

    def generateHeaders(self, category):
        """generate source for function prototypes."""
        fileHeader = outputfile.OutputFile(self.rootDirectory + category.name + '.h')
        fileHeader.write('#ifndef qla_%s_h\n' % category.name)
        fileHeader.write('#define qla_%s_h\n\n' % category.name)
        for func in category.getFunctions(self.platformId): 
            self.generateHeader(fileHeader, func, ';\n\n')
        fileHeader.write('#endif\n\n')
        fileHeader.close()

    def getReturnCommand(self, returnValue):
        """generate code to convert datatype of return value."""
        if returnValue.tensorRank == common.VECTOR \
        or returnValue.tensorRank == common.MATRIX \
        or returnValue.type == common.ANY:
            return returnValue.tensorRank + \
                'ToVaries(result, returnValue)'
        if returnValue.type == common.STRING:
            return 'strcpy(result, returnValue.c_str())'
        else:
            return '*result = returnValue'

    def generateConstructor(self, fileFunc, func):
        """generate source code for constructor."""
        conversions = self.generateConversions(func.Parameters)
        libraryCall = self.generateCode(self.libraryCall, func.Parameters)
        fileFunc.write(self.bufferConstructor.text % (conversions, 
            func.libraryFunction, libraryCall, func.name))

    def generateMember(self, fileFunc, func):
        """generate source code for member function."""
        conversions = self.generateConversions(func.Parameters)
        libraryCall = self.generateCode(self.libraryCall, 
            func.Parameters, True, True)
        libraryReturnType = self.libraryReturnType.apply(func.returnValue)
        functionReturnCommand = self.getReturnCommand(func.returnValue)
        fileFunc.write(self.bufferMember.text % (conversions, func.libraryClass, 
            func.libraryClass, libraryReturnType, func.accessLibFunc, 
            libraryCall, functionReturnCommand, func.name))

    def generateFuncSources(self, category):
        """generate source for function implementations."""
        fileFunc = outputfile.OutputFile(self.rootDirectory + category.name + '.cpp')
        fileFunc.write(self.bufferIncludes.text % (category.name, category.name))
        for func in category.getFunctions(self.platformId): 
            self.generateHeader(fileFunc, func, ' {\n')
            if isinstance(func, function.Constructor):
                self.generateConstructor(fileFunc, func)
            else:
                self.generateMember(fileFunc, func)
        fileFunc.close()

