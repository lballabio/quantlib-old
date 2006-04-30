
"""
 Copyright (C) 2005, 2006 Eric Ehlers
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
    """Generate source code for C addin."""

    def generate(self):
        """Generate source code for C addin."""
        log.Log.getInstance().logMessage('  begin generating C ...')
        for category in config.Config.getInstance().getCategories(self.platformId):
            self.generateHeaders(category)
            self.generateFunctions(category)
        log.Log.getInstance().logMessage('  done generating C.')

    def generateHeader(self, fileHeader, func, suffix):
        """Generate source for prototype of given function."""
        functionReturnType = self.functionReturnType.apply(func.returnValue)
        fileHeader.write('int %s(' % func.name)
        functionDeclaration = func.generateParameterList(self.functionDeclaration, 
            'char *handle')
        if functionDeclaration: functionDeclaration += ','
        fileHeader.write(functionDeclaration)
        fileHeader.write('\n        %sresult)%s' % (functionReturnType, suffix))

    def generateHeaders(self, category):
        """Generate source for function prototypes."""
        fileHeader = outputfile.OutputFile(self, self.rootDirectory + category.name + '.h')
        fileHeader.write('#ifndef qla_%s_h\n' % category.name)
        fileHeader.write('#define qla_%s_h\n\n' % category.name)
        for func in category.getFunctions(self.platformId): 
            self.generateHeader(fileHeader, func, ';\n\n')
        fileHeader.write('#endif\n\n')
        fileHeader.close()

    def generateReturnCommand(self, returnValue):
        """Generate code to convert datatype of return value."""
        if returnValue.tensorRank == common.VECTOR \
        or returnValue.tensorRank == common.MATRIX \
        or returnValue.type == common.ANY:
            return returnValue.tensorRank + \
                'ToVaries(result, returnValue)'
        if returnValue.type == common.STRING:
            return 'strcpy(result, returnValue.c_str())'
        elif returnValue.type == common.BOOL:
            return '*result = returnValue ? TRUE : FALSE'
        elif returnValue.type == common.VOID:
            return '*result = TRUE'
        else:
            return '*result = returnValue'

    def generateFunction(self, fileFunc, func):
        """Generate source code for function."""
        conversions = self.generateConversions(func.Parameters)
        functionBody = func.generateBody(self)
        functionReturnCommand = self.generateReturnCommand(func.returnValue)
        fileFunc.write(self.bufferFunction.text % (conversions, 
            functionBody, functionReturnCommand, func.name))

    def generateFunctions(self, category):
        """Generate source for function implementations."""
        fileFunc = outputfile.OutputFile(self, self.rootDirectory + category.name + '.cpp')
        fileFunc.write(self.bufferIncludes.text % (category.includeList(), category.name))
        for func in category.getFunctions(self.platformId): 
            self.generateHeader(fileFunc, func, ' {\n')
            self.generateFunction(fileFunc, func)
        fileFunc.close()

