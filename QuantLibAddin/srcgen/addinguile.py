
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

"""generate source code for Guile addin."""

import addin
import function
import config
import outputfile
import common
import log

class AddinGuile(addin.Addin):
    """Generate source code for Guile addin."""

    def generate(self):
        """Generate source code for Guile addin."""
        log.Log.getInstance().logMessage('  begin generating Guile ...')
        self.generateInitFunc()
        self.generateFunctions()
        log.Log.getInstance().logMessage('  done generation Guile.')

    def generateFuncHeader(self, fileHeader, func, suffix):
        """Generate source for prototype of given function."""
        fileHeader.write('SCM %s(' % func.name)
        fileHeader.write('SCM x')
        fileHeader.write(')%s\n' % suffix)

    def generateFuncHeaders(self, category):
        """Generate source for function prototypes."""
        fileHeader = outputfile.OutputFile(self.rootDirectory + category.name + '.h')
        fileHeader.write('#ifndef qla_%s_h\n' % category.name)
        fileHeader.write('#define qla_%s_h\n\n' % category.name)
        fileHeader.write('#include <guile/gh.h>\n\n')
        for func in category.getFunctions(self.platformId): 
            self.generateFuncHeader(fileHeader, func, ';\n')
        fileHeader.write('#endif\n\n')
        fileHeader.close()

    def generateRegistrations(self, category):
        """Generate code to register function."""
        ret = '    /* ' + category.displayName + ' */\n'
        stub = '    gh_new_procedure("%s", %s, 1, 0, 0);\n'
        for func in category.getFunctions(self.platformId): 
            ret += stub % (func.name, func.name)
        return ret

    def generateInitFunc(self):
        """Generate initialisation function."""
        fileInit = outputfile.OutputFile(self.rootDirectory + 'qladdin.c')
        headers = ''
        registrations = ''
        i = 0
        for category in config.Config.getInstance().getCategories(self.platformId):
            i += 1
            headers += '#include <' + category.name + '.h>\n'
            registrations += self.generateRegistrations(category)
            if i < len(config.Config.getInstance().categoryDict):
                registrations += '\n'
        fileInit.write(self.bufferInitFunc.text % (headers, registrations))
        fileInit.close()

    def generateConversions(self, paramList):
        """Generate code to convert datatypes."""
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

    def generateReturnCommand(self, returnValue):
        """Generate source code for function return command."""
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

    def generateFunction(self, fileFunc, func):
        """Generate source code for body of function."""
        conversions = self.generateConversions(func.Parameters)
        functionBody = func.generateBody(self)
        functionReturnCommand = self.generateReturnCommand(func.returnValue)
        fileFunc.write(self.bufferFunction.text % (conversions, 
            functionBody, functionReturnCommand, func.name))

    def generateFunctions(self):
        """Generate source for function implementations."""
        for category in config.Config.getInstance().getCategories(self.platformId):
            self.generateFuncHeaders(category)
            fileFunc = outputfile.OutputFile(self.rootDirectory + category.name + '.cpp')
            fileFunc.write(self.bufferIncludes.text % (category.name, category.name))
            for func in category.getFunctions(self.platformId): 
                self.generateFuncHeader(fileFunc, func, ' {')
                self.generateFunction(fileFunc, func)
            fileFunc.close()

