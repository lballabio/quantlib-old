
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

"""generate source code for Excel addin."""

import addin
import config
import outputfile
import function
import common
import log

# constants

MAXLEN = 255        # max length of excel string
MAXPARAM = 30       # max #/params to an Excel function
NUMDESC = 10        # #/params to describe a function
MAXLENERR = 'string length exceeds Excel maximum of %d:\n' % MAXLEN
MAXPARAMERR = 'number of function parameters exceeds max of %d' % MAXPARAM
REGLINE = '            TempStrNoSize("\\x%02X""%s")%s'
ADDIN = 'qladdin.cpp'
RET_STRING = """        static char ret[XL_MAX_STR_LEN];
        ObjHandler::stringToChar(ret, returnValue);
        return ret;"""
RET_XLOPER = """        static XLOPER xRet;
        ObjHandler::%sToXloper(xRet, returnValue);
        return &xRet;"""

class AddinExcel(addin.Addin):
    """generate source code for Excel addin."""

    def generate(self):
        """generate source code for Excel addin."""
        log.Log.getInstance().logMessage('  begin generating %s...' % self.name)
        self.generateFuncRegisters()
        self.generateFuncDefs()
        log.Log.getInstance().logMessage('  done generating %s.' % self.name)

    def generateFuncDefs(self):
        """generate source code for function bodies."""
        for category in config.Config.getInstance().getCategories(self.platformId):
            fileFunc = outputfile.OutputFile(self.rootDirectory + category.name + '.cpp')
            fileFunc.write(self.bufferIncludes.text % category.name)
            for func in category.getFunctions(self.platformId): 
                if isinstance(func, function.Constructor):
                    self.generateConstructor(fileFunc, func)
                elif isinstance(func, function.Member):
                    self.generateMember(fileFunc, func)
                else:
                    self.generateProcedure(fileFunc, func)
            fileFunc.close()

    def generateConstructor(self, fileFunc, func):
        """generate source code for body of constructor function."""
        functionDeclaration = self.generateCode(self.functionDeclaration, 
            func.Parameters)
        libraryCall = self.generateCode(self.libraryCall, func.Parameters)
        conversions = self.generateConversions(func.Parameters)
        fileFunc.write(self.bufferConstructor.text % (func.name, functionDeclaration, 
            conversions, func.libraryFunction, libraryCall, func.name))

    def generateMember(self, fileFunc, func):
        """generate source code for body of member function."""
        functionDeclaration = self.generateCode(self.functionDeclaration, 
            func.Parameters)
        functionReturnType = self.functionReturnType.apply(func.returnValue)
        functionReturnCommand = self.getReturnCommand(func.returnValue)
        libraryCall = self.generateCode(self.libraryCall, 
            func.Parameters, True, True)
        libraryReturnType = self.libraryReturnType.apply(func.returnValue)
        conversions = self.generateConversions(func.Parameters)
        fileFunc.write(self.bufferMember.text %
            (functionReturnType, func.name, functionDeclaration, conversions, 
            func.libraryClass, func.libraryClass, libraryReturnType, 
            func.accessLibFunc, libraryCall, functionReturnCommand, func.name))

    def generateProcedure(self, fileFunc, func):
        """generate source code for body of procedural function."""
        functionDeclaration = self.generateCode(self.functionDeclaration, 
            func.Parameters)
        functionReturnType = self.functionReturnType.apply(func.returnValue)
        functionReturnCommand = self.getReturnCommand(func.returnValue)
        libraryCall = self.generateCode(self.libraryCall, func.Parameters, False, True)
        libraryReturnType = self.libraryReturnType.apply(func.returnValue)
        conversions = self.generateConversions(func.Parameters)
        fileFunc.write(self.bufferProcedure.text %
            (functionReturnType, func.name, functionDeclaration, conversions, 
            libraryReturnType, func.name, libraryCall, functionReturnCommand, func.name))

    def getReturnCommand(self, returnValue):
        """derive return statement for function."""
        if returnValue.tensorRank == common.SCALAR:
            if returnValue.type == common.STRING:
                return RET_STRING
            elif returnValue.type == common.ANY:
                pass
            else:
                return '        return &returnValue;'
        return RET_XLOPER % (returnValue.tensorRank)

    def formatLine(self, text, comment, lastParameter = False):
        """format a line of text for the function register code."""
        if len(text) >= MAXLEN:
            raise ValueError, MAXLENERR + text
        if lastParameter:
            suffix = ');'
        else:
            suffix = ','
        str1 = REGLINE % (len(text), text, suffix)
        return '%-45s// %s\n' % (str1, comment)

    def generateParamString(self, func):
        """generate string to register function parameters."""
        paramStr = self.xlRegisterReturn.apply(func.returnValue)
        if isinstance(func, function.Constructor):
            paramStr += 'C'
        for param in func.Parameters:
            paramStr += self.xlRegisterParam.apply(param)
        paramStr += '#'
        return paramStr

    def generateFuncRegister(self, fileHeader, func):
        """generate call to xlfRegister for given function."""
        # We call xlfRegister with NUMDESC parameters to describe the function
        # +1 additional parm to describe each parm in function being registered.
        numRegisterParams = NUMDESC + func.ParameterCount
        paramStr = self.generateParamString(func)
        paramList = ''
        i = 0
        for param in func.Parameters:
            i += 1
            paramList += self.xlListParams.apply(param)
            if i < func.ParameterCount:
                paramList += ','
        if isinstance(func, function.Constructor): # extra parameter for object handle
            paramList = "handle," + paramList
            numRegisterParams += 1
        if numRegisterParams > MAXPARAM:
            raise ValueError, MAXPARAMERR
        fileHeader.write('        Excel(xlfRegister, 0, %d, &xDll,\n' % numRegisterParams)
        fileHeader.write(self.formatLine(func.name, 'function code name'))
        fileHeader.write(self.formatLine(paramStr, 'parameter codes'))
        fileHeader.write(self.formatLine(func.name, 'function display name'))
        fileHeader.write(self.formatLine(paramList, 'comma-delimited list of parameters'))    
        fileHeader.write(self.formatLine('1', 'function type (0 = hidden function, 1 = worksheet function, 2 = command macro)'))
        fileHeader.write(self.formatLine(func.functionCategory, 'function category'))
        fileHeader.write(self.formatLine('', 'shortcut text (command macros only)'))
        fileHeader.write(self.formatLine('', 'path to help file'))
        if func.Parameters:
            fileHeader.write(self.formatLine(func.description, 'function description'))
            i = 0
            if isinstance(func, function.Constructor):
                fileHeader.write(self.formatLine('handle of new object', 'description param 0'))
                i += 1
            j = 1
            lastParameter = False
            for param in func.Parameters:
                desc = param.description
                if j >= func.ParameterCount:                
                    lastParameter = True
                    # append 2 spaces to description of last parameter to work around bug 
                    # in Excel which causes description to be corrupted when displayed 
                    # in the Function Wizard
                    desc += '  '
                fileHeader.write(self.formatLine(desc, 'description param %d' % i, lastParameter))
                i += 1
                j += 1
        else:
            fileHeader.write(self.formatLine(func.description, 'function description', True))
        fileHeader.write('\n')

    def generateFuncRegisters(self):
        """generate source code to register functions."""
        fileHeader = outputfile.OutputFile(self.rootDirectory + ADDIN)
        fileHeader.write(self.bufferRegHeader.text)
        for category in config.Config.getInstance().getCategories(self.platformId):
            fileHeader.write('        // %s\n\n' % category.displayName)
            for func in category.getFunctions(self.platformId): 
                self.generateFuncRegister(fileHeader, func)
        fileHeader.write(self.bufferRegFooter.text)
        fileHeader.close()

