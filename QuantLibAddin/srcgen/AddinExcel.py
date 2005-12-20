
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

'addins'

import Addin
import Config
import OutputFile
import common
import utils

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

class AddinExcel(Addin.Addin):
    'generate source code for Excel addin'

    def generate(self):
        'generate source code for Excel addin'
        utils.logMessage('  begin generating %s...' % self.name)
        self.generateFuncRegisters()
        self.generateFuncDefs()
        utils.logMessage('  done generating %s.' % self.name)

    def generateFuncDefs(self):
        'generate source code for function bodies'
        for category in Config.Config.getInstance().getCategories(self.platformId):
            if category.headerOnly:
                continue
            fileFunc = OutputFile.OutputFile(self.rootDirectory + category.name + '.cpp')
            fileFunc.write(self.bufferIncludes.text % category.name)
            for function in category.getFunctions(self.platformId): 
                if function.constructor:
                    self.generateConstructor(fileFunc, function)
                else:
                    self.generateMember(fileFunc, function)
            fileFunc.close()

    def generateConstructor(self, fileFunc, function):
        'generate source code for body of constructor function'
        functionDeclaration = self.generateCode(self.functionDeclaration, 
            function.Parameters)
        libraryCall = self.generateCode(self.libraryCall, function.Parameters)
        conversions = self.generateConversions(function.Parameters)
        fileFunc.write(self.bufferConstructor.text % (function.name, functionDeclaration, 
            conversions, function.libraryFunction, libraryCall, function.name))

    def generateMember(self, fileFunc, function):
        'generate source code for body of member function'
        functionDeclaration = self.generateCode(self.functionDeclaration, 
            function.Parameters)
        functionReturnType = self.functionReturnType.apply(function.returnValue)
        functionReturnCommand = self.getReturnCommand(function.returnValue)
        libraryCall = self.generateCode(self.libraryCall, 
            function.Parameters, True, True)
        libraryReturnType = self.libraryReturnType.apply(function.returnValue)
        libraryFunctionName = function.getLibFuncName()
        conversions = self.generateConversions(function.Parameters)
        fileFunc.write(self.bufferMember.text %
            (functionReturnType, function.name, functionDeclaration, conversions, 
            function.libraryClass(), function.libraryClass(), libraryReturnType, 
            libraryFunctionName, libraryCall, functionReturnCommand, function.name))

    def getReturnCommand(self, returnValue):
        if returnValue.tensorRank == common.SCALAR:
            if returnValue.type == common.STRING:
                return RET_STRING
            elif returnValue.type == common.ANY:
                pass
            else:
                return '        return &returnValue;'
        return RET_XLOPER % (returnValue.tensorRank)

    def formatLine(self, text, comment, lastParameter = False):
        'format a line of text for the function register code'
        if len(text) >= MAXLEN:
            raise ValueError, MAXLENERR + text
        if lastParameter:
            suffix = ');'
        else:
            suffix = ','
        str1 = REGLINE % (len(text), text, suffix)
        return '%-45s// %s\n' % (str1, comment)

    def generateParamString(self, function):
        'generate string to register function parameters'
        paramStr = self.xlRegisterReturn.apply(function.returnValue)
        if function.constructor:
            paramStr += 'C'
        for param in function.Parameters:
            paramStr += self.xlRegisterParam.apply(param)
        paramStr += '#'
        return paramStr

    def generateFuncRegister(self, fileHeader, function):
        'generate call to xlfRegister for given function'
        # We call xlfRegister with NUMDESC parameters to describe the function
        # +1 additional parm to describe each parm in function being registered.
        numRegisterParams = NUMDESC + function.ParameterCount
        paramStr = self.generateParamString(function)
        paramList = ''
        i = 0
        for param in function.Parameters:
            i += 1
            paramList += self.xlListParams.apply(param)
            if i < function.ParameterCount:
                paramList += ','
        if function.constructor:    # extra parameter for object handle
            paramList = "handle," + paramList
            numRegisterParams += 1
        if numRegisterParams > MAXPARAM:
            raise ValueError, MAXPARAMERR
        fileHeader.write('        Excel(xlfRegister, 0, %d, &xDll,\n' % numRegisterParams)
        fileHeader.write(self.formatLine(function.name, 'function code name'))
        fileHeader.write(self.formatLine(paramStr, 'parameter codes'))
        fileHeader.write(self.formatLine(function.name, 'function display name'))
        fileHeader.write(self.formatLine(paramList, 'comma-delimited list of parameters'))    
        fileHeader.write(self.formatLine('1', 'function type (0 = hidden function, 1 = worksheet function, 2 = command macro)'))
        fileHeader.write(self.formatLine(function.functionCategory, 'function category'))
        fileHeader.write(self.formatLine('', 'shortcut text (command macros only)'))
        fileHeader.write(self.formatLine('', 'path to help file'))
        if function.Parameters:
            fileHeader.write(self.formatLine(function.description, 'function description'))
            i = 0
            if function.constructor:
                fileHeader.write(self.formatLine('handle of new object', 'description param 0'))
                i += 1
            j = 1
            lastParameter = False
            for param in function.Parameters:
                desc = param.description
                if j >= function.ParameterCount:                
                    lastParameter = True
                    # append 2 spaces to description of last parameter to work around bug 
                    # in Excel which causes description to be corrupted when displayed 
                    # in the Function Wizard
                    desc += '  '
                fileHeader.write(self.formatLine(desc, 'description param %d' % i, lastParameter))
                i += 1
                j += 1
        else:
            fileHeader.write(self.formatLine(function.description, 'function description', True))
        fileHeader.write('\n')

    def generateFuncRegisters(self):
        'generate source code to register functions'
        fileHeader = OutputFile.OutputFile(self.rootDirectory + ADDIN)
        fileHeader.write(self.bufferRegHeader.text)
        for category in Config.Config.getInstance().getCategories(self.platformId):
            fileHeader.write('        // %s\n\n' % category.displayName)
            for function in category.getFunctions(self.platformId): 
                self.generateFuncRegister(fileHeader, function)
        fileHeader.write(self.bufferRegFooter.text)
        fileHeader.close()

