
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

"""Generate source code for Excel addin."""

import addin
import config
import outputfile
import function
import common
import log
import sys

# constants

MAXLEN = 255        # max length of excel string
MAXPARAM = 30       # max #/params to an Excel function
NUMDESC = 10        # #/params to describe a function
MAXLENERR = 'string length exceeds Excel maximum of %d:\n' % MAXLEN
MAXPARAMERR = 'number of parameters to function "%s" exceeds Excel max of %d'
REGLINE = '            TempStrNoSize("\\x%02X""%s")%s'
ADDIN = 'qladdin.cpp'
RET_NUM = """\
        return &returnValue;"""
RET_STRING = """\
        static char ret[XL_MAX_STR_LEN];
        ObjHandler::stringToChar(ret, returnValue);
        return ret;"""
RET_XLOPER = """\
        static XLOPER xRet;
        ObjHandler::%sToXloper(xRet, returnValue);
        return &xRet;"""

class AddinExcel(addin.Addin):
    """Generate source code for Excel addin."""

    def generate(self):
        """Generate source code for Excel addin."""
        log.Log.getInstance().logMessage('  begin generating %s...' % self.name)
        self.generateFuncRegisters()
        self.generateFunctions()
        log.Log.getInstance().logMessage('  done generating %s.' % self.name)

    def generateFunctions(self):
        """Generate source code for all functions in all categories."""
        for category in config.Config.getInstance().getCategories(self.platformId):
            fileFunc = outputfile.OutputFile(self.rootDirectory + category.name + '.cpp')
            fileFunc.write(self.bufferIncludes.text % category.name)
            for func in category.getFunctions(self.platformId): 
                self.generateFunction(fileFunc, func)
            fileFunc.close()

    def generateFunction(self, fileFunc, func):
        """Generate source code for a given function."""
        functionReturnType = self.functionReturnType.apply(func.returnValue)
        functionDeclaration = func.generateParameterList(self.functionDeclaration, 
            'char *handle')
        conversions = self.generateConversions(func.Parameters)
        functionBody = func.generateBody(self)
        functionReturnCommand = self.generateReturnCommand(func.returnValue)
        fileFunc.write(self.bufferFunction.text %
            (functionReturnType, func.name, functionDeclaration, conversions, 
            functionBody, functionReturnCommand, func.name))

    def generateReturnCommand(self, returnValue):
        """Derive return statement for function."""
        if returnValue.tensorRank == common.SCALAR:
            if returnValue.type == common.STRING:
                return RET_STRING
            elif returnValue.type == common.ANY:    # fall through to XLOPER below
                pass
            else:                                   # long/double/boolean
                return RET_NUM
        return RET_XLOPER % (returnValue.tensorRank)

    def formatLine(self, text, comment, lastParameter = False):
        """Format a line of text for the function register code."""
        if len(text) >= MAXLEN: sys.exit(MAXLENERR + text)
        if lastParameter:
            suffix = ');'
        else:
            suffix = ','
        line = REGLINE % (len(text), text, suffix)
        return '%-45s// %s\n' % (line, comment)

    def generateParamString(self, func):
        """Generate string to register function parameters."""
        paramStr = self.xlRegisterReturn.apply(func.returnValue)
        for param in func.Parameters:
            paramStr += self.xlRegisterParam.apply(param)
        paramStr += '#'
        return paramStr

    def generateFuncRegister(self, fileHeader, func):
        """Generate call to xlfRegister for given function."""
        # We call xlfRegister with NUMDESC parameters to describe the function
        # +1 additional parm to describe each parm in function being registered.
        numRegisterParams = NUMDESC + func.ParameterCount
        paramStr = self.generateParamString(func)
        paramList = ''
        i = 0
        for param in func.Parameters:
            i += 1
            paramList += self.xlListParams.apply(param)
            if i < func.ParameterCount: paramList += ','
        if numRegisterParams > MAXPARAM: sys.exit(MAXPARAMERR % (func.name, MAXPARAM))
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
        """Generate source code to register functions."""
        fileHeader = outputfile.OutputFile(self.rootDirectory + ADDIN)
        fileHeader.write(self.bufferRegHeader.text)
        for category in config.Config.getInstance().getCategories(self.platformId):
            fileHeader.write('        // %s\n\n' % category.displayName)
            for func in category.getFunctions(self.platformId): 
                self.generateFuncRegister(fileHeader, func)
        fileHeader.write(self.bufferRegFooter.text)
        fileHeader.close()

