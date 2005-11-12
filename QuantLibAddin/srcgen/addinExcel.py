
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

import addin
import common
import utils
import category
import rule

# constants

ADDIN = 'qladdin.cpp'
BUF_CTOR = 'stub.excel.constructor'
BUF_INCLUDES = 'stub.excel.includes'
BUF_MEMBER = 'stub.excel.member'
BUF_REGISTER = 'stub.excel.regheader'
RET_STRING = """        static char ret[XL_MAX_STR_LEN];
        ObjHandler::stringToChar(ret, returnValue);
        return ret;"""
RET_XLOPER = """        static XLOPER xRet;
        ObjHandler::%sToXloper(xRet, returnValue);
        return &xRet;"""
MAXLEN = 255        # max length of excel string
MAXLENERR = 'string length exceeds Excel maximum of %d:\n' % MAXLEN
MAXPARAM = 30       # max #/params to an Excel function
MAXPARAMERR  = 'number of function parameters exceeds max of %d' % MAXPARAM
NUMDESC = 10        # #/params to describe a function
REGFOOT = """        Excel(xlFree, 0, 1, &xDll);\n
        return 1;
    } catch (const std::exception &e) {
        std::ostringstream err;
        err << "Error loading QuantLibAddin: " << e.what();
        Excel(xlcAlert, 0, 1, TempStrStl(err.str()));
        Excel(xlFree, 0, 1, &xDll);
        return 0;
    } catch (...) {
        Excel(xlFree, 0, 1, &xDll);
        return 0;
    }
}\n\n"""
REGLINE = '            TempStrNoSize("\\x%02X""%s")%s'
XL_REG_PARAM = 'xlRegisterParam'
XL_REG_RET = 'xlRegisterReturn'
XL_LIST_PARAMS = 'xlListParams'

class AddinExcel(addin.Addin):

    def __init__(self,
            categories):
        super(AddinExcel, self).__init__(common.CONFIG_EXCEL, categories)
        self.bufCtor = utils.loadBuffer(BUF_CTOR)
        self.bufMember = utils.loadBuffer(BUF_MEMBER)
        self.bufInclude = utils.loadBuffer(BUF_INCLUDES)

    def setRules(self, config):
        self.ruleFunctionDeclare = rule.Rule(config[common.FUNC_DEC])
        self.ruleFunctionReturnType = rule.Rule(config[common.FUNC_RET_TYPE])
        self.ruleLibraryCall = rule.Rule(config[common.LIB_CALL])
        self.ruleLibraryReturnType = rule.Rule(config[common.LIB_RET])
        self.ruleConversions = rule.Rule(config[common.CONVERSIONS])
        self.ruleXlRegisterParam = rule.Rule(config[XL_REG_PARAM])
        self.ruleXlRegisterReturn = rule.Rule(config[XL_REG_RET])
        self.ruleXlListParams = rule.Rule(config[XL_LIST_PARAMS])

    def generate(self):
        'generate source code for Excel addin'
        print self.name
        utils.logMessage('  begin generating %s...' % self.name)
        self.generateFuncRegisters()
        self.generateFuncDefs()
        utils.logMessage('  done generating %s.' % self.name)

    def generateFuncDefs(self):
        'generate source code for function bodies'
        for categoryKey in self.categories[common.KEYS]:
            category = self.categories[common.DICT][categoryKey]
            if not category.platformSupported(self.platformId):
                continue
            if category.headerOnly:
                continue
            fileName = self.rootDir + category.name + '.cpp' + common.TEMPFILE
            fileFunc = file(fileName, 'w')
            utils.printHeader(fileFunc)
            fileFunc.write(self.bufInclude % category.name)
            for functionKey in category.functions[common.KEYS]:
                function = category.functions[common.DICT][functionKey]
                if not function.platformSupported(self.platformId):
                    continue
                if function.isConstructor:
                    self.generateConstructor(fileFunc, function)
                else:
                    self.generateMember(fileFunc, function)
            fileFunc.close()
            utils.updateIfChanged(fileName)

    def generateConstructor(self, fileFunc, function):
        'generate source code for body of constructor function'
        functionDeclaration = self.generateCode(self.ruleFunctionDeclare, function.parameters)
        libraryCall = self.generateCode(self.ruleLibraryCall, function.parameters)
        conversions = self.generateConversions(function.parameters)
        fileFunc.write(self.bufCtor % (function.name, functionDeclaration, conversions, 
            function.libFunction, libraryCall, function.name))

    def generateMember(self, fileFunc, function):
        'generate source code for body of member function'
        functionDeclaration = self.generateCode(self.ruleFunctionDeclare, function.parameters)
        functionReturnType = self.ruleFunctionReturnType.apply(function.returnValue)
        functionReturnCommand = self.getReturnCommand(function.returnValue)
        libraryCall = self.generateCode(self.ruleLibraryCall, function.parameters, True, True)
        libraryReturnType = self.ruleLibraryReturnType.apply(function.returnValue)
        libraryFunctionName = utils.getLibFuncName(function)
        conversions = self.generateConversions(function.parameters)
        fileFunc.write(self.bufMember %
            (functionReturnType, function.name, functionDeclaration, conversions, 
            function.className, function.className, libraryReturnType, libraryFunctionName, 
            libraryCall, functionReturnCommand, function.name))

    def getReturnCommand(self, returnValue):
        if returnValue.tensorRank == common.SCALAR:
            if returnValue.type == common.STRING:
                return RET_STRING
            elif returnValue.type == common.ANY:
                pass
            else:
                return '        return &returnValue;'
        return RET_XLOPER % (returnValue.tensorRank + returnValue.type.capitalize())

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
        paramStr = self.ruleXlRegisterReturn.apply(function.returnValue)
        if function.isConstructor:
            paramStr += 'C'
        for param in function.parameters:
            paramStr += self.ruleXlRegisterParam.apply(param)
        if function.isConstructor:
            paramStr += '#'
        return paramStr

    def generateFuncRegister(self, fileHeader, function):
        'generate call to xlfRegister for given function'
        funcParams = function.parameters
        # We call xlfRegister with NUMDESC parameters to describe the function
        # +1 additional parm to describe each parm in function being registered.
        numRegisterParams = NUMDESC + function.parameterCount
        paramStr = self.generateParamString(function)
        paramList = ''
        i = 0
        for param in function.parameters:
            i += 1
            paramList += self.ruleXlListParams.apply(param)
            if i < function.parameterCount:
                paramList += ','
        if function.isConstructor:    # extra parameter for object handle
            paramList = "handle," + paramList
            numRegisterParams += 1
        # FIXME validation below to be moved into parse.py?
        if numRegisterParams > MAXPARAM:
            raise ValueError, MAXPARAMERR
        fileHeader.write('        Excel(xlfRegister, 0, %d, &xDll,\n' % numRegisterParams)
        fileHeader.write(self.formatLine(function.name, 'function code name'))
        fileHeader.write(self.formatLine(paramStr, 'parameter codes'))
        fileHeader.write(self.formatLine(function.name, 'function display name'))
        fileHeader.write(self.formatLine(paramList, 'comma-delimited list of parameters'))    
        fileHeader.write(self.formatLine('1', 'function type (0 = hidden function, 1 = worksheet function, 2 = command macro)'))
        fileHeader.write(self.formatLine('QuantLib', 'function category'))
        fileHeader.write(self.formatLine('', 'shortcut text (command macros only)'))
        fileHeader.write(self.formatLine('', 'path to help file'))
        if funcParams:
            fileHeader.write(self.formatLine(function.description, 'function description'))
            i = 0
            if function.isConstructor:
                fileHeader.write(self.formatLine('handle of new object', 'description param 0'))
                i += 1
            j = 1
            lastParameter = False
            for param in funcParams:
                desc = param.description
                if j >= function.parameterCount:                
                    lastParameter = True
                    # append 2 spaces to description of last parameter to work around bug in Excel
                    # which causes description to be corrupted when displayed in the Function Wizard
                    desc += '  '
                fileHeader.write(self.formatLine(desc, 'description param %d' % i, lastParameter))
                i += 1
                j += 1
        else:
            fileHeader.write(self.formatLine(function.description, 'function description', True))
        fileHeader.write('\n')

    def generateFuncRegisters(self):
        'generate source code to register functions'
        fileName = self.rootDir + ADDIN + common.TEMPFILE
        fileHeader = file(fileName, 'w')
        utils.printHeader(fileHeader)
        bufHead = utils.loadBuffer(BUF_REGISTER)
        fileHeader.write(bufHead)
        for categoryKey in self.categories[common.KEYS]:
            category = self.categories[common.DICT][categoryKey]
            if not category.platformSupported(self.platformId):
                continue
            fileHeader.write('        // %s\n\n' % category.displayName)
            for functionKey in category.functions[common.KEYS]:
                function = category.functions[common.DICT][functionKey]
                if not function.platformSupported(self.platformId):
                    continue
                self.generateFuncRegister(fileHeader, function)
        fileHeader.write(REGFOOT)
        fileHeader.close()
        utils.updateIfChanged(fileName)

