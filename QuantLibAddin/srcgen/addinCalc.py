
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

'calc addin'

import addin
import common
import parse
import utils
import category
import rule

# constants

BUF_CTOR = 'stub.calc.constructor'
BUF_MEMBER = 'stub.calc.member'
QLA_HEADER = 'qla_all.hpp'
IDL = 'QuantLibAddin.idl'
IDL_FOOT = 'stub.calc.idlfoot'
IDL_FUNC = 'stub.calc.idlfunc'
IDL_HEAD = 'stub.calc.idlhead'
INCLUDES = 'stub.calc.includes'
MAP = 'stub.calc.map'
MAPFILE = 'funcdef.cpp'
MAPLINE = """    %s[ STRFROMANSI( "%s" ) ]
        =  STRFROMANSI( "%s" );\n"""
PARMLINE = '    %s[ STRFROMANSI( "%s" ) ].push_back( STRFROMANSI( "%s" ) );\n'
RULE_IDL = 'ruleIDL'
RET_TYPE_IDL = 'returnTypeIDL'

class AddinCalc(addin.Addin):

    def __init__(self,
            categories):
        super(AddinCalc, self).__init__(common.CONFIG_CALC, categories)
        self.bufCtor = utils.loadBuffer(BUF_CTOR)
        self.bufMember = utils.loadBuffer(BUF_MEMBER)

    def setRules(self, config):
        self.ruleFunctionReturnType = rule.Rule(config[common.FUNC_RET_TYPE])
        self.ruleFunctionDeclare = rule.Rule(config[common.FUNC_DEC])
        self.ruleLibraryReturnType = rule.Rule(config[common.LIB_RET])
        self.ruleLibraryCall = rule.Rule(config[common.LIB_CALL])
        self.ruleConversions = rule.Rule(config[common.CONVERSIONS])
        self.ruleIDL = rule.Rule(config[RULE_IDL])
        self.ruleReturnTypeIDL = rule.Rule(config[RET_TYPE_IDL])

    def generate(self):
        'generate source code for Excel static addin'
        utils.logMessage('  begin generating Calc...')
        self.generateFuncMap()
        self.generateAutoHeader()
        self.generateHeaders()
        self.generateFuncSources()
        self.generateIDLSource()
        utils.logMessage('  done generating Calc.')

    def generateFuncMap(self):
        'generate help text for function wizard'
        fileName = self.rootDir + MAPFILE + common.TEMPFILE
        fileMap = file(fileName, 'w')
        utils.printHeader(fileMap)
        bufCalcMap = utils.loadBuffer(MAP)
        fileMap.write(bufCalcMap)
        for categoryKey in self.categories[common.KEYS]:
            category = self.categories[common.DICT][categoryKey]
            if not category.platformSupported(self.platformId):
                continue
            fileMap.write('    // %s\n\n' % category.displayName)
            for functionKey in category.functions[common.KEYS]:
                function = category.functions[common.DICT][functionKey]
                if not function.platformSupported(self.platformId):
                    continue
                fileMap.write('    // %s\n\n' % function.name)
                fileMap.write(MAPLINE % ('funcMap', function.name, function.name))
                fileMap.write(MAPLINE % ('funcDesc', function.name, function.description))
                if function.isConstructor:
                    fileMap.write(PARMLINE % ('argName', function.name, 'handle'))
                    fileMap.write(PARMLINE % ('argDesc', function.name, 
                           'handle of newly constructed ' + function.libFunction + ' object'))
                for param in function.parameters:
                    fileMap.write(PARMLINE % ('argName', function.name, param.name))
                    fileMap.write(PARMLINE % ('argDesc', function.name, param.description))
                fileMap.write('\n')
        fileMap.write('}\n\n')
        fileMap.close()
        utils.updateIfChanged(fileName)

    def generateAutoHeader(self):
        'generate header file that lists all other headers'
        fileName = self.rootDir + QLA_HEADER + common.TEMPFILE
        fileHeader = file(fileName, 'w')
        utils.printHeader(fileHeader)
        fileHeader.write('#ifndef qla_calc_auto_hpp\n')
        fileHeader.write('#define qla_calc_auto_hpp\n\n')
        for categoryKey in self.categories[common.KEYS]:
            category = self.categories[common.DICT][categoryKey]
            if not category.platformSupported(self.platformId):
                continue
            fileHeader.write('#include <Addins/Calc/%s.hpp>\n' % categoryKey)
        fileHeader.write('\n#endif\n\n')
        fileHeader.close()
        utils.updateIfChanged(fileName)

    def generateHeader(self, fileHeader, function, declararion = True):
        'generate implementation for given function'
        if declararion:
            prototype = '    virtual %s SAL_CALL %s('
            suffix = ';\n'
        else:
            prototype = '%s SAL_CALL QLAddin::%s(' 
            suffix = ' {'
        functionReturnType = self.ruleFunctionReturnType.apply(function.returnValue)
        fileHeader.write(prototype % (functionReturnType, function.name))
        if function.isConstructor:
            fileHeader.write('\n        const STRING &handle,')
        functionDeclaration = self.generateCode(self.ruleFunctionDeclare, function.parameters)
        fileHeader.write(functionDeclaration)
        fileHeader.write(') THROWDEF_RTE_IAE%s\n' % suffix)

    def generateHeaders(self):
        'generate source for function prototypes'
        for categoryKey in self.categories[common.KEYS]:
            category = self.categories[common.DICT][categoryKey]
            if not category.platformSupported(self.platformId):
                continue
            fileName = self.rootDir + category.name + '.hpp' + common.TEMPFILE
            fileHeader = file(fileName, 'w')
            utils.printHeader(fileHeader)
            fileHeader.write('#ifndef qla_calc_%s_hpp\n' % category.name)
            fileHeader.write('#define qla_calc_%s_hpp\n\n' % category.name)
            for functionKey in category.functions[common.KEYS]:
                function = category.functions[common.DICT][functionKey]
                if not function.platformSupported(self.platformId):
                    continue
                self.generateHeader(fileHeader, function)
            fileHeader.write('#endif\n\n')
            fileHeader.close()
            utils.updateIfChanged(fileName)

    def getReturnCommand(self, returnValue):
        'generate code to convert datatype of return value'
        indent = 8 * ' '
        if returnValue.tensorRank == common.SCALAR \
        and (returnValue.type == common.LONG or \
             returnValue.type == common.DOUBLE):
             return indent + 'return returnValue;'
        else:
            functionReturnType = self.ruleFunctionReturnType.apply(returnValue)
            line1 = indent + functionReturnType + ' returnValueCalc;\n'
            line2 = indent + returnValue.tensorRank + 'ToCalc(returnValueCalc, returnValue);\n'
            line3 = indent + 'return returnValueCalc;'
            return line1 + line2 + line3

    def generateMember(self, fileFunc, function):
        'generate source for given function'
        self.generateHeader(fileFunc, function, False)
        conversions = self.generateConversions(function.parameters)
        libraryReturnType = self.ruleLibraryReturnType.apply(function.returnValue)
        libraryFunctionName = utils.getLibFuncName(function)
        libraryCall = self.generateCode(self.ruleLibraryCall, function.parameters, True, True)
        functionReturnCommand = self.getReturnCommand(function.returnValue)
        fileFunc.write(self.bufMember % (conversions, function.className, function.className, 
            libraryReturnType, libraryFunctionName, libraryCall, functionReturnCommand, function.name))

    def generateConstructor(self, fileFunc, function):
        self.generateHeader(fileFunc, function, False)
        libraryCall = self.generateCode(self.ruleLibraryCall, function.parameters)
        conversions = self.generateConversions(function.parameters)
        fileFunc.write(self.bufCtor % (conversions, function.libFunction, 
            libraryCall, function.name))

    def generateFuncSources(self):
        'generate source for function implementations'
        bufInclude = utils.loadBuffer(INCLUDES)
        for categoryKey in self.categories[common.KEYS]:
            category = self.categories[common.DICT][categoryKey]
            if not category.platformSupported(self.platformId):
                continue
            if category.headerOnly:
                continue
            fileName = self.rootDir + category.name + '.cpp' + common.TEMPFILE
            fileFunc = file(fileName, 'w')
            utils.printHeader(fileFunc)
            bufIncludeFull = bufInclude % category.name
            fileFunc.write(bufIncludeFull)
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
    
    def generateIDLSource(self):
        'generate the IDL file for the addin'
        fileName = self.rootDir + IDL + common.TEMPFILE
        fileIDL = file(fileName, 'w')
        utils.printTimeStamp(fileIDL)
        bufIDLHead = utils.loadBuffer(IDL_HEAD)
        fileIDL.write(bufIDLHead)
        bufIDLFunc = utils.loadBuffer(IDL_FUNC)
        for categoryKey in self.categories[common.KEYS]:
            category = self.categories[common.DICT][categoryKey]
            if not category.platformSupported(self.platformId):
                continue
            fileIDL.write('                // %s\n\n' % category.name)
            for functionKey in category.functions[common.KEYS]:
                function = category.functions[common.DICT][functionKey]
                if not function.platformSupported(self.platformId):
                    continue
                paramList = self.generateCode(self.ruleIDL, function.parameters)
                if function.isConstructor:
                    handle = '\n' + 24 * ' ' + '[in] string handle'
                    if function.parameters:
                        handle += ','
                else:
                    handle = ''
                returnTypeIDL = self.ruleReturnTypeIDL.apply(function.returnValue)
                fileIDL.write(bufIDLFunc % (returnTypeIDL, 
                    function.name, handle, paramList))
        bufIDLFoot = utils.loadBuffer(IDL_FOOT)
        fileIDL.write(bufIDLFoot)
        fileIDL.close()
        utils.updateIfChanged(fileName)
    
