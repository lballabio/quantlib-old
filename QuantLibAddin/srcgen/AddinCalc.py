
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

import Addin
import Config
import OutputFile
import common
import utils

# constants

IDLFILE = 'QuantLibAddin.idl'
MAPFILE = 'funcdef.cpp'
MAPLINE = """    %s[ STRFROMANSI( "%s" ) ]
        =  STRFROMANSI( "%s" );\n"""
PARMLINE = '    %s[ STRFROMANSI( "%s" ) ].push_back( STRFROMANSI( "%s" ) );\n'
QLA_HEADER = 'qla_all.hpp'

class AddinCalc(Addin.Addin):
    'generate source code for Calc addin'

    def generate(self):
        'generate source code for Calc addin'
        utils.logMessage('  begin generating Calc...')
        self.generateFuncMap()
        self.generateAutoHeader()
        self.generateHeaders()
        self.generateFuncSources()
        self.generateIDLSource()
        utils.logMessage('  done generating Calc.')

    def generateFuncMap(self):
        'generate help text for function wizard'
        fileMap = OutputFile.OutputFile(self.rootDirectory + MAPFILE)
        fileMap.write(self.bufferMap.text)

        for category in Config.Config.getInstance().getCategories(self.platformId):
            fileMap.write('    // %s\n\n' % category.displayName)
            for function in category.getFunctions(self.platformId): 
                fileMap.write('    // %s\n\n' % function.name)
                fileMap.write(MAPLINE % ('funcMap', function.name, function.name))
                fileMap.write(MAPLINE % ('funcDesc', function.name, function.description))
                if function.constructor:
                    fileMap.write(PARMLINE % ('argName', function.name, 'handle'))
                    fileMap.write(PARMLINE % ('argDesc', function.name, 
                           'handle of newly constructed ' + function.libraryFunction + ' object'))
                for param in function.Parameters:
                    fileMap.write(PARMLINE % ('argName', function.name, param.name))
                    fileMap.write(PARMLINE % ('argDesc', function.name, param.description))
                fileMap.write('\n')
        fileMap.write('}\n\n')
        fileMap.close()

    def generateAutoHeader(self):
        'generate header file that lists all other headers'
        fileHeader = OutputFile.OutputFile(self.rootDirectory + QLA_HEADER)
        fileHeader.write('#ifndef qla_calc_auto_hpp\n')
        fileHeader.write('#define qla_calc_auto_hpp\n\n')
        for category in Config.Config.getInstance().getCategories(self.platformId):
            fileHeader.write('#include <Addins/Calc/%s.hpp>\n' % category.name)
        fileHeader.write('\n#endif\n\n')
        fileHeader.close()

    def generateHeader(self, fileHeader, function, declaration = True):
        'generate implementation for given function'
        if declaration:
            prototype = '    virtual %s SAL_CALL %s('
            suffix = ';\n'
        else:
            prototype = '%s SAL_CALL QLAddin::%s(' 
            suffix = ' {'
        functionReturnType = self.functionReturnType.apply(function.returnValue)
        fileHeader.write(prototype % (functionReturnType, function.name))
        if function.constructor:
            fileHeader.write('\n        const STRING &handle,')
        functionDeclaration = self.generateCode(self.functionDeclaration, 
            function.Parameters)
        fileHeader.write(functionDeclaration)
        fileHeader.write(') THROWDEF_RTE_IAE%s\n' % suffix)

    def generateHeaders(self):
        'generate source for function prototypes'
        for category in Config.Config.getInstance().getCategories(self.platformId):
            fileHeader = OutputFile.OutputFile(self.rootDirectory + category.name + '.hpp')
            fileHeader.write('#ifndef qla_calc_%s_hpp\n' % category.name)
            fileHeader.write('#define qla_calc_%s_hpp\n\n' % category.name)
            for function in category.getFunctions(self.platformId): 
                self.generateHeader(fileHeader, function)
            fileHeader.write('#endif\n\n')
            fileHeader.close()

    def getReturnCommand(self, returnValue):
        'generate code to convert datatype of return value'
        indent = 8 * ' '
        if returnValue.tensorRank == common.SCALAR \
        and (returnValue.type == common.LONG or \
             returnValue.type == common.DOUBLE):
             return indent + 'return returnValue;'
        else:
            functionReturnType = self.functionReturnType.apply(returnValue)
            line1 = indent + functionReturnType + ' returnValueCalc;\n'
            line2 = indent + returnValue.tensorRank + 'ToCalc(returnValueCalc, returnValue);\n'
            line3 = indent + 'return returnValueCalc;'
            return line1 + line2 + line3

    def generateMember(self, fileFunc, function):
        'generate source for given function'
        self.generateHeader(fileFunc, function, False)
        conversions = self.generateConversions(function.Parameters)
        libraryReturnType = self.libraryReturnType.apply(function.returnValue)
        libraryFunctionName = function.getLibFuncName()
        libraryCall = self.generateCode(self.libraryCall, 
            function.Parameters, True, True)
        functionReturnCommand = self.getReturnCommand(function.returnValue)
        fileFunc.write(self.bufferMember.text % (conversions, function.libraryClass(), 
            function.libraryClass(), libraryReturnType, libraryFunctionName, 
            libraryCall, functionReturnCommand, function.name))

    def generateConstructor(self, fileFunc, function):
        self.generateHeader(fileFunc, function, False)
        libraryCall = self.generateCode(self.libraryCall, function.Parameters)
        conversions = self.generateConversions(function.Parameters)
        fileFunc.write(self.bufferConstructor.text % (conversions, 
            function.libraryFunction, libraryCall, function.name))

    def generateFuncSources(self):
        'generate source for function implementations'
        for category in Config.Config.getInstance().getCategories(self.platformId):
            if category.headerOnly: continue
            fileFunc = OutputFile.OutputFile(self.rootDirectory + category.name + '.cpp')
            fileFunc.write(self.bufferIncludes.text % category.name)
            for function in category.getFunctions(self.platformId): 
                if function.constructor:
                    self.generateConstructor(fileFunc, function)
                else:
                    self.generateMember(fileFunc, function)
            fileFunc.close()
    
    def generateIDLSource(self):
        'generate the IDL file for the addin'
        fileIDL = OutputFile.OutputFile(self.rootDirectory + IDLFILE, False)
        fileIDL.write(self.bufferIdlHeader.text)
        for category in Config.Config.getInstance().getCategories(self.platformId):
            fileIDL.write('                // %s\n\n' % category.name)
            for function in category.getFunctions(self.platformId): 
                paramList = self.generateCode(self.ruleIDL, function.Parameters)
                if function.constructor:
                    handle = '\n' + 24 * ' ' + '[in] string handle'
                    if function.Parameters:
                        handle += ','
                else:
                    handle = ''
                returnTypeIDL = self.returnTypeIDL.apply(function.returnValue)
                fileIDL.write(self.bufferIdlFunction.text % (returnTypeIDL, 
                    function.name, handle, paramList))
        fileIDL.write(self.bufferIdlFooter.text)
        fileIDL.close()
    
