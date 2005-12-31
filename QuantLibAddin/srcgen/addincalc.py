
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

"""Generate source code for Calc addin."""

import addin
import function
import config
import outputfile
import common
import log

# constants

IDLFILE = 'QuantLibAddin.idl'
MAPFILE = 'funcdef.cpp'
MAPLINE = """    %s[ STRFROMANSI( "%s" ) ]
        =  STRFROMANSI( "%s" );\n"""
PARMLINE = '    %s[ STRFROMANSI( "%s" ) ].push_back( STRFROMANSI( "%s" ) );\n'
QLA_HEADER = 'qla_all.hpp'

class AddinCalc(addin.Addin):
    """generate source code for Calc addin."""

    def generate(self):
        """generate source code for Calc addin."""
        log.Log.getInstance().logMessage('  begin generating Calc...')
        self.generateFuncMap()
        self.generateAutoHeader()
        self.generateHeaders()
        self.generateFuncSources()
        self.generateIDLSource()
        log.Log.getInstance().logMessage('  done generating Calc.')

    def generateFuncMap(self):
        """generate help text for function wizard."""
        fileMap = outputfile.OutputFile(self.rootDirectory + MAPFILE)
        fileMap.write(self.bufferMap.text)

        for category in config.Config.getInstance().getCategories(self.platformId):
            fileMap.write('    // %s\n\n' % category.displayName)
            for func in category.getFunctions(self.platformId): 
                fileMap.write('    // %s\n\n' % func.name)
                fileMap.write(MAPLINE % ('funcMap', func.name, func.name))
                fileMap.write(MAPLINE % ('funcDesc', func.name, func.description))
                if isinstance(func, function.Constructor):
                    fileMap.write(PARMLINE % ('argName', func.name, 'handle'))
                    fileMap.write(PARMLINE % ('argDesc', func.name, 
                           'handle of newly constructed ' + func.libraryFunction + ' object'))
                for param in func.Parameters:
                    fileMap.write(PARMLINE % ('argName', func.name, param.name))
                    fileMap.write(PARMLINE % ('argDesc', func.name, param.description))
                fileMap.write('\n')
        fileMap.write('}\n\n')
        fileMap.close()

    def generateAutoHeader(self):
        """generate header file that lists all other headers."""
        fileHeader = outputfile.OutputFile(self.rootDirectory + QLA_HEADER)
        fileHeader.write('#ifndef qla_calc_auto_hpp\n')
        fileHeader.write('#define qla_calc_auto_hpp\n\n')
        for category in config.Config.getInstance().getCategories(self.platformId):
            fileHeader.write('#include <Addins/Calc/%s.hpp>\n' % category.name)
        fileHeader.write('\n#endif\n\n')
        fileHeader.close()

    def generateHeader(self, fileHeader, func, declaration = True):
        """generate implementation for given function."""
        if declaration:
            prototype = '    virtual %s SAL_CALL %s('
            suffix = ';\n'
        else:
            prototype = '%s SAL_CALL QLAddin::%s(' 
            suffix = ' {'
        functionReturnType = self.functionReturnType.apply(func.returnValue)
        fileHeader.write(prototype % (functionReturnType, func.name))
        if isinstance(func, function.Constructor):
            fileHeader.write('\n        const STRING &handle,')
        functionDeclaration = self.generateCode(self.functionDeclaration, 
            func.Parameters)
        fileHeader.write(functionDeclaration)
        fileHeader.write(') THROWDEF_RTE_IAE%s\n' % suffix)

    def generateHeaders(self):
        """generate source for function prototypes."""
        for category in config.Config.getInstance().getCategories(self.platformId):
            fileHeader = outputfile.OutputFile(self.rootDirectory + category.name + '.hpp')
            fileHeader.write('#ifndef qla_calc_%s_hpp\n' % category.name)
            fileHeader.write('#define qla_calc_%s_hpp\n\n' % category.name)
            for func in category.getFunctions(self.platformId): 
                self.generateHeader(fileHeader, func)
            fileHeader.write('#endif\n\n')
            fileHeader.close()

    def getReturnCommand(self, returnValue):
        """generate code to convert datatype of return value."""
        indent = 8 * ' '
        if returnValue.tensorRank == common.SCALAR \
        and (returnValue.type == common.LONG or
             returnValue.type == common.DOUBLE):
             return indent + 'return returnValue;'
        else:
            functionReturnType = self.functionReturnType.apply(returnValue)
            line1 = indent + functionReturnType + ' returnValueCalc;\n'
            line2 = indent + returnValue.tensorRank + 'ToCalc(returnValueCalc, returnValue);\n'
            line3 = indent + 'return returnValueCalc;'
            return line1 + line2 + line3

    def generateConstructor(self, fileFunc, func):
        """generate source for constructor."""
        self.generateHeader(fileFunc, func, False)
        libraryCall = self.generateCode(self.libraryCall, func.Parameters)
        conversions = self.generateConversions(func.Parameters)
        fileFunc.write(self.bufferConstructor.text % (conversions, 
            func.libraryFunction, libraryCall, func.name))

    def generateMember(self, fileFunc, func):
        """generate source for member function."""
        self.generateHeader(fileFunc, func, False)
        conversions = self.generateConversions(func.Parameters)
        libraryReturnType = self.libraryReturnType.apply(func.returnValue)
        libraryCall = self.generateCode(self.libraryCall, 
            func.Parameters, True, True)
        functionReturnCommand = self.getReturnCommand(func.returnValue)
        fileFunc.write(self.bufferMember.text % (conversions, func.libraryClass, 
            func.libraryClass, libraryReturnType, func.accessLibFunc, 
            libraryCall, functionReturnCommand, func.name))

    def generateProcedure(self, fileFunc, func):
        """generate source for procedural function."""
        self.generateHeader(fileFunc, func, False)
        conversions = self.generateConversions(func.Parameters)
        libraryReturnType = self.libraryReturnType.apply(func.returnValue)
        libraryCall = self.generateCode(self.libraryCall, func.Parameters, False, True)
        functionReturnCommand = self.getReturnCommand(func.returnValue)
        fileFunc.write(self.bufferProcedure.text % (conversions, libraryReturnType, 
            func.name, libraryCall, functionReturnCommand, func.name))

    def generateFuncSources(self):
        """generate source for function implementations."""
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
    
    def generateIDLSource(self):
        """generate the IDL file for the addin."""
        fileIDL = outputfile.OutputFile(self.rootDirectory + IDLFILE, False)
        fileIDL.write(self.bufferIdlHeader.text)
        for category in config.Config.getInstance().getCategories(self.platformId):
            fileIDL.write('                // %s\n\n' % category.name)
            for func in category.getFunctions(self.platformId): 
                paramList = self.generateCode(self.ruleIDL, func.Parameters)
                if isinstance(func, function.Constructor):
                    handle = '\n' + 24 * ' ' + '[in] string handle'
                    if func.Parameters:
                        handle += ','
                else:
                    handle = ''
                returnTypeIDL = self.returnTypeIDL.apply(func.returnValue)
                fileIDL.write(self.bufferIdlFunction.text % (returnTypeIDL, 
                    func.name, handle, paramList))
        fileIDL.write(self.bufferIdlFooter.text)
        fileIDL.close()
    
