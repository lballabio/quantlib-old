
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

"""Generate source code for Calc addin."""

import addin
import function
import config
import outputfile
import common
import log

# constants

IDLFILE = 'QLA-Calc.idl'
MAPFILE = 'funcdef.cpp'
MAPLINE = """    %s[ STRFROMANSI( "%s" ) ]
        =  STRFROMANSI( "%s" );\n"""
PARMLINE = '    %s[ STRFROMANSI( "%s" ) ].push_back( STRFROMANSI( "%s" ) );\n'
QLA_HEADER = 'qla_all.hpp'

class AddinCalc(addin.Addin):
    """Generate source code for Calc addin."""

    stringConvert = 'ouStringToStlString(%s)'

    def generate(self):
        """Generate source code for Calc addin."""
        log.Log.getInstance().logMessage('  begin generating Calc...')
        self.generateFuncMap()
        self.generateAutoHeader()
        self.generateHeaders()
        self.generateFunctions()
        self.generateIDL()
        log.Log.getInstance().logMessage('  done generating Calc.')

    def generateFuncMap(self):
        """Generate help text for function wizard."""
        fileMap = outputfile.OutputFile(self.rootDirectory + MAPFILE)
        fileMap.write(self.bufferMap.text)

        for category in config.Config.getInstance().getCategories(self.platformId):
            fileMap.write('    // %s\n\n' % category.displayName)
            for func in category.getFunctions(self.platformId): 
                fileMap.write('    // %s\n\n' % func.name)
                fileMap.write(MAPLINE % ('funcMap', func.name, func.name))
                fileMap.write(MAPLINE % ('funcDesc', func.name, func.description))
                for param in func.Parameters:
                    fileMap.write(PARMLINE % ('argName', func.name, param.name))
                    fileMap.write(PARMLINE % ('argDesc', func.name, param.description))
                fileMap.write('\n')
        fileMap.write('}\n\n')
        fileMap.close()

    def generateAutoHeader(self):
        """Generate header file that lists all other headers."""
        fileHeader = outputfile.OutputFile(self.rootDirectory + QLA_HEADER)
        fileHeader.write('#ifndef qla_calc_auto_hpp\n')
        fileHeader.write('#define qla_calc_auto_hpp\n\n')
        for category in config.Config.getInstance().getCategories(self.platformId):
            fileHeader.write('#include <Addins/Calc/%s.hpp>\n' % category.name)
        fileHeader.write('\n#endif\n\n')
        fileHeader.close()

    def generateHeader(self, fileHeader, func, declaration = True):
        """Generate implementation for given function."""
        if declaration:
            prototype = '    virtual %s SAL_CALL %s('
            suffix = ';\n'
        else:
            prototype = '%s SAL_CALL QLAddin::%s(' 
            suffix = ' {'
        functionReturnType = self.functionReturnType.apply(func.returnValue)
        fileHeader.write(prototype % (functionReturnType, func.name))
        functionDeclaration = func.generateParameterList(self.functionDeclaration,
            'const STRING &handle')
        fileHeader.write(functionDeclaration)
        fileHeader.write(') THROWDEF_RTE_IAE%s\n' % suffix)

    def generateHeaders(self):
        """Generate source for function prototypes."""
        for category in config.Config.getInstance().getCategories(self.platformId):
            fileHeader = outputfile.OutputFile(self.rootDirectory + category.name + '.hpp')
            fileHeader.write('#ifndef qla_calc_%s_hpp\n' % category.name)
            fileHeader.write('#define qla_calc_%s_hpp\n\n' % category.name)
            for func in category.getFunctions(self.platformId): 
                self.generateHeader(fileHeader, func)
            fileHeader.write('#endif\n\n')
            fileHeader.close()

    def generateReturnCommand(self, returnValue):
        """Generate code to convert datatype of return value."""
        indent = 8 * ' '
        if returnValue.type == common.VOID:
             return indent + 'return 1;'
        elif returnValue.tensorRank == common.SCALAR \
        and (returnValue.type == common.LONG or
             returnValue.type == common.DOUBLE):
             return indent + 'return returnValue;'
        else:
            functionReturnType = self.functionReturnType.apply(returnValue)
            line1 = indent + functionReturnType + ' returnValueCalc;\n'
            line2 = indent + returnValue.tensorRank + 'ToCalc(returnValueCalc, returnValue);\n'
            line3 = indent + 'return returnValueCalc;'
            return line1 + line2 + line3

    def generateFunction(self, fileFunc, func):
        """Generate source code for a given function"""
        self.generateHeader(fileFunc, func, False)
        conversions = self.generateConversions(func.Parameters)
        functionBody = func.generateBody(self)
        functionValueObject = func.generateVO(self)
        functionReturnCommand = self.generateReturnCommand(func.returnValue)
        fileFunc.write(self.bufferFunction.text % (conversions, functionBody, 
            functionValueObject, functionReturnCommand, func.name))

    def generateFunctions(self):
        """Generate source for function implementations."""
        for category in config.Config.getInstance().getCategories(self.platformId):
            fileFunc = outputfile.OutputFile(self.rootDirectory + category.name + '.cpp')
            fileFunc.write(self.bufferIncludes.text % category.includes(True))
            for func in category.getFunctions(self.platformId): 
                self.generateFunction(fileFunc, func)
            fileFunc.close()
    
    def generateIDL(self):
        """Generate the IDL file for the addin."""
        fileIDL = outputfile.OutputFile(self.rootDirectory + IDLFILE, False)
        fileIDL.write(self.bufferIdlHeader.text)
        for category in config.Config.getInstance().getCategories(self.platformId):
            fileIDL.write('                // %s\n\n' % category.name)
            for func in category.getFunctions(self.platformId): 
                parameterList = func.generateParameterList(self.ruleIDL, 
                    '[in] string handle')
                returnTypeIDL = self.returnTypeIDL.apply(func.returnValue)
                fileIDL.write(self.bufferIdlFunction.text % (returnTypeIDL, 
                    func.name, parameterList))
        fileIDL.write(self.bufferIdlFooter.text)
        fileIDL.close()
    
