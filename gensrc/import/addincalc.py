
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

IDLFILE = 'QuantLibAddinCalc.idl'
MAPFILE = 'funcdef.cpp'
MAPLINE = """    %s[ STRFROMANSI( "%s" ) ]
        =  STRFROMANSI( "%s" );\n"""
PARMLINE = '    %s[ STRFROMANSI( "%s" ) ].push_back( STRFROMANSI( "%s" ) );\n'
QLA_HEADER = 'qla_all.hpp'

class AddinCalc(addin.Addin):
    """Generate source code for Calc addin."""

    stringConvert = 'ouStringToStlString(%s)'
    objectIdSuffix = 'Cpp'
    voSupported = True
    convertPermanentFlag = '''
        bool permanentCpp;
        calcToScalar(permanentCpp, permanent, false);'''

    def generate(self):
        """Generate source code for Calc addin."""
        log.Log.getInstance().logMessage(' begin generating Calc...')
        self.generateFuncMap()
        self.generateAutoHeader()
        self.generateHeaders()
        self.generateFunctions()
        self.generateIDL()
        log.Log.getInstance().logMessage(' done generating Calc.')

    def generateFuncMap(self):
        """Generate help text for function wizard."""
        buf = ''
        for category in config.Config.getInstance().getCategories(self.name, function.MANUAL):
            buf += '    // %s\n\n' % category.displayName
            for func in category.getFunctions(self.name, function.MANUAL): 
                buf += '    // %s\n\n' % func.name
                buf += MAPLINE % ('funcMap', func.name, func.name)
                buf += MAPLINE % ('funcDesc', func.name, func.description)
                for param in func.ParameterList.Parameters:
                    buf += PARMLINE % ('argName', func.name, param.name)
                    buf += PARMLINE % ('argDesc', func.name, param.description)
                buf += '\n'
        buf2 = self.bufferMap.text % { 'buffer' : buf }
        fileName = self.rootDirectory + MAPFILE
        outputfile.OutputFile(self, fileName, None, buf2, False)

    def generateAutoHeader(self):
        """Generate header file that lists all other headers."""
        bufHeader = ''
        for category in config.Config.getInstance().getCategories(self.name, function.MANUAL):
            bufHeader += '#include <Addins/Calc/%s.hpp>\n' % category.name
        buf = self.bufferHeader.text % { 'buffer' : bufHeader }
        fileName = self.rootDirectory + QLA_HEADER
        outputfile.OutputFile(self, fileName, None, buf, False)

    def generateHeader(self, func, declaration = True):
        """Generate implementation for given function."""
        if declaration:
            prototype = '    virtual %s SAL_CALL %s('
            suffix = ';\n\n'
        else:
            prototype = '%s SAL_CALL QLAddin::%s(' 
            suffix = ' {'
        functionReturnType = self.functionReturnType.apply(func.returnValue)
        ret = prototype % (functionReturnType, func.name)
        ret += func.ParameterList.generate(self.functionDeclaration)
        ret += ') THROWDEF_RTE_IAE%s' % suffix
        return ret

    def generateHeaders(self):
        """Generate source for function prototypes."""
        for category in config.Config.getInstance().getCategories(self.name, function.MANUAL):
            buf = ''
            for func in category.getFunctions(self.name, function.MANUAL): 
                buf += self.generateHeader(func)
            buf2 = self.bufferCategory.text % {
                'categoryName' : category.name,
                'buffer' : buf }
            fileName = self.rootDirectory + category.name + '.hpp'
            outputfile.OutputFile(self, fileName, None, buf2, False)

    def generateFunction(self, func):
        """Generate source code for a given function"""
        if func.loopParameter:
            convertReturnType = 8 * ' ' + 'return returnValue;'
        else:
            convertReturnType = self.convertReturnType.apply(func.returnValue)
        return self.bufferFunction.text % {
            'convertReturnType' : convertReturnType,
            'cppConversions' : func.ParameterList.generate(self.cppConversions),
            'enumConversions' : func.ParameterList.generate(self.enumConversions),
            'functionBody' : func.generateBody(self),
            'functionName' : func.name,
            'functionValueObject' : func.generateVO(self),
            'header' : self.generateHeader(func, False),
            'libraryConversions' : func.ParameterList.generate(self.libraryConversions),
            'referenceConversions' : func.ParameterList.generate(self.referenceConversions) }

    def generateFunctions(self):
        """Generate source for function implementations."""
        for category in config.Config.getInstance().getCategories(self.name):
            buf = ''
            for func in category.getFunctions(self.name): 
                buf += self.generateFunction(func)
            categoryIncludes = category.includeList()
            if category.containsLoopFunction:
                categoryIncludes += '#include <%s/loop_%s.hpp>' % (
                    config.Config.getInstance().loopRootDirectory,
                    category.name)
                loopIncludes = '#include <Addins/Calc/loop.hpp>'
            else:
                loopIncludes = ''
            buf2 = self.bufferIncludes.text % {
                'categoryIncludes' : categoryIncludes,
                'loopIncludes' : loopIncludes,
                'buffer' : buf }
            fileName = self.rootDirectory + category.name + '.cpp'
            outputfile.OutputFile(self, fileName, None, buf2, False)
    
    def generateIDL(self):
        """Generate the IDL file for the addin."""
        buf = ''
        for category in config.Config.getInstance().getCategories(self.name, function.MANUAL):
            buf += '                // %s\n\n' % category.name
            for func in category.getFunctions(self.name, function.MANUAL): 
                parameterList = func.ParameterList.generate(self.ruleIDL)
                returnTypeIDL = self.returnTypeIDL.apply(func.returnValue)
                buf += self.bufferIdlFunction.text % (returnTypeIDL, 
                    func.name, parameterList)
        buf2 = self.bufferIdlHeader.text % { 'buffer' : buf }
        fileName = self.rootDirectory + IDLFILE
        outputfile.OutputFile(self, fileName, None, buf2, False)

