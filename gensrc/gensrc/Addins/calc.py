
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

"""Generate source code for Calc addin."""

from gensrc.Addins import addin
from gensrc.Functions import function
from gensrc.Functions import supportedplatform
from gensrc.Utilities import outputfile
from gensrc.Utilities import common
from gensrc.Utilities import log
from gensrc.Categories import category
from gensrc.Configuration import environment

# constants

MAPFILE = 'funcdef.cpp'
MAPLINE = """    %s[ STRFROMANSI( "%s" ) ]
        =  STRFROMANSI( "%s" );\n"""
PARMLINE = '    %s[ STRFROMANSI( "%s" ) ].push_back( STRFROMANSI( "%s" ) );\n'
LOOP_INCLUDES = '''\
#include <%s/loop_%s.hpp>
#include <Addins/Calc/loop.hpp>'''

class CalcAddin(addin.Addin):
    """Generate source code for Calc addin."""

    #############################################
    # class variables
    #############################################

    stringConvert = 'ouStringToStlString(%s)'
    objectIdSuffix_ = 'Cpp'
    convertPermanentFlag_ = ''

    #############################################
    # public interface
    #############################################

    def generate(self, categoryList, enumerationList):
        """Generate source code for Calc addin."""

        self.categoryList_ = categoryList
        self.enumerationList_ = enumerationList

        log.Log.instance().logMessage(' begin generating Calc...')
        self.generateFuncMap()
        self.generateAutoHeader()
        self.generateHeaders()
        self.generateFunctions()
        self.generateIDL()
        log.Log.instance().logMessage(' done generating Calc.')

    def generateFuncMap(self):
        """Generate help text for function wizard."""
        buf = ''
        for cat in self.categoryList_.categories(self.name_, supportedplatform.MANUAL):
            buf += '    // %s\n\n' % cat.displayName()
            for func in cat.functions(self.name_, supportedplatform.MANUAL): 
                buf += '    // %s\n\n' % func.name()
                buf += MAPLINE % ('funcMap', func.name(), func.name())
                buf += MAPLINE % ('funcDesc', func.name(), func.description())
                for param in func.parameterList().parameters():
                    buf += PARMLINE % ('argName', func.name(), param.name())
                    buf += PARMLINE % ('argDesc', func.name(), param.description())
                buf += '\n'
        buf2 = self.bufferMap_.text() % { 
            'prefix' : environment.config().prefix(),
            'addinClassName' : 'QLAddin',
            'buffer' : buf }
        fileName = self.rootPath_ + MAPFILE
        outputfile.OutputFile(self, fileName, None, buf2, False)

    def generateAutoHeader(self):
        """Generate header file that lists all other headers."""
        bufHeader = ''
        for cat in self.categoryList_.categories(self.name_, supportedplatform.MANUAL):
            bufHeader += '#include <Addins/Calc/%s.hpp>\n' % cat.name()
        buf = self.bufferHeader_.text() % { 
            'prefix' : environment.config().prefix(),
            'buffer' : bufHeader }
        fileName = self.rootPath_ + environment.config().libRootDirectory() + '_all.hpp'
        outputfile.OutputFile(self, fileName, None, buf, False)

    def generateHeader(self, func, declaration = True):
        """Generate implementation for given function."""
        functionReturnType = self.functionReturnType_.apply(func.returnValue())
        if declaration:
            prototype = '    virtual %s SAL_CALL %s(' % (functionReturnType, func.name())
            suffix = ';\n\n'
        else:
            prototype = '%s SAL_CALL %s::%s('  % (functionReturnType, 'QLAddin', func.name())
            suffix = ' {'
        ret = prototype
        ret += func.parameterList().generate(self.functionDeclaration_)
        ret += ') THROWDEF_RTE_IAE%s' % suffix
        return ret

    def generateHeaders(self):
        """Generate source for function prototypes."""
        for cat in self.categoryList_.categories(self.name_, supportedplatform.MANUAL):
            buf = ''
            for func in cat.functions(self.name_, supportedplatform.MANUAL): 
                buf += self.generateHeader(func)
            buf2 = self.bufferCategory_.text() % {
                'prefix' : environment.config().prefix(),
                'categoryName' : cat.name(),
                'buffer' : buf }
            fileName = self.rootPath_ + cat.name() + '.hpp'
            outputfile.OutputFile(self, fileName, None, buf2, False)

    def generateFunction(self, func):
        """Generate source code for a given function"""
        if func.loopParameter():
            convertReturnType = 8 * ' ' + 'return returnValue;'
        else:
            convertReturnType = self.convertReturnType_.apply(func.returnValue())
        return self.bufferFunction_.text() % {
            'convertReturnType' : convertReturnType,
            'cppConversions' : func.parameterList().generate(self.cppConversions_),
            'enumConversions' : func.parameterList().generate(self.enumConversions_),
            'functionBody' : func.generateBody(self),
            'functionName' : func.name(),
            #'functionValueObject' : func.generateVO(self),
            'functionValueObject' : '',
            'header' : self.generateHeader(func, False),
            'libraryConversions' : func.parameterList().generate(self.libraryConversions_),
            'referenceConversions' : func.parameterList().generate(self.referenceConversions_) }

    def generateFunctions(self):
        """Generate source for function implementations."""
        for cat in self.categoryList_.categories(self.name_):
            buf = ''
            for func in cat.functions(self.name_): 
                buf += self.generateFunction(func)
            categoryIncludes = cat.includeList(LOOP_INCLUDES)
            buf2 = self.bufferIncludes_.text() % {
                'categoryIncludes' : categoryIncludes,
                'prefix' : environment.config().prefix(),
                'libRoot' : environment.config().libRootDirectory(),
                'buffer' : buf }
            fileName = self.rootPath_ + cat.name() + '.cpp'
            outputfile.OutputFile(self, fileName, None, buf2, False)
    
    def generateIDL(self):
        """Generate the IDL file for the addin."""
        buf = ''
        for cat in self.categoryList_.categories(self.name_, supportedplatform.MANUAL):
            buf += '                // %s\n\n' % cat.name()
            for func in cat.functions(self.name_, supportedplatform.MANUAL): 
                parameterList = func.parameterList().generate(self.ruleIDL_)
                returnTypeIDL = self.returnTypeIDL_.apply(func.returnValue())
                buf += self.bufferIdlFunction_.text() % (returnTypeIDL, 
                    func.name(), parameterList)
        buf2 = self.bufferIdlHeader_.text() % { 'buffer' : buf }
        idlFile = environment.config().namespaceLibrary() + 'AddinCalc.idl'
        fileName = self.rootPath_ + idlFile
        outputfile.OutputFile(self, fileName, None, buf2, False)

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        super(CalcAddin, self).serialize(serializer)
        serializer.serializeAttribute(self, 'addinClassName')

