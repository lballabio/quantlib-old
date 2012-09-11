
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2009 Roland Lichters

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

from gensrc.addins import addin
from gensrc.functions import function
from gensrc.functions import supportedplatform
from gensrc.utilities import outputfile
from gensrc.utilities import common
from gensrc.utilities import log
from gensrc.categories import category
from gensrc.configuration import environment

# constants

MAPFILE = 'funcdef.cpp'
MAPLINE = """    %s[ STRFROMANSI( "%s" ) ]
        =  STRFROMANSI( "%s" );\n"""
PARMLINE = '    %s[ STRFROMANSI( "%s" ) ].push_back( STRFROMANSI( "%s" ) );\n'
LOOP_INCLUDES = '''\
#include <%s/loop/loop_%s.hpp>
#include <loop.hpp>'''

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
        for cat in self.categoryList_.categories(self.name_, self.coreCategories_, self.addinCategories_):
            buf += '    // %s\n\n' % cat.displayName()
            for func in cat.functions(self.name_, supportedplatform.MANUAL): 
                buf += '    // %s\n\n' % func.name()
                buf += MAPLINE % ('funcMap', func.name(), func.name())
                buf += MAPLINE % ('funcDesc', func.name(), func.description())
                for param in func.parameterList().parameters():
                    buf += PARMLINE % ('argName', func.name(), param.name())
                    buf += PARMLINE % ('argDesc', func.name(), param.description())
                buf += '\n'
# replaced
#        buf2 =''
#        buf2 = self.bufferMap_.text() % { 
#            'prefix' : environment.config().prefix(),
#            'addinClassName' : 'QLAddin',
#            'buffer' : buf }
#        fileName = self.rootPath_ + MAPFILE
#        outputfile.OutputFile(self, fileName, self.copyright_, buf2, True)
# by
        self.bufferMap_.set({
                'prefix' : environment.config().prefix(),
                'addinClassName' : 'CalcAddins_impl',
                'buffer' : buf })
        fileName = self.rootPath_ + MAPFILE
        outputfile.OutputFile(self, fileName, self.copyright_, self.bufferMap_, True)
        
    def generateAutoHeader(self):
        """Generate header file that lists all other headers."""
        bufHeader = ''
        for cat in self.categoryList_.categories(self.name_, self.coreCategories_, self.addinCategories_):
            bufHeader += '#include <%s.hpp>\n' % cat.name()
# replaced
#        buf = self.bufferHeader_.text() % { 
#            'prefix' : environment.config().prefix(),
#            'buffer' : bufHeader }
#        fileName = self.rootPath_ + environment.config().libRootDirectory() + '_all.hpp'
#        outputfile.OutputFile(self, fileName, self.copyright_, buf, True)
# by
        self.bufferHeader_.set({ 
            'prefix' : environment.config().prefix(),
            'buffer' : bufHeader })
        fileName = self.rootPath_ + environment.config().libRootDirectory() + '_all.hpp'
        outputfile.OutputFile(self, fileName, self.copyright_, self.bufferHeader_, True)

    def generateHeader(self, func, declaration = True):
        """Generate implementation for given function."""
        functionReturnType = self.functionReturnType_.apply(func.returnValue())
        if declaration:
            prototype = '    %s SAL_CALL %s(' % (functionReturnType, func.name())
            suffix = ';\n\n'
        else:
            prototype = '%s SAL_CALL %s::%s('  % (functionReturnType, 'CalcAddins_impl', func.name())
            suffix = ' {'
        ret = prototype
        ret += func.parameterList().generate(self.functionDeclaration_)
        ret += ') throw(RuntimeException)%s' % suffix
        return ret

    def generateHeaders(self):
        """Generate source for function prototypes."""
        for cat in self.categoryList_.categories(self.name_, self.coreCategories_, self.addinCategories_):
            buf = ''
            for func in cat.functions(self.name_, supportedplatform.MANUAL): 
                buf += self.generateHeader(func)
# replaced
#            buf2 = self.bufferCategory_.text() % {
#                'prefix' : environment.config().prefix(),
#                'categoryName' : cat.name(),
#                'buffer' : buf }
#            fileName = self.rootPath_ + cat.name() + '.hpp'
#            outputfile.OutputFile(self, fileName, cat.copyright(), buf2, True)
# by
            self.bufferCategory_.set({
                'prefix' : environment.config().prefix(),
                'categoryName' : cat.name(),
                'buffer' : buf })
            fileName = self.rootPath_ + cat.name() + '.hpp'
            outputfile.OutputFile(self, fileName, cat.copyright(), self.bufferCategory_, True)

    def generateFunction(self, func):
        """Generate source code for a given function"""
        if func.loopParameter():
            convertReturnType = 8 * ' ' + 'return returnValue;'
        else:
            convertReturnType = self.convertReturnType_.apply(func.returnValue())
# replaced
#        return self.bufferFunction_.text() % {
#            'convertReturnType' : convertReturnType,
#            'cppConversions' : func.parameterList().generate(self.cppConversions_),
#            'enumConversions' : func.parameterList().generate(self.enumConversions_),
#            'functionBody' : func.generateBody(self),
#            'functionName' : func.name(),
#            #'functionValueObject' : func.generateVO(self),
#            'functionValueObject' : '',
#            'header' : self.generateHeader(func, False),
#            'libraryConversions' : func.parameterList().generate(self.libraryConversions_),
#            'referenceConversions' : func.parameterList().generate(self.referenceConversions_) }
# by
        return self.bufferFunction_.set({
            'convertReturnType' : convertReturnType,
            'cppConversions' : func.parameterList().generate(self.cppConversions_),
            'enumConversions' : func.parameterList().generate(self.enumConversions_),
            'functionBody' : func.generateBody(self),
            'functionDeclaration' : func.parameterList().generate(self.functionDeclaration_),
            'functionName' : func.name(),
            'functionReturnType' : self.functionReturnType_.apply(func.returnValue()),
            #'functionValueObject' : func.generateVO(self),
            'functionValueObject' : '',
            'header' : self.generateHeader(func, False),
            'libraryConversions' : func.parameterList().generate(self.libraryConversions_),
            'objectConversions' : func.parameterList().generate(self.objectConversions_),
            'referenceConversions' : func.parameterList().generate(self.referenceConversions_)
})

    def generateFunctions(self):
        """Generate source for function implementations."""
        for cat in self.categoryList_.categories(self.name_, self.coreCategories_, self.addinCategories_):
            buf = ''
            for func in cat.functions(self.name_): 
                buf += self.generateFunction(func)
            categoryIncludes = cat.includeList(LOOP_INCLUDES)
# replaced
#            buf2 = self.bufferIncludes_.text() % {
#                'categoryIncludes' : categoryIncludes,
#                'prefix' : environment.config().prefix(),
#                'libRoot' : environment.config().libRootDirectory(),
#                'buffer' : buf }
#            fileName = self.rootPath_ + cat.name() + '.cpp'
#            outputfile.OutputFile(self, fileName, cat.copyright(), buf2, True)
# by
            self.bufferIncludes_.set({
                'categoryIncludes' : categoryIncludes,
                'prefix' : environment.config().prefix(),
                'libRoot' : environment.config().libRootDirectory(),
                'buffer' : buf })
            fileName = self.rootPath_ + cat.name() + '.cpp'
            outputfile.OutputFile(self, fileName, cat.copyright(), self.bufferIncludes_, True)
    
    def generateIDL(self):
        """Generate the IDL file for the addin."""
        buf = ''
        for cat in self.categoryList_.categories(self.name_, self.coreCategories_, self.addinCategories_):
            buf += '                // %s\n\n' % cat.name()
            for func in cat.functions(self.name_, supportedplatform.MANUAL): 
                parameterList = func.parameterList().generate(self.ruleIDL_)
                returnTypeIDL = self.returnTypeIDL_.apply(func.returnValue())
# replaced
#                buf += self.bufferIdlFunction_.text() % (returnTypeIDL, 
#                    func.name(), parameterList)
# by
                buf += '                ' + returnTypeIDL + ' ' + func.name() + '(' + parameterList + ');\n\n' 
# replaced
#        buf2 = self.bufferIdlHeader_.text() % { 'buffer' : buf }
#        idlFile = environment.config().namespaceLibrary() + 'AddinCalc.idl'
#        fileName = self.rootPath_ + idlFile
#        outputfile.OutputFile(self, fileName, self.copyright_, buf2, True)
# by
        self.bufferIdlHeader_.set({ 'buffer' : buf })
        idlFile = environment.config().namespaceLibrary() + 'AddinCalc.idl'
        fileName = self.rootPath_ + idlFile
        outputfile.OutputFile(self, fileName, self.copyright_, self.bufferIdlHeader_, True)

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        super(CalcAddin, self).serialize(serializer)
        serializer.serializeAttribute(self, 'addinClassName')

    def loopName(self, param):
        """Return the name of the given parameter as required for loop code - in
        this case no conversion is performed."""
        return param.name()
