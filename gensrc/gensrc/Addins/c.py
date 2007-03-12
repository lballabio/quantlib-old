
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
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

"""generate source code for C addin."""

from gensrc.Addins import addin
from gensrc.Functions import function
from gensrc.Utilities import outputfile
from gensrc.Utilities import common
from gensrc.Utilities import log
from gensrc.Categories import category
from gensrc.Configuration import environment

class CAddin(addin.Addin):
    """Generate source code for C addin."""

    BUFFER_FUNCDEC = '''\
    int %(func_name)s(%(func_dec)s
        %(func_ret)sresult)%(suffix)s'''
    BUFFER_HEADER = '''\
#ifndef %(lib_name)s_%(cat_name)s_h
#define %(lib_name)s_%(cat_name)s_h

%(func_headers)s
#endif\n\n'''

    def generate(self, categoryList, enumerationList):
        """Generate source code for C addin."""

        self.categoryList_ = categoryList
        self.enumerationList_ = enumerationList

        log.Log.instance().logMessage(' begin generating C ...')
        summaryHeaders = ''
        for cat in self.categoryList_.categories(self.name):
            self.generateHeaders(cat)
            self.generateFunctions(cat)
            summaryHeaders += '#include <Addins/C/%s.h>\n' % cat.name
        buf = self.bufferHeader.text % {
            'prefix' : environment.config().prefix,
            'headers' : summaryHeaders }
        fileName = self.rootPath + environment.config().prefix + 'addin.h'
        outputfile.OutputFile(self, fileName, self.copyright, buf, False)
        log.Log.instance().logMessage(' done generating C.')

    def generateHeader(self, func, suffix):
        """Generate source for prototype of given function."""
        functionDeclaration = func.ParameterList.generate(self.functionDeclaration_)
        if functionDeclaration: functionDeclaration += ','
        return CAddin.BUFFER_FUNCDEC % {
            'func_name' : func.name(),
            'func_dec' : functionDeclaration,
            'func_ret' : self.functionReturnType_.apply(func.returnValue),
            'suffix' : suffix }

    def generateHeaders(self, cat):
        """Generate source for function prototypes."""
        bufHeader = ''
        for func in cat.functions(self.name_): 
            bufHeader += self.generateHeader(func, ';\n')
        buf = CAddin.BUFFER_HEADER % {
            'cat_name' : cat.name(),
            'func_headers' : bufHeader }
        fileName = self.rootPath + cat.name() + '.h'
        fileHeader = outputfile.OutputFile(self, fileName, None, buf, False)

    def generateFunction(self, func):
        """Generate source code for function."""
        return self.bufferFunction_.text() % {
            'libraryConversions' : func.ParameterList.generate(self.libraryConversions_),
            'referenceConversions' : func.ParameterList.generate(self.referenceConversions_),
            'enumConversions' : func.ParameterList.generate(self.enumConversions_),
            'body' : func.generateBody(self),
            'returnCommand' : self.returnConversion_.apply(func.returnValue),
            'name' : func.name() }

    def generateFunctions(self, cat):
        """Generate source for function implementations."""
        codeBuffer = ''
        for func in cat.functions(self.name_): 
            codeBuffer += self.generateHeader(func, ' {')
            codeBuffer += self.generateFunction(func)
        buf = self.bufferIncludes_.text() % {
            'includes' : cat.includeList(),
            'name' : cat.name,
            'prefix' : environment.config().prefix,
            'libRoot' : environment.config().libRootDirectory,
            'code' : codeBuffer }
        fileName = self.rootPath_ + cat.name() + '.cpp'
        outputfile.OutputFile(self, fileName, None, buf, False)

