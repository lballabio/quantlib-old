
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

"""generate source code for C addin."""

import addin
import function
import config
import outputfile
import common
import log

class AddinC(addin.Addin):
    """Generate source code for C addin."""

    BUFFER_FUNCDEC = '''\
    int %(func_name)s(%(func_dec)s
        %(func_ret)sresult)%(suffix)s'''
    BUFFER_HEADER = '''\
#ifndef qla_%(cat_name)s_h
#define qla_%(cat_name)s_h

%(func_headers)s
#endif\n\n'''

    def generate(self):
        """Generate source code for C addin."""
        log.Log.getInstance().logMessage(' begin generating C ...')
        summaryHeaders = ''
        for category in config.Config.getInstance().getCategories(self.name):
            self.generateHeaders(category)
            self.generateFunctions(category)
            summaryHeaders += '#include <Addins/C/%s.h>\n' % category.name
        buf = self.bufferHeader.text % summaryHeaders
        fileName = self.rootDirectory + 'qladdin.h'
        outputfile.OutputFile(self, fileName, self.copyright, buf, False)
        log.Log.getInstance().logMessage(' done generating C.')

    def generateHeader(self, func, suffix):
        """Generate source for prototype of given function."""
        functionDeclaration = func.ParameterList.generate(self.functionDeclaration)
        if functionDeclaration: functionDeclaration += ','
        return AddinC.BUFFER_FUNCDEC % {
            'func_name' : func.name,
            'func_dec' : functionDeclaration,
            'func_ret' : self.functionReturnType.apply(func.returnValue),
            'suffix' : suffix }

    def generateHeaders(self, category):
        """Generate source for function prototypes."""
        bufHeader = ''
        for func in category.getFunctions(self.name): 
            bufHeader += self.generateHeader(func, ';\n')
        buf = AddinC.BUFFER_HEADER % {
            'cat_name' : category.name,
            'func_headers' : bufHeader }
        fileName = self.rootDirectory + category.name + '.h'
        fileHeader = outputfile.OutputFile(self, fileName, None, buf, False)

    def generateFunction(self, func):
        """Generate source code for function."""
        return self.bufferFunction.text % {
            'libraryConversions' : func.ParameterList.generate(self.libraryConversions),
            'referenceConversions' : func.ParameterList.generate(self.referenceConversions),
            'enumConversions' : func.ParameterList.generate(self.enumConversions),
            'body' : func.generateBody(self),
            'returnCommand' : self.returnConversion.apply(func.returnValue),
            'name' : func.name }

    def generateFunctions(self, category):
        """Generate source for function implementations."""
        codeBuffer = ''
        for func in category.getFunctions(self.name): 
            codeBuffer += self.generateHeader(func, ' {')
            codeBuffer += self.generateFunction(func)
        buf = self.bufferIncludes.text % {
            'includes' : category.includeList(),
            'name' : category.name,
            'code' : codeBuffer }
        fileName = self.rootDirectory + category.name + '.cpp'
        outputfile.OutputFile(self, fileName, None, buf, False)

