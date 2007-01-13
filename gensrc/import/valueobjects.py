
"""
 Copyright (C) 2005, 2006 Plamen Neykov

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

"""generate source code for ValueObjects."""

import addin
import function
import config
import outputfile
import common
import log

class ValueObjects(addin.Addin):
    """Generate source code for ValueObjects."""

    def generate(self):
        """Generate source code for ValueObjects."""
        log.Log.getInstance().logMessage(' begin generating ValueObjects ...')
        for category in config.Config.getInstance().getCategories('*'):
            if category.generateVOs:
                self.generateHeaders(category)
                self.generateFunctions(category)
        log.Log.getInstance().logMessage(' done generating ValueObjects.')

    def generateHeader(self, func):
        """Generate class definition source for prototype of given constructor function."""
        if not func.generateVOs: return ''
        return self.bufferClassDecl.text % {
            'functionName' : func.name,
            'constructorDeclaration' : func.ParameterList.generate(self.constructorDeclaration),
            'memberDeclaration' : func.ParameterList.generate(self.memberDeclaration) }

    def generateHeaders(self, category):
        """Generate class source for constructor function prototypes."""
        bufHeader = ''
        for func in category.getFunctions('*'):
            bufHeader += self.generateHeader(func)
        buf = self.bufferIncludesDecl.text % {
            'categoryName' : category.name,
            'headers' : bufHeader,
            'libRoot' : config.Config.getInstance().libRootDirectory,
            'namespaceObjects' : config.Config.getInstance().namespaceObjects }
        fileName = config.Config.getInstance().voFullPath + 'vo_' + category.name + '.hpp'
        fileHeader = outputfile.OutputFile(self, fileName, self.copyright, buf)

    def generateFunction(self, func):
        """Generate source code for function."""
        if func.generateVOs:
            return self.bufferClassBody.text % {
                'constructorInit' : func.ParameterList.generate(self.constructorInit),
                'constructorParList' : func.ParameterList.generate(self.constructorDeclaration),
                'functionName' : func.name,
                'propertyDeclaration' : func.ParameterList.generate(self.propertyDeclaration),
                'propertyGet' : func.ParameterList.generate(self.propertyGet) }
        else:
            return ''

    def generateFunctions(self, category):
        """Generate source for function implementations."""
        bufFunc = ''
        for func in category.getFunctions('*'): 
            bufFunc += self.generateFunction(func)
        buf = self.bufferIncludes.text % {
            'categoryName' : category.name,
            'functions' : bufFunc,
            'libRoot' : config.Config.getInstance().libRootDirectory,
            'namespaceObjects' : config.Config.getInstance().namespaceObjects,
            'voDirectory' :  config.Config.getInstance().voRootDirectory }
        fileName = config.Config.getInstance().voFullPath + 'vo_' + category.name + '.cpp'
        outputfile.OutputFile(self, fileName, self.copyright, buf)

