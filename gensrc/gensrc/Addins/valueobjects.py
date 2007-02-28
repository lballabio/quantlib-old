
"""
 Copyright (C) 2007 Eric Ehlers
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

from gensrc.Addins import addin
from gensrc.Functions import function
from gensrc.Utilities import outputfile
from gensrc.Utilities import common
from gensrc.Utilities import log
from gensrc.Categories import category
from gensrc.Configuration import environment

class ValueObjects(addin.Addin):
    """Generate source code for ValueObjects."""

    def generate(self, categoryList, enumerationList):
        """Generate source code for ValueObjects."""

        self.categoryList_ = categoryList
        self.enumerationList_ = enumerationList

        log.Log.instance().logMessage(' begin generating ValueObjects ...')
        for cat in self.categoryList_.categories('*'):
            if cat.generateVOs:
                self.generateHeaders(cat)
                self.generateFunctions(cat)
        log.Log.instance().logMessage(' done generating ValueObjects.')

    def generateHeader(self, func):
        """Generate class definition source for prototype of given constructor function."""
        if not func.generateVOs: return ''
        return self.bufferClassDecl.text % {
            'functionName' : func.name,
            'constructorDeclaration' : func.ParameterList.generate(self.constructorDeclaration),
            'memberDeclaration' : func.ParameterList.generate(self.memberDeclaration) }

    def generateHeaders(self, cat):
        """Generate class source for constructor function prototypes."""
        bufHeader = ''
        for func in cat.getFunctions('*'):
            bufHeader += self.generateHeader(func)
        buf = self.bufferIncludesDecl.text % {
            'categoryName' : cat.name,
            'headers' : bufHeader,
            'libRoot' : environment.config().libRootDirectory,
            'namespaceObjects' : environment.config().namespaceObjects }
        fileName = environment.config().voFullPath + 'vo_' + cat.name + '.hpp'
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

    def generateFunctions(self, cat):
        """Generate source for function implementations."""
        bufFunc = ''
        for func in cat.getFunctions('*'): 
            bufFunc += self.generateFunction(func)
        buf = self.bufferIncludes.text % {
            'categoryName' : cat.name,
            'functions' : bufFunc,
            'libRoot' : environment.config().libRootDirectory,
            'namespaceObjects' : environment.config().namespaceObjects,
            'voDirectory' :  environment.config().voRootDirectory }
        fileName = environment.config().voFullPath + 'vo_' + cat.name + '.cpp'
        outputfile.OutputFile(self, fileName, self.copyright, buf)

