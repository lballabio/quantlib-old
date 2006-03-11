
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
    PROP_GET_BODY ='''if(name == "%s") return %s_;
        else '''
    CONSTRUCTOR_INIT='''%s_(%s),
        '''
    def generate(self):
        """Generate source code for ValueObjects."""
        log.Log.getInstance().logMessage('  begin generating ValueObjects ...')
        for category in config.Config.getInstance().getCategories(self.platformId):
            self.generateHeaders(category)
            self.generateFunctions(category)
        log.Log.getInstance().logMessage('  done generating ValueObjects.')

    def generateHeader(self, fileHeader, func):
        """Generate class definition source for prototype of given constructor function."""
        if func.__class__ != function.Constructor: return
        constructorDeclaration = func.generateParameterList(self.constructorDeclaration, function.DECLARATION)
        memberDeclaration = func.generateParameterList(self.memberDeclaration, function.DECLARATION)
        fileHeader.write(self.bufferClassDecl.text % (func.name, func.name, constructorDeclaration, memberDeclaration.replace(',', ';')))

    def generateHeaders(self, category):
        """Generate class source for constructor function prototypes."""
        fileHeader = outputfile.OutputFile(self.rootDirectory + 'vo_' + category.name + '.hpp')
        fileHeader.write(self.bufferIncludesDecl.text % (category.name, category.name))
        for func in category.getFunctions(self.platformId):
            self.generateHeader(fileHeader, func)
        fileHeader.write('} }\n\n#endif\n\n')
        fileHeader.close()

    def generateFunction(self, fileFunc, func):
        """Generate source code for function."""
        if func.__class__ != function.Constructor: return
        propertyDeclaration = func.generateParameterList(self.propertyDeclaration, function.DECLARATION)
        propertyGet =''
        constructorInit = '\n' + ' '*8
        for par in func.Parameters:
            propertyGet += ValueObjects.PROP_GET_BODY % (par.name, par.name)
            constructorInit += ValueObjects.CONSTRUCTOR_INIT % (par.name, par.name)
        constructorInit = constructorInit[:-10]
        constructorParList = func.generateParameterList(self.constructorDeclaration, function.DECLARATION)
        fileFunc.write(self.bufferClassBody.text % 
            (func.name, propertyDeclaration, func.name, func.name, propertyGet, func.name, func.name, 
            constructorParList, constructorInit))

    def generateFunctions(self, category):
        """Generate source for function implementations."""
        fileFunc = outputfile.OutputFile(self.rootDirectory + 'vo_' + category.name + '.cpp')
        fileFunc.write(self.bufferIncludes.text % (category.name, category.name))
        for func in category.getFunctions(self.platformId): 
            self.generateFunction(fileFunc, func)
        fileFunc.write('} }\n\n')
        fileFunc.close()
