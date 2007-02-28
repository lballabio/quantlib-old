
"""
 Copyright (C) 2006, 2007 Eric Ehlers

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

"""generate source code for loop functions."""

from gensrc.Addins import addin
from gensrc.Utilities import outputfile
from gensrc.Utilities import log
from gensrc.Categories import category
from gensrc.Configuration import environment
import sys

class Loop(addin.Addin):
    """Generate source code for loop functions."""
    BUF_FILE = '''\
#include <boost/bind.hpp>

namespace %(namespace)s {
%(buffer)s
}\n'''

    def generate(self, categoryList, enumerationList):
        """Generate source code for Loops."""

        log.Log.instance().logMessage(' begin generating Loops ...')
        for cat in categoryList.categories('*'):
            if cat.containsLoopFunction:
                self.generateLoops(cat)
        log.Log.instance().logMessage(' done generating Loops.')

    def generateLoop(self, func):
        """Generate loop typedefs for given function."""
        returnType = self.loopDatatype.apply(func.returnValue)
        return self.bufferBind.text % {
            'bindList' : func.behavior.bindList(self.inputTypes2),
            'bindPointer' : func.behavior.bindPointer(self.inputTypes1, returnType),
            'const' : func.behavior.const,
            'functionName' : func.name,
            'functionScope2' : func.behavior.functionScope2,
            'inputTypes3' : func.ParameterList.generate(self.inputTypes3),
            'namespaceLibrary' : environment.config().namespaceLibrary,
            'returnType' : returnType }

    def generateLoops(self, cat):
        """Generate type definitions required for source code for loop functions."""
        buf = ''
        for func in cat.getFunctions('*'): 
            if func.loopParameter:
                buf += self.generateLoop(func)
        bufFile = Loop.BUF_FILE % {
            'buffer' : buf,
            'namespace' : environment.config().namespaceObjects }
        fileName = environment.config().loopFullPath + 'loop_' + cat.name + '.hpp'
        outputfile.OutputFile(self, fileName, self.copyright, bufFile)

