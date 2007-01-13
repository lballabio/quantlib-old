
"""
 Copyright (C) 2006 Eric Ehlers

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

import addin
import config
import outputfile
import log
import sys

class Loop(addin.Addin):
    """Generate source code for loop functions."""
    BUF_FILE = '''\
#include <boost/bind.hpp>

namespace %(namespace)s {
%(buffer)s
}\n'''

    def generate(self):
        """Generate source code for Loops."""
        log.Log.getInstance().logMessage(' begin generating Loops ...')
        for category in config.Config.getInstance().getCategories('*'):
            if category.containsLoopFunction:
                self.generateLoops(category)
        log.Log.getInstance().logMessage(' done generating Loops.')

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
            'namespaceLibrary' : config.Config.getInstance().namespaceLibrary,
            'returnType' : returnType }

    def generateLoops(self, category):
        """Generate type definitions required for source code for loop functions."""
        buf = ''
        for func in category.getFunctions('*'): 
            if func.loopParameter:
                buf += self.generateLoop(func)
        bufFile = Loop.BUF_FILE % {
            'buffer' : buf,
            'namespace' : config.Config.getInstance().namespaceObjects }
        fileName = config.Config.getInstance().loopFullPath + 'loop_' + category.name + '.hpp'
        outputfile.OutputFile(self, fileName, self.copyright, bufFile)

