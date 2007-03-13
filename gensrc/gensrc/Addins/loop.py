
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

class Loop(addin.Addin):
    """Generate source code for loop functions."""

    #############################################
    # class variables
    #############################################

    BUF_FILE = '''\
#include <boost/bind.hpp>

namespace %(namespace)s {
%(buffer)s
}\n'''

    #############################################
    # public interface
    #############################################

    def generate(self, categoryList, enumerationList):
        """Generate source code for Loops."""

        log.Log.instance().logMessage(' begin generating Loops ...')
        for cat in categoryList.categories('*'):
            if cat.containsLoopFunction():
                self.generateLoops(cat)
        log.Log.instance().logMessage(' done generating Loops.')

    def generateLoop(self, func):
        """Generate loop typedefs for given function."""
        returnType = self.loopDatatype_.apply(func.returnValue())
        return self.bufferBind_.text() % {
            'bindList' : func.behavior().bindList(self.inputTypes2_),
            'bindPointer' : func.behavior().bindPointer(self.inputTypes1_, returnType),
            'const' : func.behavior().const(),
            'functionName' : func.name(),
            'functionScope2' : func.behavior().functionScope2(),
            'inputTypes3' : func.parameterList().generate(self.inputTypes3_),
            'namespaceLibrary' : environment.config().namespaceLibrary(),
            'returnType' : returnType }

    def generateLoops(self, cat):
        """Generate type definitions required for source code for loop functions."""
        buf = ''
        for func in cat.functions('*'): 
            if func.loopParameter():
                buf += self.generateLoop(func)
        bufFile = Loop.BUF_FILE % {
            'buffer' : buf,
            'namespace' : environment.config().namespaceObjects() }
        fileName = self.rootPath_ + 'loop_' + cat.name() + '.hpp'
        outputfile.OutputFile(self, fileName, self.copyright_, bufFile)

    def inputTypes1(self):
        return self.inputTypes1_

    def inputTypes2(self):
        return self.inputTypes2_

    def inputTypes3(self):
        return self.inputTypes3_

    def loopDatatype(self):
        return self.loopDatatype_

