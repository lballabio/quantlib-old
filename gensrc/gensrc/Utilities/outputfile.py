
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

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

"""represent a file which gets overwritten only when its contents change."""

from gensrc.Configuration import environment
from gensrc.Utilities import log
import os
import sys
import filecmp
import re

# constants

HEADER = """\
// this file generated automatically by %s
// editing this file manually is not recommended\n\n"""
UPDATE_MSG = '  file %-65s - %10s'

class OutputFile(object):
    """represent a file which gets overwritten only when its contents change."""
    
    trimWhitespace = re.compile(r'^ *', re.M)

    def __init__(self, addin, fileName, copyright, buffer, printHeader = True):
        """open file and write header."""
        self.addin = addin
        self.fileName = fileName
        self.fileNameTemp = fileName + '.temp'
        self.outFile = file(self.fileNameTemp, 'w')
        if copyright:
            self.printCopyright(copyright)
        if printHeader:
            self.printHeader()
        self.outFile.write(buffer)
        self.close()

    def printCopyright(self, copyright):
        copyright = OutputFile.trimWhitespace.sub(' ', copyright)
        self.outFile.write(environment.config().copyrightBuffer % 
            { 'copyright' : copyright } )

    def printHeader(self):
        self.outFile.write(HEADER % os.path.basename(sys.argv[0]))

    def close(self):
        """close temp file and overwrite original if they are different."""
        self.outFile.close()
        if os.path.exists(self.fileName):
            if filecmp.cmp(self.fileName, self.fileNameTemp):
                os.unlink(self.fileNameTemp)
                log.Log.getInstance().logMessage(UPDATE_MSG % (self.fileName, 'unchanged'))
                self.addin.unchanged += 1
            else:
                os.unlink(self.fileName)
                os.rename(self.fileNameTemp, self.fileName)
                log.Log.getInstance().logMessage(UPDATE_MSG % (self.fileName, 'updated'))
                self.addin.updated += 1
        else:
            os.rename(self.fileNameTemp, self.fileName)
            log.Log.getInstance().logMessage(UPDATE_MSG % (self.fileName, 'created'))
            self.addin.created += 1

