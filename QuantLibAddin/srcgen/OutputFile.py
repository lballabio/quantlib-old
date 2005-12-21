
"""
 Copyright (C) 2005 Eric Ehlers

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

'output file'

import Config
import Log
import os
import sys
import filecmp

# constants

HEADER = """// this file generated automatically by %s
// editing this file manually is not recommended\n\n"""
UPDATE_MSG = '        file %-35s - %s'

class OutputFile(object):
    'represent a file which gets overwritten only when its contents change'

    def __init__(self, fileName, printCopyright = True):
        'open file and write header'
        self.fileName = fileName
        self.fileNameTemp = fileName + '.temp'
        self.outFile = file(self.fileNameTemp, 'w')
        if printCopyright:
            self.outFile.write(Config.Config.getInstance().bufferCopyright.text)
        self.outFile.write(HEADER % os.path.basename(sys.argv[0]))

    def write(self, buffer):
        'output buffer of data to file'
        self.outFile.write(buffer)

    def close(self):
        'close temp file and overwrite original if they are different'
        self.outFile.close()
        if os.path.exists(self.fileName):
            if filecmp.cmp(self.fileName, self.fileNameTemp):
                os.unlink(self.fileNameTemp)
                Log.Log.getInstance().logMessage(UPDATE_MSG % (self.fileName, 'unchanged'))
            else:
                os.unlink(self.fileName)
                os.rename(self.fileNameTemp, self.fileName)
                Log.Log.getInstance().logMessage(UPDATE_MSG % (self.fileName, 'updated'))
        else:
            os.rename(self.fileNameTemp, self.fileName)
            Log.Log.getInstance().logMessage(UPDATE_MSG % (self.fileName, 'created'))

