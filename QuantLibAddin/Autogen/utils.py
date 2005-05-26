'utilities'

import common
import time
import sys
import os
import filecmp

# constants

CR_FILENAME = 'stub.copyright'
CR_BUFFER = ''
HEADER = '// this file generated automatically by %s\n\
// editing this file manually is not recommended\n\n'
UPDATE_MSG = '        file %-35s - %s'

def updateIfChanged(fileNew):
    'replace fileOrig with fileNew if they are different'
    fileOrig = fileNew[0:len(fileNew) - len(common.TEMPFILE)]
    if os.path.exists(fileOrig):
        if filecmp.cmp(fileNew, fileOrig):
            os.unlink(fileNew)
            logMessage(UPDATE_MSG % (fileOrig, 'unchanged'))
        else:
            os.unlink(fileOrig)
            os.rename(fileNew, fileOrig)
            logMessage(UPDATE_MSG % (fileOrig, 'updated'))
    else:
        os.rename(fileNew, fileOrig)
        logMessage(UPDATE_MSG % (fileOrig, 'created'))

def printTimeStamp(fileBuf):
    'write autogeneration message to source file'
    fileBuf.write(HEADER % os.path.basename(sys.argv[0]))
    
def printHeader(fileBuf):
    'write copyright and autogeneration message to source file'
    fileBuf.write(common.CR_BUFFER)
    printTimeStamp(fileBuf)

def loadBuffer(fileName):
    'return a buffer containing file fileName'
    fileBuf = open(fileName)
    buffer = fileBuf.read()
    fileBuf.close()
    return buffer

def init():
    'load the copyright stub file to a global buffer'
    common.CR_BUFFER = loadBuffer(CR_FILENAME)

def logMessage(msg):
    'print a message to stdout'
    print time.asctime() + ' ' + msg

