'utilities'

import common
import time
import sys
import os

def generateParamList( \
        params, \
        indent, \
        datatypes, \
        prefix = '', \
        convertString = '', \
        convertLong = '', \
        reformatString = '', \
        suffix = '\n', \
        dereference = ''):
    paramList = ''
    i = 0
    for param in params:
        if datatypes == False:
            type = ''
        elif convertString and param['type'] == 'string':
            type = convertString + ' '
        elif convertLong and param['type'] == 'long':
            type = convertLong + ' '
        else:
            type = param['type'] + ' '
        if reformatString and param['type'] == 'string':
            full = reformatString % param['name']
        else:
            full = type + dereference + param['name']
        paramList += '%s%s%s' % (indent * 4 * ' ', prefix, full)
        i += 1
        if i < len(params):
            paramList += ',' + suffix
    return paramList

def printTimeStamp(fileBuf, commentChar):
    fileBuf.write(common.HEADER    \
        % (commentChar, os.path.basename(sys.argv[0]), time.asctime(), commentChar))
    
def printHeader(fileBuf):
    fileBuf.write(common.CR_BUFFER)
    printTimeStamp(fileBuf, '//')

def loadBuffer(fileName):
    fileBuf = open(fileName)
    buffer = fileBuf.read()
    fileBuf.close()
    return buffer

def init():
    common.CR_BUFFER = loadBuffer(common.CR_FILENAME)

def logMessage(msg):
    print time.asctime() + ' ' + msg
