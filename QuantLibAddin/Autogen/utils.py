'utilities'

import common
import time
import sys
import os

def generateParamList(
        paramList,
        indent = 0,
        datatypes = False,
        prefix = '',
        convertString = '',
        convertLong = '',
        reformatString = '',
        suffix = '\n',
        dereference = '',
        skipFirst = False,
        xlateNames = False):
    'reformat params into a list of parameters using given format options'
    ret = ''
    i = 0
    for param in paramList:
        i += 1
        if skipFirst == True and i == 1:
            continue
        if datatypes == False:
            type = ''
        elif convertString and param[common.TYPE] == common.STRING:
            type = convertString + ' '
        elif convertLong and param[common.TYPE] == common.LONG:
            type = convertLong + ' '
        else:
            type = param[common.TYPE] + ' '
        if reformatString and param[common.TYPE] == common.STRING:
            full = reformatString % param[common.NAME]
        elif xlateNames == True and param[common.CLASS] != '':
            full = common.HANDLE + param[common.CLASS]
        else:
            full = type + dereference + param[common.NAME]
        ret += '%s%s%s' % (indent * 4 * ' ', prefix, full)
        if i < len(paramList):
            ret += ',' + suffix
    return ret

def printTimeStamp(fileBuf, commentChar):
    fileBuf.write(common.HEADER
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

