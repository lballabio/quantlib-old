'utilities'

import common
import time
import sys
import os

# constants

CR_FILENAME = 'copyright.txt'
HEADER = '%s this file generated automatically by %s\n\
%s editing this file manually is not recommended\n\n'

def generateParamList(
        paramList,              # list of dicts describing parameters
        indent = 0,             # #/tabstops to indent 
        datatypes = False,      # include datatypes in output
        prefix = '',            # text to prefix to each parameter
        convertString = '',     # string conversion e.g. 'char *' to 'string'
        convertLong = '',       # conversion of parms with datatype 'long'
        reformatString = '',    # string reformatting e.g. 'std::string(%s)'
        suffix = '\n',          # text to append to each parameter
        dereference = '',       # dereference character e.g. * or &
        skipFirst = False,      # skip first parm in list (for object handles)
        xlateNames = False,     # translate parm name using its CLASS attribute
        arrayCount = False,     # C code requires array size separate from array
        appendTensor = False,   # append tensor rank (vector/matrix) to variable names
        convertVec = '',        # string to convert datatype to appropriate vector
        replaceVec = '',        # replace vector datatype with given string
        convertMat = '',        # string to convert datatype to appropriate matrix
        replaceMat = '',        # replace matrix datatype with given string 
        convertString2 = ''):   # replace matrix datatype with given string 
    'reformat params into a list of parameters using given format options'
    ret = ''
    i = 0
    for param in paramList:
        i += 1
        if skipFirst == True and i == 1:
            continue
        paramName = param[common.NAME]
        deref = dereference
        if datatypes == False:
            type = ''
            if param[common.TENSOR] == common.VECTOR and appendTensor == True:
                paramName += 'Vector'
                deref = ''
            if param[common.TENSOR] == common.MATRIX and appendTensor == True:
                paramName += 'Matrix'
                deref = ''
        else:
            if convertString and param[common.TYPE] == common.STRING:
                if param[common.TENSOR] == common.SCALAR:
                    type = convertString + ' '
                else:
                    type = convertString2 + ' '
            elif convertLong and param[common.TYPE] == common.LONG:
                type = convertLong + ' '
            else:
                type = param[common.TYPE] + ' '
            if param[common.TENSOR] == common.VECTOR:
                if arrayCount == True:
                    ret += indent * 4 * ' ' + prefix + 'long ' + \
                        paramName + 'Size,' + suffix
                    type += '* '
                elif convertVec != '':
                    type = convertVec % type
                else:
                    type = replaceVec + ' '
                    deref = ''
            elif param[common.TENSOR] == common.MATRIX:
                if arrayCount == True:
                    ret += indent * 4 * ' ' + prefix + 'long ' + \
                        paramName + 'Rows,' + suffix + \
                        indent * 4 * ' ' + prefix + 'long ' + \
                        paramName + 'Cols,' + suffix
                    type += '** '
                elif convertMat != '':
                    type = convertMat % type
                else:
                    type = replaceMat + ' '
                    deref = ''
        if reformatString and param[common.TYPE] == common.STRING \
                and param[common.TENSOR] == common.SCALAR:
            full = reformatString % paramName
        elif xlateNames == True and param[common.CLASS] != '':
            full = common.HANDLE + param[common.CLASS]
        else:
            full = type + deref + paramName
        ret += '%s%s%s' % (indent * 4 * ' ', prefix, full)
        if i < len(paramList):
            ret += ',' + suffix
    return ret

def printTimeStamp(fileBuf, commentChar):
    fileBuf.write(HEADER
        % (commentChar, os.path.basename(sys.argv[0]), commentChar))
    
def printHeader(fileBuf):
    fileBuf.write(common.CR_BUFFER)
    printTimeStamp(fileBuf, '//')

def loadBuffer(fileName):
    fileBuf = open(fileName)
    buffer = fileBuf.read()
    fileBuf.close()
    return buffer

def init():
    common.CR_BUFFER = loadBuffer(CR_FILENAME)

def logMessage(msg):
    print time.asctime() + ' ' + msg

