'utilities'

import common
import time
import sys
import os

# constants

CR_FILENAME = 'copyright.txt'
HEADER = '%s this file generated automatically by %s on %s\n\
%s editing this file manually is not recommended\n\n'

def generateParamList(
        paramList,              # list of dicts describing parameters
        indent = 0,             # #/tabstops to indent 
        datatypes = False,      # include datatypes in output
        prefix = '',            # text to prefix to each parameter
        convertString = '',     # string conversion e.g. 'char *' to 'string'
        convertLong = '',       # conversion of parms with datatype 'long'
        reformatString = '',    # string reformatting e.g. 'std::string(%s)'
        dereference = '',       # dereference character e.g. * or &
        skipFirst = False,      # skip first parm in list (for object handles)
        xlateNames = False,     # translate parm name using its CLASS attribute
        arrayCount = False,     # C code requires array size separate from array
        appendTensor = False,   # append tensor rank (vector/matrix) to variable names
        convertVec = '',        # string to convert datatype to appropriate vector
        convertMat = '',        # string to convert datatype to appropriate matrix
        replaceTensor = '',     # replace vector/matrix datatype with given string 
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
                        paramName + 'Size,' + '\n'
                    type += '* '
                elif convertVec != '':
                    type = convertVec % type
                else:
                    type = replaceTensor + ' '
                    deref = ''
            elif param[common.TENSOR] == common.MATRIX:
                if arrayCount == True:
                    ret += indent * 4 * ' ' + prefix + 'long ' + \
                        paramName + 'Rows,' + '\n' + \
                        indent * 4 * ' ' + prefix + 'long ' + \
                        paramName + 'Cols,' + '\n'
                    type += '** '
                elif convertMat != '':
                    type = convertMat % type
                else:
                    type = replaceTensor + ' '
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
            ret += ',\n'
    return ret

def generateConversions(paramList):
    'generate code to convert arrays to vectors/matrices'
    ret = ''
    for param in paramList:
        if param[common.TENSOR] == common.VECTOR: 
            if param[common.TYPE] == common.STRING:
                type = 'std::string'
            else:
                type = param[common.TYPE]
            ret += 8 * ' ' + 'std::vector <' + type + \
                '> ' + param[common.NAME] + 'Vector =\n' + \
                12 * ' ' + param[common.TYPE] + 'ArrayToVector(' + \
                param[common.NAME] + ');\n'
        elif param[common.TENSOR] == common.MATRIX: 
            if param[common.TYPE] == common.STRING:
                type = 'std::string'
            else:
                type = param[common.TYPE]
            ret += 8 * ' ' + 'std::vector < std::vector < ' + \
                type + '> >' + param[common.NAME] + \
                'Matrix =\n' + 12 * ' ' + param[common.TYPE] + \
                'ArrayToMatrix(' + param[common.NAME] + ');\n'
    return ret

def printTimeStamp(fileBuf, commentChar):
    fileBuf.write(HEADER % (commentChar, 
        os.path.basename(sys.argv[0]), time.asctime(), commentChar))
    
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

