'utilities'

import common
import time
import sys
import os
import filecmp

# constants

CR_FILENAME = 'stub.copyright'
CR_BUFFER = ''
HEADER = '%s this file generated automatically by %s\n\
%s editing this file manually is not recommended\n\n'
UPDATE_MSG = '        file %s - %s'

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
        convertMatStr = '',     # replace matrix datatype with given string 
        genDoxy = False):       # generate comments for Doxygen
    'reformat params into a list of parameters using given format options'
    ret = ''
    i = 0
    indentStr = indent * 4 * ' '
    for param in paramList:
        i += 1
        if skipFirst == True and i == 1:
            continue
        paramName = param[common.NAME]
        deref = dereference
        if datatypes == False:
            type = ''
            if param[common.TENSOR] == common.VECTOR and appendTensor == True:
                paramName += 'Array'
                deref = ''
            if param[common.TENSOR] == common.MATRIX and appendTensor == True:
                paramName += 'Matrix'
                deref = ''
        else:
            if convertString and param[common.TYPE] == common.STRING:
                if param[common.TENSOR] == common.SCALAR:
                    type = convertString + ' '
                else:
                    type = convertMatStr + ' '
            elif convertLong and param[common.TYPE] == common.LONG:
                type = convertLong + ' '
            else:
                type = param[common.TYPE] + ' '
            if param[common.TENSOR] == common.VECTOR:
                if convertVec != '':
                    type = convertVec % type
                else:
                    type = replaceTensor + ' '
                    deref = ''
            elif param[common.TENSOR] == common.MATRIX:
                if convertMat != '':
                    type = convertMat % type
                else:
                    type = replaceTensor + ' '
                    deref = ''
        if arrayCount == True:
            if datatypes == True:
                if convertString and param[common.TYPE] == common.STRING:
                    if param[common.TENSOR] == common.SCALAR:
                        type = convertString + ' '
                    else:
                        type = convertMatStr + ' '
                else:
                    type = param[common.TYPE] + ' '
            else:
                type = ''
            if param[common.TENSOR] == common.VECTOR:
                    if datatypes == True:
                        type += '* '
                        x = 'long '
                    else:
                        x = ''
                    ret += indentStr + prefix + x + \
                        paramName + 'Size,' + '\n'
            elif param[common.TENSOR] == common.MATRIX:
                    if datatypes == True:
                        type += '** '
                        x = 'long '
                    else:
                        x = ''
                    ret += indentStr + prefix + x + \
                        paramName + 'Rows,' + '\n' + \
                        indentStr + prefix + x + \
                        paramName + 'Cols,' + '\n'
        if reformatString and param[common.TYPE] == common.STRING \
                and param[common.TENSOR] == common.SCALAR:
            full = reformatString % paramName
        elif xlateNames == True and param[common.CLASS] != '':
            full = common.HANDLE + param[common.CLASS]
        else:
            full = type + deref + paramName
        if genDoxy == True:
            ret += '%s/*! %s\n%s*/\n' % (indentStr, param[common.DESC], indentStr)
        ret += '%s%s%s' % (indentStr, prefix, full)
        if i < len(paramList):
            ret += ',\n'
    return ret

def generateConversions(paramList):
    'generate code to convert arrays to vectors/matrices'
    ret = ''
    indent = 8 * ' ';
    bigIndent = 12 * ' ';
    for param in paramList:
        if param[common.TENSOR] == common.VECTOR: 
            if param[common.TYPE] == common.STRING:
                type = 'char *'
                type2 = 'char **'
            else:
                type = param[common.TYPE] + ' '
                type2 = param[common.TYPE] + ' '
            nmArray = param[common.NAME] + 'Array'
            nmSize = param[common.NAME] + 'ArraySize'
            ret += indent + type + '*' + nmArray + ';\n' \
                + indent + 'long ' + nmSize + ';\n' \
                + indent + 'Conversion< ' + type2 + '>::convertArray(\n' \
                + bigIndent + param[common.NAME] + ', ' + nmArray + ', ' \
                + nmSize + ');\n'
        elif param[common.TENSOR] == common.MATRIX: 
            if param[common.TYPE] == common.STRING:
                type = 'char'
            else:
                type = param[common.TYPE]
            nmMatrix = param[common.NAME] + 'Matrix'
            nmRows = param[common.NAME] + 'MatrixRows'
            nmCols = param[common.NAME] + 'MatrixCols'
            ret += indent + type + ' **' + nmMatrix + ';\n' \
                + indent + 'long ' + nmRows + ';\n' \
                + indent + 'long ' + nmCols + ';\n' \
                + indent + 'Conversion<' + type + '>::convertMatrix(\n' \
                + bigIndent + param[common.NAME] + ', ' + nmMatrix + ', ' \
                + nmRows + ', ' + nmCols + ');\n'
    return ret

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

def printTimeStamp(fileBuf, commentChar):
    'write autogeneration message to source file using native comment'
    fileBuf.write(HEADER % (commentChar, 
        os.path.basename(sys.argv[0]), commentChar))
    
def printHeader(fileBuf):
    'write copyright and autogeneration message to source file'
    fileBuf.write(common.CR_BUFFER)
    printTimeStamp(fileBuf, '//')

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

