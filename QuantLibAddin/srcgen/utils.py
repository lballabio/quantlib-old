
"""
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

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

'utilities'

import time
import common
import os
import filecmp
import sys
import parameter
import function
import config

# constants

CR_FILENAME = 'stub.copyright'
HEADER = """// this file generated automatically by %s
// editing this file manually is not recommended\n\n"""
UPDATE_MSG = '        file %-35s - %s'

def logMessage(msg):
    'print a message to stdout'
    print time.asctime() + ' ' + msg

def loadBuffer(fileName):
    'return a buffer containing file fileName'
    fileBuf = open(fileName)
    buffer = fileBuf.read()
    fileBuf.close()
    return buffer

def init():
    'load the copyright stub file to a global buffer'
    common.CR_BUFFER = loadBuffer(CR_FILENAME)

def testAttribute(node, attribute, value = ''):
    'test for existence of attribute & for equality to value (if supplied)'
    if not node.has_key(common.ATTS): return False
    if not node[common.ATTS].has_key(attribute): return False
    if value:
         return node[common.ATTS][attribute] == value
    else:
        return True

def stringToBool(str):
    if str.lower() == common.TRUE:
        return True
    elif str.lower() == common.FALSE:
        return False
    else:
         raise ValueError, 'unable to convert string "%s" to bool' % str

def childToBool(node, child):
    'test for existence of attribute & for equality to value (if supplied)'
    if not node.has_key(child): return False
    return stringToBool(node[child])

def printHeader(fileBuf):
    'write copyright and autogeneration message to source file'
    fileBuf.write(common.CR_BUFFER)
    printTimeStamp(fileBuf)

def printTimeStamp(fileBuf):
    'write autogeneration message to source file'
    fileBuf.write(HEADER % os.path.basename(sys.argv[0]))

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

def generateConversions(
        parameters, 
        sourceTypeOther, 
        sourceTypeNum = '',
        sourceTypeOptional = '',
        prefix = ''):
    'generate code to convert arrays to vectors/matrices'
    ret = ''
    indent = 8 * ' ';
    bigIndent = 12 * ' ';
    for param in parameters:
        if param.ignore: continue

        if param.tensorrank == common.SCALAR \
        and param.type != common.ANY \
        and not param.isOptional:
            continue

        suffix = param.type.capitalize()

        if param.type == common.STRING:
            targetType = 'std::string'
        elif param.type == common.ANY:
            targetType = 'boost::any'
        else:
            targetType = param.type

        if param.tensorrank == common.VECTOR: 
            targetType = 'std::vector < ' + targetType + ' >'
        elif param.tensorrank == common.MATRIX: 
            targetType = 'std::vector < std::vector < ' + targetType + ' > >'

        if sourceTypeOptional and param.isOptional:
            sourceType = sourceTypeOptional
        elif sourceTypeNum \
        and (param.type == common.LONG
        or   param.type == common.DOUBLE):
            sourceType = sourceTypeNum
        else:
            sourceType = sourceTypeOther

        if param.isOptional \
        and param.tensorrank == common.SCALAR:
            defaultValue = ', ' + param.defaultvalue
        else:
            defaultValue = ''

        tensor = param.tensorrank.capitalize()

        ret += indent + targetType + ' ' + param.name + tensor + ' = ' + '\n' \
            + bigIndent + prefix + sourceType + 'To' + tensor + suffix + '(' + param.name + defaultValue + ');\n'

    return ret

def getLibFuncName(function):
    if function.getObject:
        return 'objectPointer->getObject().' + function.libFunction
    else:
        return 'objectPointer->' + function.libFunction

def attributeToBool(node, attributeName):
    if testAttribute(node, attributeName):
        return stringToBool(node[common.ATTS][attributeName])
    else:
        return False

