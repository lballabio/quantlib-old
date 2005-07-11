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

'output QuantLibAddin source files'

import common
import utils
import params

# constants

ENUM_END   = '        );\n\n'
ENUM_LINE  = '            MAP("%s", %s);\n'
ENUM_START = '        REG_ENUM(%s,\n'
INCLUDES   = 'stub.Qla.includes'

def generateEnum(fileEnum, enumDef):
    fileEnum.write(ENUM_START % enumDef[common.CLASS])
    for enum in enumDef[common.DEFS]:
        if enumDef[common.CTOR] == common.TRUE:
            enumVal = enumDef[common.CLASS] + '(' + enum[common.ENUM] + ')'
        else:
            enumVal = enum[common.ENUM]
        fileEnum.write(ENUM_LINE % (enum[common.STRING], enumVal))
    fileEnum.write(ENUM_END)

def generateEnumSource(enumDefs):
    'generate source file for enumerations'
    bufInclude = utils.loadBuffer(INCLUDES)
    fileName = '../qla/enumregistry.cpp' + common.TEMPFILE
    fileEnum = file(fileName, 'w')
    utils.printHeader(fileEnum)
    fileEnum.write(bufInclude)
    for enumDef in enumDefs[common.ENUMS][common.ENUMDEFS]:
        if not utils.testAttribute(enumDef, 'documentation_only', 'true'):
            generateEnum(fileEnum, enumDef)
    fileEnum.write('    }\n\n}\n\n')
    fileEnum.close()
    utils.updateIfChanged(fileName)

def generate(enumDefs):
    'generate source code for QuantLibAddin'
    utils.logMessage('  begin generating QuantLibAddin ...')
    generateEnumSource(enumDefs)
    utils.logMessage('  done generating QuantLibAddin.')

