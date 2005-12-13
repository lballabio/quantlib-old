
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

'rule'

import common
import utils
import parameter

# constants

SET_PREFIX = 'setPrefix'
SET_TYPE = 'setType'
REFORMAT_TYPE = 'reformatType'
SET_NAME = 'setName'
DEREFERENCE_NAME = 'dereferenceName'
REFORMAT_NAME = 'reformatName'
SET_LIB_CONVERSION = 'setLibConversion'
SET_TYPE_CONVERSION = 'setTypeConversion'

class Rule(object):
    convExceptionsMap = {
        'QuantLib::Date': lambda x: 'QuantLibAddin::createQLDate(%s)' % x
    }

    def __init__(self,
            ruleDef):
        'initialize the object'
        self.definition = ruleDef
        self.appendTensorRank = utils.attributeToBool(ruleDef, common.APPEND_TENSORRANK)
        self.typeConversion = utils.attributeToBool(ruleDef, common.TYPE_CNV)
        self.indent = int(utils.getAttribute(ruleDef, common.INDENT, '0')) * 4 * ' '

    def apply(self, param):
        self.param = param

        self.prefix = ''
        self.paramType = param.type
        self.paramName = param.name
        self.conversion = ''

        self.setPrefix()
        self.setType()
        self.reformatType()
        self.setName()
        self.dereferenceName()
        self.reformatName()
        self.setLibConversion()
        self.setTypeConversion()

        if self.appendTensorRank and self.param.needsConversion:
            self.paramName += self.param.tensorRank.capitalize()

        if self.paramType and self.paramName:
            delim = ' '
        else:
            delim = ''

        return self.indent + self.prefix + self.paramType + delim + self.paramName + self.conversion

    def setSubrule(self, rule):
        self.subrule = None
        if self.definition.has_key(rule):
            for key in self.definition[rule]:
                if key.find(self.param.tensorRank) != -1:
                    self.subrule = self.definition[rule][key]
                    return

    def applySubrule(self, init = ''):
        ret = init
        if self.param.isOptional and self.subrule.has_key(common.OPTIONAL):
            ret = self.subrule[common.OPTIONAL]
        elif self.subrule.has_key(self.param.type):
            ret = self.subrule[self.param.type]
        elif self.subrule.has_key(common.DEFAULT):
            ret = self.subrule[common.DEFAULT]
        if ret == common.NULL:
            ret = ''
        return ret

    def setPrefix(self):
        self.setSubrule(SET_PREFIX)
        if self.subrule:
            self.prefix = self.applySubrule(self.prefix)
            if self.prefix: self.prefix += ' '

    def setType(self):
        self.setSubrule(SET_TYPE)
        if self.subrule:
            self.paramType = self.applySubrule(self.paramType)

    def reformatType(self):
        self.setSubrule(REFORMAT_TYPE)
        if self.subrule:
            format = self.applySubrule('%s')
            self.paramType = format % self.paramType

    def setName(self):
        self.setSubrule(SET_NAME)
        if self.subrule:
            self.paramName = self.applySubrule()

    def dereferenceName(self):
        self.setSubrule(DEREFERENCE_NAME)
        if self.subrule:
            deref = self.applySubrule()
            self.paramName = deref + self.paramName

    def reformatName(self):
        self.setSubrule(REFORMAT_NAME)
        if self.subrule:
            format = self.applySubrule('%s')
            self.paramName = format % self.paramName

    def setLibConversion(self):
        if self.typeConversion and self.param.typeConversion:
            if Rule.convExceptionsMap.has_key(self.param.typeConversion):
                self.paramName = Rule.convExceptionsMap[self.param.typeConversion](self.paramName)
            else:
                self.paramName = 'QuantLibAddin::Create<%s>()(%s)' % (self.param.typeConversion, self.paramName)

    def setTypeConversion(self):
        self.setSubrule(SET_TYPE_CONVERSION)
        if not self.subrule: return
        sourceType = self.applySubrule()

        deref = ''
        if self.param.isOptional and self.param.tensorRank == common.SCALAR:
            defaultValue = ', ' + self.param.defaultValue
            if self.definition[SET_TYPE_CONVERSION].has_key(common.DEREFERENCE):
                deref = self.definition[SET_TYPE_CONVERSION][common.DEREFERENCE]
        else:
            defaultValue = ''

        if self.definition[SET_TYPE_CONVERSION].has_key(common.PREFIX):
            prefix = self.definition[SET_TYPE_CONVERSION][common.PREFIX]
        else:
            prefix = ''

        indent = 8 * ' '
        self.conversion = ';\n' + indent + prefix + sourceType + 'To' \
            + self.param.tensorRank.capitalize() \
            + '(' + self.param.name + self.param.tensorRank.capitalize() \
            + ', ' + deref + self.param.name + defaultValue + ');\n'

