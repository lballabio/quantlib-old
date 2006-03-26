
"""
 Copyright (C) 2005, 2006 Eric Ehlers
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

"""algorithms required to generate the source code for a given function 
parameter in a given context."""

import serializable
import common

class SubRule(serializable.Serializable):
    """the subset of a Rule pertaining to one or more tensor ranks."""

    groupName = 'SubRules'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeList(self, 'tensorRanks', 'tensorRank')
        serializer.serializeDict(self, 'replacements')

class Rule(serializable.Serializable):
    """this class encapsulates an algorithm required to generate the source
    code for a given function parameter in a given context."""

    groupName = 'Rules'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeObjectList(self, SubRule)
        serializer.serializeAttribute(self, common.DEREFERENCE)

class RuleGroup(serializable.Serializable):
    """a collection of Rules for generating source code for function parameters."""

    groupName = 'RuleGroups'
    convExceptionsMap = {
        'QuantLib::Date': lambda x: 'QuantLibAddin::createQLDate(%s)' % x
    }

    def __init__(self):
        """not all Rules will be populated for a given RuleGroup."""
        self.setPrefix = None
        self.setType = None
        self.reformatType = None
        self.setName = None
        self.dereferenceName = None
        self.reformatName = None
        self.setTypeConversion = None

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeAttribute(self, common.QL_TYPE)
        serializer.serializeAttributeBoolean(self, common.APPEND_CONVERSION_SUFFIX)
        serializer.serializeAttributeInteger(self, common.INDENT, 0)
        serializer.serializeObjectPropertyDict(self, Rule)

    def postSerialize(self):
        """perform post serialization initialization."""
        self.indent *= 4 * ' '

    def apply(self, param):
        """apply all available Rules to given parameter."""
        self.param = param

        self.prefix = ''
        self.paramType = param.type
        self.paramName = param.name
        self.conversion = ''

        if self.setSubrule(self.setPrefix):         self.invokeSetPrefix()
        if self.setSubrule(self.setType):           self.invokeSetType()
        if self.setSubrule(self.reformatType):      self.invokeReformatType()
        if self.setSubrule(self.setName):           self.invokeSetName()
        if self.setSubrule(self.dereferenceName):   self.invokeDereferenceName()
        if self.setSubrule(self.reformatName):      self.invokeReformatName()
        self.setLibConversion()
        if self.setSubrule(self.setTypeConversion): self.invokeSetTypeConversion()

        if self.appendConversionSuffix and self.param.needsConversion:
            self.paramName += common.CONVERSION_SUFFIX

        if self.paramType and self.paramName:
            delim = ' '
        else:
            delim = ''

        return self.indent + self.prefix + self.paramType \
            + delim + self.paramName + self.conversion

    def setSubrule(self, rule):
        """determine which SubRule, if any, applies to the tensor rank 
        of the given parameter."""
        self.subrule = None
        if rule:
            for subRule in rule.SubRules:
                for tensorRank in subRule.tensorRanks:
                    if tensorRank == self.param.tensorRank:
                        self.subrule = subRule
                        return True

    def applySubrule(self, init = ''):
        """invoke portion of SubRule that pertains to given parameter."""
        returnValue = init
        replacements = self.subrule.replacements
        if self.param.default and replacements.has_key(common.OPTIONAL):
            returnValue = replacements[common.OPTIONAL]
        elif replacements.has_key(self.param.type):
            returnValue = replacements[self.param.type]
        elif replacements.has_key(common.DEFAULT):
            returnValue = replacements[common.DEFAULT]
        if returnValue == common.NULL:
            returnValue = ''
        return returnValue

    def invokeSetPrefix(self):
        self.prefix = self.applySubrule(self.prefix)
        if self.prefix: self.prefix += ' '

    def invokeSetType(self):
        self.paramType = self.applySubrule(self.paramType)

    def invokeReformatType(self):
        format = self.applySubrule('%s')
        self.paramType = format % self.paramType

    def invokeSetName(self):
        self.paramName = self.applySubrule()

    def invokeDereferenceName(self):
        deref = self.applySubrule()
        self.paramName = deref + self.paramName

    def invokeReformatName(self):
        format = self.applySubrule('%s')
        self.paramName = format % self.paramName

    def setLibConversion(self):
        if self.ql_type and self.param.ql_type:
            if RuleGroup.convExceptionsMap.has_key(self.param.ql_type):
                self.paramName = RuleGroup.convExceptionsMap[self.param.ql_type](self.paramName)
            else:
                self.paramName = 'QuantLibAddin::Create<%s>()(%s)' % (self.param.ql_type, self.paramName)

    def invokeSetTypeConversion(self):
        sourceType = self.applySubrule()

        deref = ''
        if self.setTypeConversion.dereference and self.param.tensorRank == common.SCALAR:
            deref = self.setTypeConversion.dereference
        if self.param.default and self.param.tensorRank == common.SCALAR:
            defaultValue = ', ' + self.param.default
        else:
            defaultValue = ''

        indent = 8 * ' '
        self.conversion = ';\n' + indent + sourceType + 'To' \
            + self.param.tensorRank.capitalize() \
            + '(' + self.param.name + common.CONVERSION_SUFFIX \
            + ', ' + deref + self.param.name + defaultValue + ');\n'

