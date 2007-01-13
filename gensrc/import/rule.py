
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

"""Algorithms required to generate the source code for a given function 
parameter in a given context."""

import serializable
import common
import code
import config

class DataValue(serializable.Serializable):

    groupName = 'DataValues'

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeValue(self)
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeAttribute(self, 'codeID')

    def apply(self):
        if self.codeID:
            return code.lines[self.codeID]
        else:
            return self.value

class DataType(serializable.Serializable):

    groupName = 'DataTypes'

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeValue(self)
        serializer.serializeAttribute(self, 'codeID')
        serializer.serializeObjectContainer(self, DataType, True)
        serializer.serializeObjectDict(self, DataValue, allowNone = True)

    def valueRetrieve(self, const, value):
        if not self.DataValues:
            return None
        if self.DataValues.has_key(value):
            return self.DataValues[value].apply()
        elif self.DataValues.has_key(common.CONST) and const:
            return self.DataValues[common.CONST].apply()
        elif self.DataValues.has_key(common.OTHER):
            return self.DataValues[common.OTHER].apply()
        else:
            return None

    def apply(self, param, value = None):
        valueRetrieve = self.valueRetrieve(param.const, value)
        if valueRetrieve:
            return valueRetrieve
        elif self.DataTypes:
            return self.DataTypes.apply(param)
        elif self.codeID:
            return code.lines[self.codeID]
        else:
            return self.value

class DataTypes(serializable.Serializable):

    def apply(self, param):
        """Invoke portion of SubRule that pertains to given parameter."""
        # loopParameter
        if param.loop and self.DataTypes.has_key(common.LOOP):
            return self.DataTypes[common.LOOP].apply(param, param.loop)
        # libraryClass
        elif param.libraryClass and self.DataTypes.has_key(common.LIBRARY_CLASS):
            return self.DataTypes[common.LIBRARY_CLASS].apply(param, param.libraryClass)
        # libraryType:
        # - if the given library type is listed under 'implicit conversions', then fall 
        #   through to cases below (where the parameter will be treated as a native 
        #   datatype e.g. long/double/etc.)
        # - otherwise, if rule libraryType has been specified then apply that rule.
        elif param.libraryType and self.DataTypes.has_key(common.LIBRARY_TYPE) \
        and param.libraryType not in config.Config.getInstance().implicitConversions:
            return self.DataTypes[common.LIBRARY_TYPE].apply(param, param.libraryType)
        # objectClass
        elif param.objectClass and self.DataTypes.has_key(common.OBJECT):
            return self.DataTypes[common.OBJECT].apply(param, param.objectClass)
        # enumeration
        elif param.enumeration and self.DataTypes.has_key(common.ENUM):
            return self.DataTypes[common.ENUM].apply(param, param.enumeration)
        # libToHandle
        elif param.libToHandle and self.DataTypes.has_key(common.LIB_TO_HANDLE):
            return self.DataTypes[common.LIB_TO_HANDLE].apply(param, param.libToHandle)
        # handleToLib
        elif param.handleToLib and self.DataTypes.has_key(common.HANDLE_TO_LIB):
            return self.DataTypes[common.HANDLE_TO_LIB].apply(param, param.handleToLib)
        # underlyingClass
        elif param.underlyingClass and self.DataTypes.has_key(common.UNDERLYING_CLASS):
            return self.DataTypes[common.UNDERLYING_CLASS].apply(param, param.underlyingClass)
        # underlyingClassnonconst
        elif param.underlyingClassNonconst and self.DataTypes.has_key(common.UNDERLYING_CLASS_NONCONST):
            return self.DataTypes[common.UNDERLYING_CLASS_NONCONST].apply(param, param.underlyingClassNonconst)
        # default
        elif param.default and self.DataTypes.has_key(common.DEFAULT):
            return self.DataTypes[common.DEFAULT].apply(param, param.default)
        # vectorIterator
        elif param.vectorIterator and self.DataTypes.has_key(common.VECTOR_ITERATOR):
            return self.DataTypes[common.VECTOR_ITERATOR].apply(param, param.vectorIterator)
        # datatype
        elif self.DataTypes.has_key(param.type):
            return self.DataTypes[param.type].apply(param, param.type)
        # other
        elif self.DataTypes.has_key(common.OTHER):
            return self.DataTypes[common.OTHER].apply(param)
        else:
            return None

class SubRule(serializable.Serializable):
    """the subset of a Rule pertaining to one or more tensor ranks."""

    groupName = 'SubRules'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeList(self, 'tensorRanks', 'tensorRank')
        serializer.serializeObjectContainer(self, DataType)

    def apply(self, param):
        """invoke portion of SubRule that pertains to given parameter."""
        return self.DataTypes.apply(param)

class Wrap(serializable.Serializable):
    """A class to process the 'wrap' text for a rule.  If this class
    is specified in the XML, then ParameterList will invoke it after the
    Rule is processed e.g.
        T = W % R
    where R is the text derived from Rule, W is the value of Wrap, and
    T is the final text to be returned to the Addin.

    This can be used e.g. to prepend a comment to the generated code."""

    name = 'Wrap'

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeValue(self)
        serializer.serializeAttribute(self, 'codeID')

    def postSerialize(self):
        if self.codeID:
            self.text = code.lines[self.codeID]
        else:
            self.text = self.value

class Rule(serializable.Serializable):
    """This class encapsulates an algorithm required to generate the source
    code for a given function parameter in a given context."""

    groupName = 'Rules'

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeAttribute(self, common.DELIM, '')
        serializer.serializeAttributeBoolean(self, common.CHECK_PARAM_IGNORE)
        serializer.serializeAttributeBoolean(self, common.CHECK_SKIP_FIRST)
        serializer.serializeAttributeBoolean(self, common.PAD_LAST_PARAM, False)
        serializer.serializeAttributeInteger(self, common.INDENT, 0)
        serializer.serializeObject(self, Wrap, True)
        serializer.serializeObjectList(self, SubRule)

    def postSerialize(self):
        """Perform post serialization initialization."""
        self.indent *= 4 * ' '
        if self.Wrap:
            self.wrapText = self.Wrap.text
        else:
            self.wrapText = None

    def apply(self, param):
        """Apply all available Rules to given parameter."""

        if self.checkParameterIgnore and param.ignore: return

        if self.padLastParamDesc and param.lastParameter:
            self.paramDesc = param.description + '  '
        else:
            self.paramDesc = param.description

        self.param = param

        if self.applySubRule():
            return self.invokeRule()

    def applySubRule(self):
        '''Apply the SubRule, if any, which pertains to the tensor rank
        of the given parameter'''
        for subRule in self.SubRules:
            if self.param.tensorRank in subRule.tensorRanks:
                self.subRuleResult = subRule.apply(self.param)
                return self.subRuleResult != None

    def invokeRule(self):
        return self.subRuleResult % {
            common.DEFAULT_VALUE : self.param.default,
            common.DESCRIPTION : self.paramDesc,
            common.DESC_LEN : len(self.paramDesc),
            common.ENUM : self.param.enumeration,
            common.HANDLE_TO_LIB : self.param.handleToLib,
            common.HANDLE_TO_LIB2 : self.param.handleToLib2,
            common.INDENT : self.indent,
            common.INDENT2 : self.indent + '    ',
            common.LIB_TO_HANDLE : self.param.libToHandle,
            common.LIBRARY_CLASS : self.param.libraryClass,
            common.LIBRARY_TYPE : self.param.libraryType,
            common.NAME : self.param.name,
            common.NAMESPACE_LIB : config.Config.getInstance().namespaceLibrary,
            common.NAMESPACE_OBJ : config.Config.getInstance().namespaceObjects,
            common.OBJECT : self.param.objectClass,
            common.TENSOR_RANK : self.param.tensorRank,
            common.TYPE : self.param.type,
            common.UNDERLYING_CLASS : self.param.underlyingClass,
            common.UNDERLYING_CLASS_NONCONST : self.param.underlyingClassNonconst }

