
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

"""encapsulate state and behavior required 
to generate source code for a function."""

import parameter
import serializable
import common
import textwrap

# contexts in which the function's parameters are listed:
DECLARATION = 0         # Addin function being declared
INVOCATION = 1          # Addin function calling corresponding QuantLib function

class Function(serializable.Serializable):
    """encapsulate state and behavior required 
    to generate source code for a function."""

    groupName = 'Functions'
    # Whether first param should be omitted when parameters are listed when
    # context == INVOCATION.  Defaulted to False here in the base class.
    skipFirst = False

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self.__dict__, common.NAME)
        serializer.serializeProperty(self.__dict__, common.DESCRIPTION)
        serializer.serializeProperty(self.__dict__, common.FUNCTION_CATEGORY)
        serializer.serializeProperty(self.__dict__, common.PLATFORMS, '*')
        serializer.serializeObjectList(self.__dict__, parameter.Parameter)

    def platformSupported(self, platformID):
        """determine whether this function supported by given platform."""
        if self.platforms == '*': return True
        return self.platforms.find(platformID) != -1

    def generateParameterList(self, rule, ignored = '', context = DECLARATION):
        """generate source code relating to a list of function parameters."""
        ret = ''
        i = 0
        eol = ''
        for parameter in self.Parameters:
            i += 1
            if context == INVOCATION:
                if i == 1 and self.skipFirst: continue
                if parameter.ignore : continue
            ret += eol + rule.apply(parameter)
            if i < self.ParameterCount: eol = ',\n'
        if ret: ret = '\n' + ret
        return ret

class Constructor(Function):
    """function which constructs a QuantLib object."""

    returnValue = parameter.ConstructorReturnValue
    OBJECT_DECLARE = '''\
        ObjHandler::obj_ptr objectPointer(new QuantLibAddin::%s(%s));\n'''
    OBJECT_STORE = '''\
        std::string returnValue =
            ObjHandler::storeObject(%s, objectPointer);'''

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        super(Constructor, self).serialize(serializer)
        serializer.serializeProperty(self.__dict__, common.LIBRARY_FUNCTION)

    def generateParameterList(self, rule, handleInput = '', context = DECLARATION):
        """generate source code relating to a list of function parameters."""
        ret = super(Constructor, self).generateParameterList(rule)
        if context == DECLARATION:
            if ret: comma = ','
            else:   comma = ''
            ret = '\n' + rule.indent + handleInput + comma + ret
        return ret

    def generateBody(self, addin):
        """generate source code for function body"""
        libraryCall = self.generateParameterList(addin.libraryCall, context = INVOCATION)
        objectDeclare = self.OBJECT_DECLARE % (self.libraryFunction, libraryCall)
        handle = addin.stringConvert % 'handleStub'
        objectStore = self.OBJECT_STORE % handle
        return objectDeclare + objectStore

class Member(Function):
    """function which invokes member function of existing QuantLib object."""

    skipFirst = True    # omit object handle when invoking its member function
    BODY = '''\
        boost::shared_ptr < QuantLibAddin::%s > objectPointer =
            OH_GET_OBJECT(QuantLibAddin::%s, %s);

        %s returnValue;
        returnValue = %s(%s);'''
    ERROR_NO_INPUTS = textwrap.dedent('''\
    Invalid definition for member function "%s":
    No parameters defined.  Member functions must have at least
    one parameter which is the handle of the object being accessed.''')

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        super(Member, self).serialize(serializer)
        serializer.serializeProperty(self.__dict__, common.LIBRARY_FUNCTION)
        serializer.serializeAttributeBoolean(self.__dict__, common.GET_OBJECT)
        serializer.serializeObject(self.__dict__, parameter.ReturnValue)

    def postSerialize(self):
        """perform post serialization initialization."""
        if not self.ParameterCount: sys.exit(ERROR_NO_INPUTS % self.name)

        # library class of object being accessed
        self.libraryClass = self.Parameters[0].libraryClass

        # code snippet to access underlying QuantLib object
        if self.getObject:
            self.accessLibFunc = 'objectPointer->getObject().' + self.libraryFunction
        else:
            self.accessLibFunc = 'objectPointer->' + self.libraryFunction

    def generateBody(self, addin):
        """generate source code for function body"""
        libraryReturnType = addin.libraryReturnType.apply(self.returnValue)
        libraryCall = self.generateParameterList(addin.libraryCall, context = INVOCATION)
        handle = addin.stringConvert % self.Parameters[0].name
        return self.BODY % (self.libraryClass, self.libraryClass, handle,
            libraryReturnType, self.accessLibFunc, libraryCall)

class Procedure(Function):
    """procedural function not associated with any QuantLib object."""
    BODY = '''\
        %s returnValue;
        returnValue = QuantLibAddin::%s(%s);'''

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        super(Procedure, self).serialize(serializer)
        serializer.serializeObject(self.__dict__, parameter.ReturnValue)

    def generateBody(self, addin):
        """generate source code for function body"""
        libraryReturnType = addin.libraryReturnType.apply(self.returnValue)
        libraryCall = self.generateParameterList(addin.libraryCall, context = INVOCATION)
        return self.BODY % (libraryReturnType, self.name, libraryCall)

