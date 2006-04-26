
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

"""Encapsulate state and behavior required to generate source code for a 
function."""

import parameter
import serializable
import common

# contexts in which a function's parameters are listed:
DECLARATION = 0     # Addin function being declared
INVOCATION = 1      # Addin function calling corresponding QuantLib function
VALUEOBJECT = 2     # Generating VO code

class Function(serializable.Serializable):
    """Encapsulate state and behavior required 
    to generate source code for a function."""

    groupName = 'Functions'
    # Derived classes may override skipFirst to True to prevent the function's
    # first input parameter from being listed in generated source code.
    skipFirst = False
    functionCall = '''\
        ObjHandler::FunctionCall functionCall;'''

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeProperty(self, common.DESCRIPTION)
        serializer.serializeProperty(self, common.FUNCTION_CATEGORY)
        serializer.serializeProperty(self, common.PLATFORMS, '*')
        serializer.serializeProperty(self, common.ALIAS, 'QuantLibAddin::' + self.name)
        serializer.serializeObjectList(self, parameter.Parameter)

    def platformSupported(self, platformID):
        """Determine whether this function supported by given platform."""
        if self.platforms == '*': return True
        return self.platforms.find(platformID) != -1

    def generateParameterList(self, rule, context = DECLARATION, checkSkipFirst = True, x = False):
        """Generate source code relating to a list of function parameters."""
        returnValue = ''
        endOfLine = ''
        i = 0
        for parameter in self.Parameters:
            i += 1
            if context == INVOCATION:
                if checkSkipFirst and i == 1 and self.skipFirst: continue
                if parameter.ignore : continue
            elif context == VALUEOBJECT:
                if parameter.ignore : continue
            returnValue += endOfLine + rule.apply(parameter)
            if i < self.ParameterCount: endOfLine = ',\n'
        if returnValue: returnValue = '\n' + returnValue
        return returnValue
    
    def generateVO(self, addin): return "/* no VO - not a constructor*/"

class Constructor(Function):
    """Function which constructs a QuantLib object."""

    skipFirst = True    # omit handle when calling object constructor
    returnValue = parameter.ConstructorReturnValue
    BODY = '''\
        ObjHandler::obj_ptr objectPointer(new QuantLibAddin::%s(
            boost::shared_ptr < ObjHandler::Object::InstanceName > (new ObjHandler::InstanceNameXL(instanceName)),%s));

        std::string returnValue =
            ObjHandler::storeObject(objectPointer);'''
    functionCall = '''\
        ObjHandler::FunctionCall functionCall;
        functionCall.clearCell();'''

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        super(Constructor, self).serialize(serializer)
        serializer.serializeProperty(self, common.LIBRARY_FUNCTION)

    def postSerialize(self):
        """Perform post serialization initialization."""

        # implicit in the definition of a Constructor is that the first parameter
        # is a string to be used as the handle of the new object
        self.Parameters.insert(0, parameter.ParameterHandle(
            'name of this %s instance' % self.libraryFunction))
        self.Parameters[0].ql_type = ''
        self.ParameterCount += 1
        self.description = 'Construct an object of class %s and return its instance name' \
            % self.libraryFunction

    def generateBody(self, addin):
        """Generate source code for function body."""
        self.skipFirst = True
        libraryCall = self.generateParameterList(addin.libraryCall, INVOCATION)
        handle = addin.stringConvert % self.Parameters[0].name
        return self.BODY % (self.libraryFunction, libraryCall)

    def generateVO(self, addin):
        for p in self.Parameters: p.ql_type= ''
        libraryCall = self.generateParameterList(addin.libraryCall, INVOCATION, False)
        return '''\
        
        objectPointer->setProperties(boost::shared_ptr<ObjHandler::ValueObject>(new QuantLibAddin::ValueObjects::%s(%s)));
        ''' % (self.name, libraryCall)

class Member(Function):
    """Function which invokes member function of existing QuantLib object."""

    skipFirst = True    # omit object handle when invoking its member function
    BODY = '''\
        OH_GET_OBJECT(objectPointer, %s, %s)
        %s returnValue;
        returnValue = %s(%s)%s;'''

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        super(Member, self).serialize(serializer)
        serializer.serializeProperty(self, common.LIBRARY_FUNCTION)
        serializer.serializeAttribute(self, common.LIBRARY_CLASS)
        serializer.serializeAttributeBoolean(self, common.GET_OBJECT)
        serializer.serializeAttributeBoolean(self, common.NO_QLA_NS)
        serializer.serializeObject(self, parameter.ReturnValue)

    def postSerialize(self):
        """Perform post serialization initialization."""

        # implicit in the definition of a Member is that the first parameter
        # is the handle of the object to be retrieved
        self.Parameters.insert(0, parameter.ParameterHandle(
            'handle of existing %s object' % self.libraryClass))
        self.ParameterCount += 1

        # code snippet to access underlying QuantLib object
        if self.getObject:
            self.accessLibFunc = 'objectPointer->getObject().' + self.libraryFunction
        else:
            self.accessLibFunc = 'objectPointer->' + self.libraryFunction

    def generateBody(self, addin):
        """Generate source code for function body."""
        libraryReturnType = addin.libraryReturnType.apply(self.returnValue)
        libraryCall = self.generateParameterList(addin.libraryCall, INVOCATION)
        handle = addin.stringConvert % self.Parameters[0].name
        libraryClass = self.libraryClass
        if not self.noQlaNS: libraryClass = 'QuantLibAddin::' + libraryClass
        return self.BODY % (handle, libraryClass, libraryReturnType, 
            self.accessLibFunc, libraryCall, self.returnValue.conversion())

class Procedure(Function):
    """Procedural function not associated with any QuantLib object."""

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        super(Procedure, self).serialize(serializer)
        serializer.serializeObject(self, parameter.ReturnValue)

    def generateBody(self, addin):
        """Generate source code for function body."""
        if self.returnValue.type == common.VOID:
            returnCommand = ''
        else:            
            returnCommand = addin.libraryReturnType.apply(self.returnValue) \
                + ' returnValue;\n        returnValue = '
        libraryCall = '%s(%s)%s;' % (self.alias, 
            self.generateParameterList(addin.libraryCall, INVOCATION), self.returnValue.conversion())
        return '        ' + returnCommand + libraryCall

