
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

"""Encapsulate state and behavior required to generate source code for a
function."""

from gensrc.Utilities import common
from gensrc.Utilities import buffer
from gensrc.Serialization import serializable
from gensrc.Functions import behavior
from gensrc.Functions import behaviorloop
from gensrc.Functions import supportedplatform
from gensrc.Parameters import parameterlist
from gensrc.Parameters import parameter
from gensrc.Configuration import environment

class Function(serializable.Serializable):
    """Encapsulate state and behavior required
    to generate source code for a function."""

    #############################################
    # class variables
    #############################################

    groupName_ = 'Functions'
    loopParameter_ = None
    enumeration_ = None
    generateVOs_ = False
    validatePermanent_ = ''
    VALIDATE_TRIGGER = '''
        ObjectHandler::validateRange(Trigger, "Trigger");'''

    #############################################
    # public interface
    #############################################

    def platformSupported(self, platformName, implementation):
        """Determine whether this function supported by given platform."""
        return self.supportedPlatforms_.has_key(platformName) \
            and self.supportedPlatforms_[platformName].implNum() >= implementation

    def xlMacro(self):
        """Determine whether this function requires macro on excel platform."""
        return self.supportedPlatforms_.has_key('Excel') \
            and self.supportedPlatforms_['Excel'].xlMacro()

    def calcInWizard(self):
        """Determine whether to calc this function under the Excel Function Wizard."""
        return self.supportedPlatforms_.has_key('Excel') \
            and self.supportedPlatforms_['Excel'].calcInWizard()

    def parameterList(self):
        return self.parameterList_

    def loopParameter(self):
        return self.loopParameter_

    def returnValue(self):
        return self.returnValue_

    def parameterObjectID(self):
        return self.parameterObjectID_

    def alias(self):
        return self.alias_

    def type(self):
        return self.type_

    def libraryFunction(self):
        return self.libraryFunction_

    def generateVOs(self):
        return self.generateVOs_

    def description(self):
        return self.description_

    def longDescription(self):
        return self.longDescription_

    def xlTrigger(self):
        return self.xlTrigger_

    def validatePermanent(self):
        return self.validatePermanent_

    def behavior(self):
        return self.behavior_

    def printDebug(self):
        self.parameterList_.printDebug()

    def visible(self):
        return self.visible_

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        serializer.serializeAttribute(self, common.NAME)
        serializer.serializeProperty(self, common.DESCRIPTION)
        serializer.serializeProperty(self, common.LONG_DESC, self.description_)
        serializer.serializeObjectDict(self, supportedplatform.SupportedPlatform)
        serializer.serializeProperty(self, common.ALIAS, environment.config().namespaceObjects() + '::' + self.name_)
        serializer.serializeObject(self, parameterlist.ParameterList)
        serializer.serializeBoolean(self, common.DOCUMENTATION_ONLY)
        serializer.serializeAttributeBoolean(self, common.TRIGGER, True)
        serializer.serializeAttributeBoolean(self, 'visible', True)

    def postSerialize(self):
        # some fields required for the Excel addin
        if self.dependencyTrigger_:
            self.xlTrigger_ = Function.VALIDATE_TRIGGER
        else:
            self.xlTrigger_ = ''

