
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
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
    resetCaller_ = ''
    generateVOs_ = False
    validatePermanent_ = ''
    XL_WIZ_CHECK = '''
#ifdef OHXL_ENABLE_GARBAGE_COLLECTION
        if (functionCall.IsCalledByFuncWiz())
            return 0;
#endif // OHXL_ENABLE_GARBAGE_COLLECTION'''
    VALIDATE_TRIGGER = '''
        ObjHandler::validateRange(trigger, "trigger");'''

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

    def xlCalcInWizard(self):
        """Determine whether this function should be enabled under excel wizard."""
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

    def deref(self):
        return self.deref_

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

    def xlWizardCheck(self):
        return self.xlWizardCheck_

    def validatePermanent(self):
        return self.validatePermanent_

    def resetCaller(self):
        return self.resetCaller_

    def behavior(self):
        return self.behavior_

    def printDebug(self):
        self.parameterList_.printDebug()

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

    def postSerialize(self):
        # some fields required for the Excel addin
        if self.xlCalcInWizard():
            self.xlWizardCheck_ = ''
        else:
            self.xlWizardCheck_ = Function.XL_WIZ_CHECK
        if self.dependencyTrigger_:
            self.xlTrigger_ = Function.VALIDATE_TRIGGER
        else:
            self.xlTrigger_ = ''

