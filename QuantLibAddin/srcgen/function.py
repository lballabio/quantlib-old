
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

'function'

import common
import utils
import parameter

class Function(object):

    def __init__(self,
            functionDef):
        'initialize the object'
        self.name = functionDef[common.NAME]
        self.description = functionDef[common.DESC]
        self.libFunction = functionDef[common.LIBFUNC]
        self.functionCat = functionDef[common.CATG]
        self.isConstructor = utils.stringToBool(functionDef[common.CTOR])
        self.parameters = []
        for paramDef in functionDef[common.PARAMS]:
            self.parameters.append(parameter.Parameter(paramDef))
        self.parameterCount = len(self.parameters)
        if not self.isConstructor \
        and len(self.parameters):
            self.className = self.parameters[0].className
        self.returnValue = parameter.Parameter(functionDef[common.RETURNVALUE])
        self.supportedPlatforms = utils.getVal(functionDef, common.PLATFORMS, '*')
        self.getObject = utils.attributeToBool(functionDef, common.GET_OBJECT)

    def platformSupported(self, platform):
        if self.supportedPlatforms == '*': return True
        return self.supportedPlatforms.find(platform) != -1

