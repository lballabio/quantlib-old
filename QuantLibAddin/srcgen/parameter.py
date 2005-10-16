
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

'parameter'

import common
import utils

class Parameter(object):

    def __init__(self,
            paramDef):
        'initialize the object'
        if paramDef.has_key(common.NAME):
            self.name = paramDef[common.NAME]
        else:
            self.name = ''
        if utils.testAttribute(paramDef, common.CLASS):
            self.className = paramDef[common.ATTS][common.CLASS]
        else:
            self.className = None
        if utils.testAttribute(paramDef, common.TYPE_CNV):
            self.typeConversion = paramDef[common.ATTS][common.TYPE_CNV]
        else:
            self.typeConversion = None
        self.type = paramDef[common.TYPE]
        self.tensorRank = paramDef[common.TENSORRANK]
        self.description = paramDef[common.DESC]
        self.isOptional = utils.testAttribute(paramDef, common.DEFAULT)
        if self.isOptional:
            self.defaultValue = paramDef[common.ATTS][common.DEFAULT]
        self.ignore = utils.attributeToBool(paramDef, common.IGNORE)

        if self.ignore \
        or (self.tensorRank == common.SCALAR \
        and self.type != common.ANY \
        and not self.isOptional):
            self.needsConversion = False
        else:
            self.needsConversion = True


