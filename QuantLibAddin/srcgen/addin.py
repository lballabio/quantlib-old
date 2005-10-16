
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

'addins'

import common
import parse
import category
import rule

class Addin(object):

    def __init__(self,
            addinName,
            categories):
        'initialize the object'
        self.name = addinName
        self.categories = categories
        config = parse.parseFile(self.name)

        self.platformId = config[common.PLATFORMID]
        self.displayName = config[common.DISPLAY_NAME]
        self.rootDir = common.ADDIN_ROOT + self.displayName + '/'
        self.setRules(config)

    def generateCode(self, rule, params, skipFirst = False, skipIgnore = False):
        ret = ''
        i = 0
        eol = ''
        for param in params:
            i += 1
            if i == 1 and skipFirst: continue
            if param.ignore and skipIgnore: continue
            ret += eol + rule.apply(param)
            if i < len(params):
                eol = ',\n'
        if ret: ret = '\n' + ret
        return ret

    def generateConversions(self, params):
        ret = ''
        for param in params:
            if param.needsConversion:
                ret += self.ruleConversions.apply(param)
        return ret

