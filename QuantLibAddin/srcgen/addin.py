
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
            configFileName,
            categories):
        'initialize the object'
        config = parse.parseFile(configFileName)
        self.categories = categories
        self.name = config[common.NAME]
        self.platformId = config[common.PLATFORMID]
        self.rootDir = common.ADDIN_ROOT + self.name + '/'
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

