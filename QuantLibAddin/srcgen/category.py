
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

'category'

import common
import utils
import parse
import function

class Category(object):

    def __init__(self,
            categoryName):
        'initialize the object'
        self.name = categoryName
        categoryDef = parse.parseFile(self.name)
        self.headerOnly = utils.stringToBool(categoryDef[common.HDRONLY])
        self.displayName = categoryDef[common.DISPLAY_NAME]
        self.description = categoryDef[common.DESC]
        self.functions = {}
        self.functions[common.DICT] = {}
        self.functions[common.KEYS] = []
        for functionDef in categoryDef[common.FUNCS]:
            func = function.Function(functionDef)
            self.functions[common.DICT][func.name] = func
            self.functions[common.KEYS].append(func.name)
        self.functions[common.KEYS].sort()
        self.functionCount = len(self.functions)

