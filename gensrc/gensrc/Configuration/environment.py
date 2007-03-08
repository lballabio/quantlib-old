
"""
 Copyright (C) 2007 Eric Ehlers

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

"""global configuration state for srcgen application."""

import gensrc
from gensrc.Patterns import singleton

def config():
    return Environment.instance().configuration()

def getType(typeName, superTypeName = None):
    return Environment.instance().superTypeList().getType(typeName, superTypeName)

class Environment(singleton.Singleton):
    """global configuration state for gensrc application."""

    def rootDirectory(self):
        return gensrc.__path__[0]

    def setConfiguration(self, configuration):
        self.configuration_ = configuration

    def setTypes(self, superTypeList):
        self.superTypeList_ = superTypeList

    def configuration(self):
        return self.configuration_

    def superTypeList(self):
        return self.superTypeList_

