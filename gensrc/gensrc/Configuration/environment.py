
"""
 Copyright (C) 2007, 2008 Eric Ehlers

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

"""global configuration state for gensrc application."""

import gensrc
from gensrc.Patterns import singleton

def config():
    return Environment.instance().configuration()

def getType(typeName, superTypeName = None):
    return Environment.instance().typeList().getType(typeName, superTypeName)

class Environment(singleton.Singleton):
    """global configuration state for gensrc application."""

    #############################################
    # public interface
    #############################################

    def rootDirectory(self):
        return gensrc.__path__[0]

    def setConfiguration(self, configuration):
        self.configuration_ = configuration

    def setTypes(self, typeList):
        self.typeList_ = typeList

    def typeList(self):
        return self.typeList_

    def configuration(self):
        return self.configuration_

