
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

"""Global configuration state for gensrc application."""

import os
import gensrc
from gensrc.patterns import singleton
from gensrc.configuration import exceptions

def config():
    """Return the global configuration object."""
    return Environment.instance().configuration()

def getType(typeName, superTypeName = None):
    """Return a type object given the type and supertype name."""
    return Environment.instance().typeList().getType(typeName, superTypeName)

class Environment(singleton.Singleton):
    """Global configuration state for gensrc application."""

    #############################################
    # public interface
    #############################################

    def gensrcRootPath(self):
        """Return the root path of the application."""
        return gensrc.__path__[0] + '/'

    def addinRootPath(self):
        """Return the root path of the target source code tree."""
        return self.addinRootPath_

    def addinConfigPath(self):
        """Return the current working directory."""
        return self.addinConfigPath_

    def coreConfigPath(self):
        """Return the gensrc subdirectory of the ObjectHandler application."""
        return self.coreConfigPath_

    def typeList(self):
        """Return the global typelist object."""
        return self.typeList_

    def configuration(self):
        """Return the global configuration object."""
        return self.configuration_

    def init(self, configuration, typeList, ohDir):
        """Initialize the application environment."""
        self.typeList_ = typeList
        self.configuration_ = configuration
        cwd = os.getcwd().replace('\\', '/')
        self.addinConfigPath_ = cwd + '/'
        relativePath = self.configuration_.relativePath() + '/'
        if not self.addinConfigPath_.endswith(relativePath):
            raise exceptions.InvalidRelativePathException(
                cwd, self.configuration_.relativePath())
        self.addinRootPath_ = self.addinConfigPath_[0:len(self.addinConfigPath_) \
            - len(relativePath)]
        self.coreConfigPath_ = ohDir + '/gensrc/'
        if not os.path.exists(self.coreConfigPath_):
            raise exceptions.InvalidCorePathException(self.coreConfigPath_)

