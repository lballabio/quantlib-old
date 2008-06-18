
"""
 Copyright (C) 2008 Eric Ehlers

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

"""initialization for gensrc application."""

from gensrc.configuration import configuration
from gensrc.configuration import environment
from gensrc.types import typelist
from gensrc.utilities import utilities

def init(ohDir):
    """initialize the application.

    Logically these operations belong to the constructor of class Environment
    but must be physically separate to avoid circular imports."""
    config = utilities.serializeObject(configuration.Configuration, 'config/config')
    environment.Environment.instance().init(config, typelist.TypeList(), ohDir)

