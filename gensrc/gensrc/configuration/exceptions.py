
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

from gensrc.exceptions import exceptions

class ConfigurationException(exceptions.GensrcException):
    """Exceptions encountered when configuring gensrc."""

class InvalidRelativePathException(ConfigurationException):

    INVALID_RELATIVE_PATH_ERROR = """
Error initializing paths.  gensrc has been started from this directory:

%(cwd)s

gensrc has been configured with this relative path:

%(relativePath)s

The relative path is expected to be a right substring of the startup directory
but this is not the case."""

    def __init__(self, cwd, relativePath):
        self.value_ = InvalidRelativePathException.INVALID_RELATIVE_PATH_ERROR % {
            'cwd' : cwd,
            'relativePath' : relativePath }

class InvalidCorePathException(ConfigurationException):

    INVALID_CORE_PATH_ERROR = """
Error initializing paths.  gensrc has been configured with the following root path:

%(rootPath)s

gensrc has been configured with this relative path to the core application:

%(corePath)s

The concatenation of these two values is not a valid path:

%(rootPath)s%(corePath)s"""

    def __init__(self, rootPath, corePath):
        self.value_ = InvalidCorePathException.INVALID_CORE_PATH_ERROR % {
            'rootPath' : rootPath,
            'corePath' : corePath }

