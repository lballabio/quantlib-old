
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

"""Exceptions encountered with the gensrc configuration."""

from gensrc.exceptions import exceptions

class ConfigurationException(exceptions.GensrcException):
    """Exceptions encountered with the gensrc configuration."""

class InvalidRelativePathException(ConfigurationException):
    """The configured path is not a right substring of the current working
    directory."""

    INVALID_RELATIVE_PATH_ERROR = """
Error initializing paths.  gensrc has been started from this directory:

%(cwd)s

gensrc has been configured with this relative path:

%(relativePath)s

The relative path is expected to be a right substring of the startup directory
but this is not the case."""

    def __init__(self, cwd, relativePath):
        """Initialize the InvalidRelativePathException object."""
        self.value_ = InvalidRelativePathException.INVALID_RELATIVE_PATH_ERROR % {
            'cwd' : cwd,
            'relativePath' : relativePath }

class InvalidCorePathException(ConfigurationException):
    """The configured path to the ObjectHandler application is incorrect."""

    INVALID_CORE_PATH_ERROR = """
Error initializing paths.  gensrc has been configured with the following path
to the gensrc directory in the ObjectHandler source code tree:

%(corePath)s

This path is not valid.  The value is derived by appending "/gensrc/" to the
value that was supplied to the oh_dir argument on the gensrc.py command line."""

    def __init__(self, corePath):
        """Initialize the InvalidCorePathException object."""
        self.value_ = InvalidCorePathException.INVALID_CORE_PATH_ERROR % {
            'corePath' : corePath }

