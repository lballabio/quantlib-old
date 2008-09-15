
"""
 Copyright (C) 2007 Eric Ehlers

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

"""Exceptions encountered when processing design patterns."""

from gensrc.exceptions import exceptions

class PatternException(exceptions.GensrcException):
    """Exceptions encountered when processing design patterns."""

class SingletonOverrideNewException(PatternException):
    """Illegal attempt to override __new__ in a Singleton."""

    def __init__(self):
        """Initialize the SingletonOverrideNewException object."""
        self.value_ = 'Cannot override __new__ in a Singleton'

class SingletonCallException(PatternException):
    """Illegal attempt to instantiate Singleton without using instance()."""

    def __init__(self):
        """Initialize the SingletonCallException object."""
        self.value_ = 'Singletons may only be instantiated through instance()'

