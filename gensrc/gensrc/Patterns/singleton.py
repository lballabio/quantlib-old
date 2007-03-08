
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

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

from gensrc.Patterns import exceptions

"""
    Implementation of the Singleton pattern.
    Derived from code placed in the public domain by Gary Robinson.
"""

class MetaSingleton(type):
    """Meta type for the Singleton class."""

    def __new__(metaclass, strName, tupBases, dict):
        if dict.has_key('__new__'):
            raise exceptions.SingletonOverrideNewException()
        return super(MetaSingleton,metaclass).__new__(metaclass, strName, tupBases, dict)
        
    def __call__(cls, *lstArgs, **dictArgs):
        raise exceptions.SingletonCallException()

class Singleton(object):
    """Implementation of the Singleton pattern."""
    __metaclass__ = MetaSingleton
    
    def instance(cls):
        """Call this to instantiate an instance or retrieve the existing instance."""
        if not cls._isInstantiated():
            instance = cls.__new__(cls)
            instance.__init__()
            cls.cInstance = instance
        return cls.cInstance
    instance = classmethod(instance)
    
    def _isInstantiated(cls):
        return hasattr(cls, 'cInstance')
    _isInstantiated = classmethod(_isInstantiated)  

