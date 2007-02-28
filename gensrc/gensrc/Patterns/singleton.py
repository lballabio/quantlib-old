
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

"""
    Implementation of the Singleton pattern.
    Derived from code placed in the public domain by Gary Robinson.
"""

class MetaSingleton(type):
    """Meta type for the Singleton class."""

    def __new__(metaclass, strName, tupBases, dict):
        if dict.has_key('__new__'):
            sys.exit('Can not override __new__ in a Singleton')
        return super(MetaSingleton,metaclass).__new__(metaclass, strName, tupBases, dict)
        
    def __call__(cls, *lstArgs, **dictArgs):
        sys.exit('Singletons may only be instantiated through getInstance()')

class Singleton(object):
    """Implementation of the Singleton pattern."""
    __metaclass__ = MetaSingleton
    
    def getInstance(cls):
        """Call this to instantiate an instance or retrieve the existing instance."""
        if not cls._isInstantiated():
            instance = cls.__new__(cls)
            instance.__init__()
            cls.cInstance = instance
        return cls.cInstance
    getInstance = classmethod(getInstance)
    
    def _isInstantiated(cls):
        return hasattr(cls, 'cInstance')
    _isInstantiated = classmethod(_isInstantiated)  

