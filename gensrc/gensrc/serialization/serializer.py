
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

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

"""An object which can write a Serializable object to, or read it from,
a data stream."""

from gensrc.serialization import exceptions

class Serializer(object):
    """An object which can write a Serializable object to, or read it from,
    a data stream.

    Base class Serializer represents the interface for both Serializers 
    (Writers) and Deserializers (Readers).  All subclasses of Serializer 
    implement the same interface.  When a Serializable object invokes the 
    interface of a Serializer, the Serializable doesn't need to know if it's 
    being read or written e.g:

        class Foo(serializable.Serializable):
            def serialize(self, serializer):
                serializer.serializeAttribute(self, common.NAME)

    Base class Serializer is presently empty and is included to clarify
    class relationships. """

    def __init__(self):
        raise exceptions.SerializationOverrideException("Serializer", "__init__")

