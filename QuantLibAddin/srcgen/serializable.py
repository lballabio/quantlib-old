
"""
 Copyright (C) 2005, 2006 Eric Ehlers

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

"""Class to represent an object which is capable of being serialized by a
Serializer, or deserialized by a Deserializer."""

class Serializable(object):
    """Class to represent an object which is capable of being serialized by a
    Serializer, or deserialized by a Deserializer.  The interface is the same
    in either case and the word Serialize indicates both serialization and
    deserialization."""

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        sys.exit('method not implemented')

    def postSerialize(self):
        """invoke any post serialization behavior that may be required."""
        pass
        #print "Serializable #########################"

    def key(self):
        """return unique identifier for this object."""
        return self.name

