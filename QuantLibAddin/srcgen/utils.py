
"""
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

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

'utilities'

import time
import XmlReader

def logMessage(msg):
    'print a message to stdout'
    print time.asctime() + ' ' + msg

def serializeObject(objectClass, fileName = None):
    'instantiate an xml reader and load requested object'
    if not fileName: fileName = objectClass.__name__
    objectInstance = objectClass()
    serializer = XmlReader.XmlReader(fileName)
    objectInstance.serialize(serializer)
    return objectInstance

