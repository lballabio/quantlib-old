
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

"""procedural utilities"""

import xmlreader
import log
import config

LINE_HEADER1 = '''\
addin           unchanged   updated     created     total'''
LINE_HEADER2 = '''\
=============== =========== =========== =========== ==========='''
LINE_FORMAT = '%-15s%12d%12d%12d%12d'

def serializeObject(objectClass, fileName = None):
    """instantiate an xml reader and load requested object."""
    if not fileName: 
        fileName = config.Config.getInstance().rootDir + '/metadata/' + objectClass.__name__.lower()
    objectInstance = objectClass()
    serializer = xmlreader.XmlReader(fileName)
    objectInstance.serialize(serializer)
    objectInstance.postSerialize()
    return objectInstance

def generate(addins):
    log.Log.getInstance().logMessage('begin ...')
    
    for addin in addins:
        addin.generate()

    log.Log.getInstance().logMessage('end')

    log.Log.getInstance().logMessage()
    log.Log.getInstance().logMessage(LINE_HEADER1)
    log.Log.getInstance().logMessage(LINE_HEADER2)
    totalAll = 0
    totalUnchanged = 0
    totalUpdated = 0
    totalCreated = 0
    for addin in addins:
        totalLine = addin.unchanged + addin.updated + addin.created
        totalUnchanged += addin.unchanged
        totalUpdated += addin.updated
        totalCreated += addin.created
        totalAll += totalLine
        msg = LINE_FORMAT % (addin.name, addin.unchanged, addin.updated,
            addin.created, totalLine)
        log.Log.getInstance().logMessage(msg)

    if len(addins) > 1:
        msg = LINE_FORMAT % ('total', totalUnchanged, totalUpdated,
            totalCreated, totalAll)
        log.Log.getInstance().logMessage(LINE_HEADER2)
        log.Log.getInstance().logMessage(msg)

