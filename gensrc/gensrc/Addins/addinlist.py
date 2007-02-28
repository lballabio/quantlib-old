
"""
 Copyright (C) 2007 Eric Ehlers

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

import sys
import getopt

from gensrc.Addins import addinqla
from gensrc.Addins import addinexcel
from gensrc.Addins import addincalc
from gensrc.Addins import addinc
from gensrc.Addins import addinguile
from gensrc.Addins import addindoxygen
from gensrc.Addins import valueobjects
from gensrc.Addins import loop
from gensrc.Categories import categorylist
from gensrc.Enumerations import enumerationlist
from gensrc.Utilities import utilities
from gensrc.Utilities import log
from gensrc.Configuration import configuration
from gensrc.Configuration import environment

class AddinList(object):
    """class to encapsulate data and behavior 
    required to generate addin source code."""

    LINE_HEADER1 = '''\
addin           unchanged   updated     created     total'''
    LINE_HEADER2 = '''\
=============== =========== =========== =========== ==========='''
    LINE_FORMAT = '%-15s%12d%12d%12d%12d'

    creators = {
        'q' : addinqla.AddinQla,
        'e' : addinexcel.AddinExcel,
        'o' : addincalc.AddinCalc,
        'c' : addinc.AddinC,
        'g' : addinguile.AddinGuile,
        'd' : addindoxygen.AddinDoxygen,
        'v' : valueobjects.ValueObjects,
        'l' : loop.Loop,
    }

    def __init__(self, addinIds):
        """initialize"""

        config = utilities.serializeObject(configuration.Configuration, 'config/config')
        environment.Environment.getInstance().setConfiguration(config)

        self.categoryList_ = categorylist.CategoryList()
        if config.usingEnumerations:
            self.enumerationList_ = enumerationlist.EnumerationList()
	else:
            self.enumerationList_ = None

        self.addins = []
        for addinId in addinIds:
            creator = AddinList.creators[addinId]
            self.addins.append(utilities.serializeObject(creator))

    def generate(self):

        self.generateCode()
        self.printSummary()

    def generateCode(self):

        log.Log.getInstance().logMessage('begin ...')
        
        for addin in self.addins:
            addin.generate(self.categoryList_, self.enumerationList_)
    
        log.Log.getInstance().logMessage('end')
    
    def printSummary(self):

        log.Log.getInstance().logMessage()
        log.Log.getInstance().logMessage(AddinList.LINE_HEADER1)
        log.Log.getInstance().logMessage(AddinList.LINE_HEADER2)

        totalAll = 0
        totalUnchanged = 0
        totalUpdated = 0
        totalCreated = 0

        for addin in self.addins:
            totalLine = addin.unchanged + addin.updated + addin.created
            totalUnchanged += addin.unchanged
            totalUpdated += addin.updated
            totalCreated += addin.created
            totalAll += totalLine
            msg = AddinList.LINE_FORMAT % (addin.name, addin.unchanged, 
                addin.updated, addin.created, totalLine)
            log.Log.getInstance().logMessage(msg)
    
        if len(self.addins) > 1:
            msg = AddinList.LINE_FORMAT % ('total', totalUnchanged, 
                totalUpdated, totalCreated, totalAll)
            log.Log.getInstance().logMessage(AddinList.LINE_HEADER2)
            log.Log.getInstance().logMessage(msg)
    
