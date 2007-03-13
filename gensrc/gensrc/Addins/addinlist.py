
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

from gensrc.Addins import excel
from gensrc.Addins import calc
from gensrc.Addins import c
from gensrc.Addins import guile
from gensrc.Addins import doxygen
from gensrc.Addins import valueobjects
from gensrc.Addins import enumerations
from gensrc.Addins import loop

from gensrc.Categories import categorylist
from gensrc.Enumerations import enumerationlist
from gensrc.Utilities import utilities
from gensrc.Utilities import log
from gensrc.Configuration import configuration
from gensrc.Configuration import environment
from gensrc.Types import supertype

class AddinList(object):
    """class to encapsulate data and behavior 
    required to generate addin source code."""

    #############################################
    # class variables
    #############################################

    LINE_HEADER1 = '''\
addin           unchanged   updated     created     total'''
    LINE_HEADER2 = '''\
=============== =========== =========== =========== ==========='''
    LINE_FORMAT = '%-15s%12d%12d%12d%12d'

    creators = {
        'x' : (excel.ExcelAddin, 'excel'),
        'o' : (calc.CalcAddin, 'calc'),
        'g' : (guile.GuileAddin, 'guile'),
        'c' : (c.CAddin, 'c'),
        'v' : (valueobjects.ValueObjects, 'valueobjects'),
        'e' : (enumerations.Enumerations, 'enumerations'),
        'l' : (loop.Loop, 'loop'),
        'd' : (doxygen.Doxygen, 'doxygen'),
    }

    #############################################
    # public interface
    #############################################

    def generate(self):

        self.generateCode()
        self.printSummary()

    def generateCode(self):

        log.Log.instance().logMessage('begin ...')

        for addin in self.addins_:
            addin.generate(self.categoryList_, self.enumerationList_)

        log.Log.instance().logMessage('end')

    def printSummary(self):

        log.Log.instance().logMessage()
        log.Log.instance().logMessage(AddinList.LINE_HEADER1)
        log.Log.instance().logMessage(AddinList.LINE_HEADER2)

        totalAll = 0
        totalUnchanged = 0
        totalUpdated = 0
        totalCreated = 0

        for addin in self.addins_:
            totalLine = addin.unchanged() + addin.updated() + addin.created()
            totalUnchanged += addin.unchanged()
            totalUpdated += addin.updated()
            totalCreated += addin.created()
            totalAll += totalLine
            msg = AddinList.LINE_FORMAT % (addin.name(), addin.unchanged(), 
                addin.updated(), addin.created(), totalLine)
            log.Log.instance().logMessage(msg)

        if len(self.addins_) > 1:
            msg = AddinList.LINE_FORMAT % ('total', totalUnchanged, 
                totalUpdated, totalCreated, totalAll)
            log.Log.instance().logMessage(AddinList.LINE_HEADER2)
            log.Log.instance().logMessage(msg)

    def printDebug(self):

        for cat in self.categoryList_.categories('*'):
            cat.printDebug()

        for addin in self.addins_:
            addin.printDebug()

    #############################################
    # private member functions
    #############################################

    def __init__(self, addinIds):
        """initialize"""

        config = utilities.serializeObject(configuration.Configuration, 'config/config')
        environment.Environment.instance().setConfiguration(config)

        superTypeList = utilities.serializeObject(supertype.SuperTypeList, 'metadata/Types/types')
        environment.Environment.instance().setTypes(superTypeList)

        self.categoryList_ = categorylist.CategoryList()
        if config.usingEnumerations():
            self.enumerationList_ = enumerationlist.EnumerationList()
        else:
            self.enumerationList_ = None

        self.addins_ = []
        for addinId in addinIds:
            creator, fileName = AddinList.creators[addinId]
            self.addins_.append(utilities.serializeObject(creator, 'metadata/Addins/' + fileName))

