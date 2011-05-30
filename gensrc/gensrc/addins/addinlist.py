
"""
 Copyright (C) 2007, 2008 Eric Ehlers

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

"""A list of all the Addin objects in use for this running
instance of gensrc."""

from gensrc.addins import excel
from gensrc.addins import calc
from gensrc.addins import cpp
#from gensrc.addins import c
#from gensrc.addins import guile
from gensrc.addins import doxygen
from gensrc.addins import serialization
from gensrc.addins import valueobjects
from gensrc.addins import enumerations
from gensrc.addins import loop

from gensrc.categories import categorylist
from gensrc.enumerations import enumerationlist
from gensrc.utilities import utilities
from gensrc.utilities import log
from gensrc.configuration import environment

class AddinList(object):
    """A list of all the Addin objects in use for this running
    instance of gensrc."""

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
        'p' : (cpp.CppAddin, 'cpp'),
        #'c' : (c.CAddin, 'c'),
        #'g' : (guile.GuileAddin, 'guile'),
        'v' : (valueobjects.ValueObjects, 'valueobjects'),
        'e' : (enumerations.Enumerations, 'enumerations'),
        'l' : (loop.Loop, 'loop'),
        's' : (serialization.Serialization, 'serialization'),
        'd' : (doxygen.Doxygen, 'doxygen'),
    }

    #############################################
    # public interface
    #############################################

    def generate(self):
        """Generate the code for each Addin and write summary status
        to stdout."""

        self.generateCode()
        self.printSummary()

    def generateCode(self):
        """Generate the code for each Addin."""

        log.Log.instance().logMessage('begin ...')

        for addin in self.addins_:
            addin.generate(self.categoryList_, self.enumerationList_)

        log.Log.instance().logMessage('end')

    def printSummary(self):
        """Write summary status to stdout."""

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
        """Write debug info to stdout."""

        #for cat in self.categoryList_.categories('*'):
        #    cat.printDebug()

        for addin in self.addins_:
            addin.printDebug()

    def __init__(self, addinIds):
        """Initialize the AddinList object."""

        self.categoryList_ = categorylist.CategoryList()
        if environment.config().usingEnumerations():
            self.enumerationList_ = enumerationlist.EnumerationList()
        else:
            self.enumerationList_ = None
        self.categoryList_.init(self.enumerationList_)

        self.addins_ = []
        for addinId in addinIds:
            creator, fileName = AddinList.creators[addinId]
            self.addins_.append(utilities.serializeObject(creator, 'metadata/addins/' + fileName))

