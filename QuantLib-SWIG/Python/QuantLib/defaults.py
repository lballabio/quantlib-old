"""
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

# $Id$

from QuantLib import *
import sys
import os
import new
import code
import types

if sys.hexversion >= 0x020200a0:
    Date.isLeap  = staticmethod(Date_isLeap)
    Date.minDate = staticmethod(Date_minDate)
    Date.maxDate = staticmethod(Date_maxDate)
    Date.todaysDate = staticmethod(Date_todaysDate)

History._old___init__ = History.__init__
def History_new___init__(self,dates,values):
    values = values[:]
    for i in range(len(values)):
        values[i] = values[i] or nullDouble()
    self._old___init__(dates,values)
History.__init__ = History_new___init__

MarketElementHandle._old___init__ = MarketElementHandle.__init__
MarketElementHandle._old_linkTo = MarketElementHandle.linkTo
def MarketElementHandle_new___init__(self,h=None):
    self._old___init__()
    if h:
        self.linkTo(h)
def MarketElementHandle_new_linkTo(self,h):
    self._old_linkTo(h)
    self.currentLink = h
MarketElementHandle.__init__ = MarketElementHandle_new___init__
MarketElementHandle.linkTo = MarketElementHandle_new_linkTo

