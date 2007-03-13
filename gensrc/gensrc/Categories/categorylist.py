
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

"""class to represent a group of functions."""

from gensrc.Utilities import utilities
from gensrc.Categories import category
from gensrc.Functions import supportedplatform
from gensrc.Configuration import environment

class CategoryList(object):

    #############################################
    # public interface
    #############################################

    def categoryNames(self):
        return self.categoryNames_

    def categories(self, platformName, implementation = supportedplatform.AUTO):
        """serve up function category objects alphabetically by name."""
        for categoryName in self.categoryNames_:
            cat = self.categoryDict_[categoryName]
            if platformName == '*' or cat.platformSupported(platformName, implementation):
                yield cat

    #############################################
    # private member functions
    #############################################

    def __init__(self):

        self.categoryNames_ = environment.config().categoryNames()
        self.categoryNames_.sort()

        self.categoryDict_ = {}
        for categoryName in self.categoryNames_:
            self.categoryDict_[categoryName] = \
                utilities.serializeObject(category.Category, 'metadata/Functions/' + categoryName)

