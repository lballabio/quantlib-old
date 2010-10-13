/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers

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
*/

/*! \file
    \brief class Range - Container for a matrix of doubles
*/

#ifndef oh_range_hpp
#define oh_range_hpp

#include <oh/object.hpp>
#include <vector>

namespace ObjectHandler {

    //! Container for a matrix of doubles
    /*! This class was implemented to facilitate serialization of a range of
        cells in Microsoft Excel.  In practice this functionality has not
        proved necessary - it is more expedient to deal with objects or values
        of a known type - so this class is not fully implemented.
    */
    class Range : public Object {
    public:
        Range(
            const boost::shared_ptr<ValueObject>& properties,
            const std::vector<std::vector<double> > &values,
            bool permanent)
            : Object(properties, permanent), values_(values) {}
    private:
        std::vector<std::vector<double> > values_;
    };

}

#endif

