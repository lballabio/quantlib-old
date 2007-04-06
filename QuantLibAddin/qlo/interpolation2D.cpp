
/*
 Copyright (C) 2006 Ferdinando Ametrano

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif
#include <qlo/interpolation2D.hpp>
#include <ql/math/interpolations/bilinearinterpolation.hpp>
#include <ql/math/interpolations/bicubicsplineinterpolation.hpp>
#include <qlo/Factories/interpolationsfactory.hpp>

namespace QuantLibAddin {

    Interpolation2D::Interpolation2D(
        const std::string &interpolation2DType, 
        const std::vector<double>& x,
        const std::vector<double>& y,
        const QuantLib::Matrix& dataMatrix)
    : x_(x), y_(y), dataMatrix_(dataMatrix)
    {
        QL_REQUIRE(y.size()==dataMatrix_.rows(),
            "y size (" << y.size() <<
            ") does not match number of rows in the data matrix ("
            << dataMatrix_.rows() << ")");
        QL_REQUIRE(x.size()==dataMatrix_.columns(),
            "x size (" << x.size() <<
            ") does not match number of columns in the data matrix ("
            << dataMatrix_.columns() << ")");

        libraryObject_ = Create<boost::shared_ptr<QuantLib::Interpolation2D> >()
            (interpolation2DType, x_.begin(), x_.end(), y_.begin(), y_.end(), dataMatrix_);

    }
  
}
