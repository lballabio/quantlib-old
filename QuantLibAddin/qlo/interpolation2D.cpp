/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006 Ferdinando Ametrano

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif
#include <qlo/interpolation2D.hpp>
#include <ql/math/interpolations/bilinearinterpolation.hpp>
#include <ql/math/interpolations/bicubicsplineinterpolation.hpp>
#include <qlo/Enumerations/Factories/interpolationsfactory.hpp>

namespace QuantLibAddin {

    Interpolation2D::Interpolation2D(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::string &interpolation2DType, 
        const std::vector<QuantLib::Real>& x,
        const std::vector<QuantLib::Real>& y,
        const QuantLib::Matrix& dataMatrix,
        bool permanent)
    : Extrapolator(properties, permanent),
      x_(x), y_(y), dataMatrix_(dataMatrix)
    {
        QL_REQUIRE(y.size()==dataMatrix.rows(),
            "y size (" << y.size() <<
            ") does not match number of rows in the data matrix ("
            << dataMatrix.rows() << ")");
        QL_REQUIRE(x.size()==dataMatrix.columns(),
            "x size (" << x.size() <<
            ") does not match number of columns in the data matrix ("
            << dataMatrix.columns() << ")");
        //const std::vector<QuantLib::Real>& x_ =
        //    boost::any_cast<std::vector<QuantLib::Real> >(propertyValue("XARRAY"));
        //const std::vector<QuantLib::Real>& y_ =
        //    boost::any_cast<std::vector<QuantLib::Real> >(propertyValue("YARRAY"));
        //const QuantLib::Matrix& dataMatrix_ =
        //    boost::any_cast<QuantLib::Matrix>(propertyValue("ZMATRIX"));
        libraryObject_ = ObjectHandler::Create<boost::shared_ptr<QuantLib::Interpolation2D> >()
            (interpolation2DType, x_.begin(), x_.end(), y_.begin(), y_.end(), dataMatrix_);

    }
  
}
