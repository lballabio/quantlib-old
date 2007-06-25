
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

#ifndef qla_interpolation2D_hpp
#define qla_interpolation2D_hpp

#include <oh/libraryobject.hpp>
#include <qlo/interpolation.hpp>
#include <ql/math/matrix.hpp>

namespace QuantLibAddin {

    class Interpolation2D : public Extrapolator 
    {
      public:
          Interpolation2D(const std::string &interpolation2DType,
                        const std::vector<double>& x,
                        const std::vector<double>& y,
                        const QuantLib::Matrix& dataMatrix);
      private:
        std::vector<QuantLib::Real> x_, y_;
        QuantLib::Matrix dataMatrix_;
    };
    
}

#endif
