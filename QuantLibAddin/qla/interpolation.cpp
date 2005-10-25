
/*
 Copyright (C) 2005 Aurelien Chanudet

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
    #include <qla/config.hpp>
#endif
#include <qla/interpolation.hpp>
#include <qla/typefactory.hpp>

#include <ql/Math/backwardflatinterpolation.hpp>
#include <ql/Math/forwardflatinterpolation.hpp>
#include <ql/Math/linearinterpolation.hpp>

namespace QuantLibAddin {
    
    double_matrix
    Interpolation::interpolate(const double_matrix& xArray,
                               const double_matrix& yArray,
                               const double_matrix& xValues) const {
        
        std::vector<QuantLib::Real> x;
        std::vector<QuantLib::Real> y;
        
        for (std::size_t ix=0 ; ix < xArray.size() ; ix++) {
            for (std::size_t jx=0 ; jx < xArray[ix].size() ; jx++) {
                x.push_back(xArray[ix][jx]);
            }
        }

        for (std::size_t iy=0 ; iy < yArray.size() ; iy++) {
            for (std::size_t jy=0 ; jy < yArray[iy].size() ; jy++) {
                y.push_back(yArray[iy][jy]);
            }
        }
        
        boost::shared_ptr<QuantLib::Interpolation> f =
            interpolationFactory(x.begin(), x.end(), y.begin());
        
        double_matrix result;
        
        for (std::size_t p=0 ; p < xValues.size() ; p++) {
            std::vector<QuantLib::Real> v;
            for (std::size_t q=0 ; q < xValues[p].size() ; q++) {
                v.push_back((*f)(xValues[p][q], allowExtrapolation_));
            }
            result.push_back(v);
        }
        
        return result;
    }
    
    boost::shared_ptr<QuantLib::Interpolation>
    LinearInterpolation::interpolationFactory(
            std::vector<double>::const_iterator xBegin,
            std::vector<double>::const_iterator xEnd,
            std::vector<double>::const_iterator yBegin) const {
        
        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::LinearInterpolation(xBegin, xEnd, yBegin));
    }
    
    boost::shared_ptr<QuantLib::Interpolation>
    BackwardFlatInterpolation::interpolationFactory(
            std::vector<double>::const_iterator xBegin,
            std::vector<double>::const_iterator xEnd,
            std::vector<double>::const_iterator yBegin) const {
        
        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::BackwardFlatInterpolation(xBegin, xEnd, yBegin));
    };
    
    boost::shared_ptr<QuantLib::Interpolation>
    ForwardFlatInterpolation::interpolationFactory(
            std::vector<double>::const_iterator xBegin,
            std::vector<double>::const_iterator xEnd,
            std::vector<double>::const_iterator yBegin) const {
        
        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::ForwardFlatInterpolation(xBegin, xEnd, yBegin));
    };
    
    CubicSplineInterpolation::CubicSplineInterpolation(
            const bool&        allowExtrapolation,
            const std::string& leftConditionTypeID,
            const double&      leftConditionValue,
            const std::string& rightConditionTypeID,
            const double&      rightConditionValue,
            const bool&        monotonicityConstraint)
    : Interpolation(allowExtrapolation),
      leftConditionValue_(leftConditionValue), rightConditionValue_(rightConditionValue),
      monotonicityConstraint_(monotonicityConstraint) {
        
        leftConditionType_ =
            Create<QuantLib::CubicSpline::BoundaryCondition>()(leftConditionTypeID);
        
        rightConditionType_ =
            Create<QuantLib::CubicSpline::BoundaryCondition>()(rightConditionTypeID);            
    }
    
    boost::shared_ptr<QuantLib::Interpolation>
    CubicSplineInterpolation::interpolationFactory(
            std::vector<double>::const_iterator xBegin,
            std::vector<double>::const_iterator xEnd,
            std::vector<double>::const_iterator yBegin) const {

        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::CubicSpline(xBegin, xEnd, yBegin,
                                      leftConditionType_, leftConditionValue_,
                                      rightConditionType_, rightConditionValue_,
                                      monotonicityConstraint_));
    }
    
}

