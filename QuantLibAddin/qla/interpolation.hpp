
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

#ifndef qla_interpolation_hpp
#define qla_interpolation_cpp

#include <oh/objhandler.hpp>
#include <ql/Math/cubicspline.hpp>

namespace QuantLibAddin {
        
    typedef std::vector<std::vector<double> > double_matrix;
    
    class Interpolation : public ObjHandler::Object {
      public:
        Interpolation(const bool& allowExtrapolation)
        : allowExtrapolation_(allowExtrapolation) { }
        
        virtual boost::shared_ptr<QuantLib::Interpolation>
        interpolationFactory(std::vector<double>::const_iterator xBegin,
                             std::vector<double>::const_iterator xEnd,
                             std::vector<double>::const_iterator yBegin) const = 0;
        
        double_matrix interpolate(const double_matrix& xArray,
                                  const double_matrix& yArray,
                                  const double_matrix& xValues) const;
        
        virtual boost::shared_ptr<void> getReference() const {
            return boost::shared_ptr<void>();
        }
        
      protected:
        bool allowExtrapolation_;
    };

    class BackwardFlatInterpolation : public Interpolation {
      public:
        BackwardFlatInterpolation(const bool& allowExtrapolation)
        : Interpolation(allowExtrapolation) { }
        
        virtual boost::shared_ptr<QuantLib::Interpolation>
        interpolationFactory(std::vector<double>::const_iterator xBegin,
                             std::vector<double>::const_iterator xEnd,
                             std::vector<double>::const_iterator yBegin) const;
    };

    class ForwardFlatInterpolation : public Interpolation {
      public:
        ForwardFlatInterpolation(const bool& allowExtrapolation)
        : Interpolation(allowExtrapolation) { }
        
        virtual boost::shared_ptr<QuantLib::Interpolation>
        interpolationFactory(std::vector<double>::const_iterator xBegin,
                             std::vector<double>::const_iterator xEnd,
                             std::vector<double>::const_iterator yBegin) const;
    };
    
    class LinearInterpolation : public Interpolation {
      public:
        LinearInterpolation(const bool& allowExtrapolation)
        : Interpolation(allowExtrapolation) { }
        
        virtual boost::shared_ptr<QuantLib::Interpolation>
        interpolationFactory(std::vector<double>::const_iterator xBegin,
                             std::vector<double>::const_iterator xEnd,
                             std::vector<double>::const_iterator yBegin) const;
    };
    
    class CubicSplineInterpolation : public Interpolation {
      public:
        CubicSplineInterpolation(
            const bool&        allowExtrapolation,
            const std::string& leftConditionType,
            const double&      leftConditionValue,
            const std::string& rightConditionType,
            const double&      rightConditionValue,
            const bool&        monotonicityConstraint);
        
        virtual boost::shared_ptr<QuantLib::Interpolation>
        interpolationFactory(std::vector<double>::const_iterator xBegin,
                             std::vector<double>::const_iterator xEnd,
                             std::vector<double>::const_iterator yBegin) const;
        
      private:
        QuantLib::CubicSpline::BoundaryCondition leftConditionType_;
        QuantLib::Real leftConditionValue_;
        QuantLib::CubicSpline::BoundaryCondition rightConditionType_;
        QuantLib::Real rightConditionValue_;
        bool monotonicityConstraint_;
    };
    
}

#endif

