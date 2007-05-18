
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006, 2007 Eric Ehlers

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

#ifndef qla_interpolationsfactory_hpp
#define qla_interpolationsfactory_hpp

#include <oh/Enumerations/typefactory.hpp>
#include <ql/math/interpolations/interpolation2d.hpp>

namespace ObjectHandler {

    /* *** Linear 1D Interpolation *** */
    typedef const std::vector<double>::const_iterator dbl_itr;
    typedef boost::shared_ptr<QuantLib::Interpolation>(*InterpolationConstructor)(
        dbl_itr&, dbl_itr&, dbl_itr&);

    template<>
    class Create<boost::shared_ptr<QuantLib::Interpolation> > :
        private RegistryManager<QuantLib::Interpolation, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::Interpolation> operator() (
                const std::string& interpolationID,
                dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin) {
            InterpolationConstructor interpolationConstructor =
                reinterpret_cast<InterpolationConstructor>(getType(interpolationID));
            return interpolationConstructor(xBegin, xEnd, yBegin);
        }
        using RegistryManager<QuantLib::Interpolation, EnumClassRegistry>::registerType;
    };

    /* *** Interpolation2D *** */
    typedef boost::shared_ptr<QuantLib::Interpolation2D>(*Interpolation2DConstructor)(
        dbl_itr&, dbl_itr&, dbl_itr&, dbl_itr&, const QuantLib::Matrix&);

    template<>
    class Create<boost::shared_ptr<QuantLib::Interpolation2D> > :
        private RegistryManager<QuantLib::Interpolation2D, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::Interpolation2D> operator() (
                const std::string& interpolationID,
                dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin, dbl_itr& yEnd,
                const QuantLib::Matrix& zData) {
            Interpolation2DConstructor interpolation2DConstructor =
                reinterpret_cast<Interpolation2DConstructor>(getType(interpolationID));
            return interpolation2DConstructor(xBegin, xEnd, yBegin, yEnd, zData);
        }
        using RegistryManager<QuantLib::Interpolation2D, EnumClassRegistry>::registerType;
    };
 }

#endif

