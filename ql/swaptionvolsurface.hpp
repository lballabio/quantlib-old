
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software developed by the QuantLib Group; you can
 redistribute it and/or modify it under the terms of the QuantLib License;
 either version 1.0, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 QuantLib License for more details.

 You should have received a copy of the QuantLib License along with this
 program; if not, please email ferdinando@ametrano.net

 The QuantLib License is also available at http://quantlib.org/license.html
 The members of the QuantLib Group are listed in the QuantLib License
*/
/*! \file swaptionvolsurface.hpp
    \brief Swaption volatility surface

    \fullpath
    ql/%swaptionvolsurface.hpp
*/

// $Id$

#ifndef quantlib_swaption_volatility_surface_h
#define quantlib_swaption_volatility_surface_h

#include <ql/date.hpp>
#include <ql/handle.hpp>

namespace QuantLib {

    //! Swaption volatility surface
    /*! This class is purely abstract and defines the interface of concrete
        swaption volatility structures which will be derived from this one.
    */
    class SwaptionVolatilitySurface {
      public:
        virtual ~SwaptionVolatilitySurface() {}
        //! returns the volatility for a given starting date and length
        virtual Rate vol(const Date& start, Time length) const = 0;
    };

    //! Swaption volatility surface with an added spread
    /*! This surface will remain linked to the original surface, i.e.,
        any changes in the latter will be reflected in this surface as well.
    */
    class SpreadedSwaptionVolatilitySurface
    : public SwaptionVolatilitySurface {
      public:
        SpreadedSwaptionVolatilitySurface(
            const Handle<SwaptionVolatilitySurface>&, Spread spread);
        //! volatility of the original surface plus the given spread
        Rate vol(const Date& start, Time length) const;
      private:
        Handle<SwaptionVolatilitySurface> theOriginalSurface;
        Spread theSpread;
    };


    // inline definitions

    inline
    SpreadedSwaptionVolatilitySurface::SpreadedSwaptionVolatilitySurface(
        const Handle<SwaptionVolatilitySurface>& h, Spread spread)
    : theOriginalSurface(h), theSpread(spread) {}

    inline Rate SpreadedSwaptionVolatilitySurface::vol(const Date& start,
        Time length) const {
            return theOriginalSurface->vol(start,length)+theSpread;
    }

}


#endif
