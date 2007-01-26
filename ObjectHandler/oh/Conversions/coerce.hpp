
/*
 Copyright (C) 2006 Eric Ehlers

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

#ifndef oh_conversions_coerce_hpp
#define oh_conversions_coerce_hpp

#include <oh/exception.hpp>
#include <sstream>

namespace ObjHandler {

    template <class TypeIn, class TypeOut>
    class Coerce {
    public:

        TypeOut operator()(const TypeIn &in) {
            TypeOut out;
            for (Conversion *conversion = getConversions();
                    *conversion; ++conversion) {
                if ((*conversion)(in, out)) 
                    return out;
            }

            OH_FAIL("Unable to coerce value to type " 
                << typeid(TypeOut).name());
        }

        TypeOut operator()(const TypeIn &in, const TypeOut &defaultValue) {
            if (inputMissing(in)) {
                return defaultValue;
            } else {
                return this->operator()(in);
            }
        }

    protected:
        typedef bool (*Conversion)(const TypeIn&, TypeOut&);
        virtual Conversion *getConversions() = 0;
        virtual bool inputMissing(const TypeIn&) { return false; }
    };

}

#endif
