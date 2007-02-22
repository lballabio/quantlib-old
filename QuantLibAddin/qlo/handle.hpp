
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

#ifndef qla_handle_hpp
#define qla_handle_hpp

#include <oh/object.hpp>
#include <ql/handle.hpp>

namespace QuantLibAddin {

    template <class T>
    class Handle : public ObjHandler::Object {
    public:
        Handle(const boost::shared_ptr<T> &observable) {
            if (observable)
                handle_.linkTo(observable);
        }
        void linkTo(const boost::shared_ptr<T> &observable) {
            QL_REQUIRE(observable, "Error relinking handle of type "
                << typeid(this).name() << " - input object is null");
            handle_.linkTo(observable);
        }
        QuantLib::RelinkableHandle<T> getHandle() {
            return handle_;
        }
    private:
        QuantLib::RelinkableHandle<T> handle_;
    };

}

#endif

