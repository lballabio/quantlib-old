
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

/*! \file
    \brief InstanceNameXL class
*/

#ifndef oh_instancenamexl_hpp
#define oh_instancenamexl_hpp

#include <oh/object.hpp>

namespace ObjHandler {
    //! Excel specific behavior for Object instance names.
    /*! Link to calling cell is automatically maintained.
    */
    class InstanceNameXL : public Object::InstanceName {
    public:
        InstanceNameXL(const std::string &stubName);
        ~InstanceNameXL();
        virtual bool isValid();
    private:
        static int keyCount_;
        static std::string InstanceNameXL::getKeyCount();
    };

}

#endif
