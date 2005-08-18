
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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
    \brief a simple Exception class for ObjectHandler.
*/

#ifndef oh_exception_hpp
#define oh_exception_hpp

#include <oh/objhandlerdefines.hpp>
#include <exception>
#include <string>

namespace ObjHandler {

    //! Simple implementation of an exception.
    /*! Descended from std::exception,
        supports an error message string.
    */
    class DLL_API Exception : public std::exception {
        public:
            Exception(const std::string& message);
            ~Exception() throw() {}
            const char* what() const throw ();
        private:
            std::string message_;
    };
}
#endif

