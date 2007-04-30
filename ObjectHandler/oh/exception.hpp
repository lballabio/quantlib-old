
/*
 Copyright (C) 2004, 2005, 2007 Eric Ehlers

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
    \brief Class Exception - A simple Exception class for ObjectHandler.
*/

#ifndef oh_exception_hpp
#define oh_exception_hpp

#include <oh/ohdefines.hpp>
#include <exception>
#include <string>
#include <sstream>

/*! \def OH_FAIL(message)
    Raises an exception with the given message.
*/

#define OH_FAIL(message) \
do { \
    std::ostringstream _oh_msg_stream; \
    _oh_msg_stream << message; \
    throw ObjectHandler::Exception(_oh_msg_stream.str()); \
} while (false)

/*! \def OH_REQUIRE(condition,message)
    Raises an exception if the given condition evaluates to false.
*/

#define OH_REQUIRE(condition,message) \
if (!(condition)) { \
    std::ostringstream _oh_msg_stream; \
    _oh_msg_stream << message; \
    throw ObjectHandler::Exception(_oh_msg_stream.str()); \
} else

namespace ObjectHandler {

    //! Simple implementation of an exception.
    /*! Descended from std::exception,
        supports an error message string.
    */
    class DLL_API Exception : public std::exception {
        public:
            Exception(const std::string& message): message_(message) {}
            ~Exception() throw() {}
            const char* what() const throw () { return message_.c_str(); }
        private:
            std::string message_;
    };
}

#endif

