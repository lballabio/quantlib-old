
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

/*! \file utilities.hpp
    \brief ObjectHandler utilities
*/

#ifndef oh_utilities_hpp
#define oh_utilities_hpp

#include <oh/object.hpp>

namespace ObjHandler {

    //! any_ptr ostream operator.
    /*! Write contents of boost::any
        to output stream.
    */
    std::ostream& operator<<(std::ostream& out, const any_ptr& a);
    //! Properties ostream operator.
    /*! Write contents of Object Property vector
        to output stream.
    */
    std::ostream& operator<<(std::ostream& out, const Properties &p);
    //! Write Object with given handle to log file.
    /*! Writes a warning message to log file
        if no object is found with given handle.
    */
    void logObject(const std::string &handle);
    //! Write all Objects to log file.
    /*! Takes no action if ObjectHandler
        repository is empty.
    */
    void logAllObjects();

}

#endif

