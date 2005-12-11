
/*
 Copyright (C) 2005 Eric Ehlers

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

#include <Addins/Excel/session.hpp>

#ifdef QL_ENABLE_SESSIONS

#include <ql/Patterns/singleton.hpp>
#include <ohxl/conversions.hpp>
#include <exception>
#include <sstream>

namespace QuantLibAddin {

    void Session::setSessionId() {

        // XLOPERs which might need freeing in the event of an exception

        XLOPER xCaller;
        XLOPER xReftext;

        try {

            // obtain reference to calling cell

            Excel(xlfCaller, &xCaller, 0);

            // convert reference to full address

            Excel(xlfReftext, &xReftext, 1, &xCaller);

            // get the name of the current book

            std::string callerAddress = ObjHandler::operToScalarString(&xReftext);
            std::string callerBook = bookFromAddress(callerAddress);

            // get session id from map, adding new entry if necessary

            std::map < std::string, QuantLib::Integer >::const_iterator result;
            result = sessionMap_.find(callerBook);
            if (result == sessionMap_.end()) {
                sessionMap_[callerBook] = ++sessionIdFountain_;
                sessionId_ = sessionIdFountain_;
            } else {
                sessionId_ = result->second;
            }

        } catch (const std::exception &e) {

            // free any memory that may have been allocated

            Excel(xlFree, 0, 2, &xCaller, &xReftext);

            // propagate the exception

            std::ostringstream err;
            err << "QuantLibAddin::Session::setSession: " << e.what();
            throw std::exception(err.str().c_str());
        }

    }

    // extract "BOOK.XLS" from "[BOOK.XLS]SHEET!R1C1"
    std::string Session::bookFromAddress(const std::string &address) {

        if (address[0] != '[') {
            std::ostringstream err;
            err << "error interpreting address " << address
                << " expected 1st character to be opening square bracket '['";
            throw exception(err.str().c_str());
        }

        int endBracket = address.find("]", 1);
        if (endBracket == std::string::npos) {
            std::ostringstream err;
            err << "error interpreting address " << address
                << " unable to locate closing square bracket ']'";
            throw exception(err.str().c_str());
        }

        std::string bookName = address.substr(1, endBracket - 1);
        return bookName;

    }

    const QuantLib::Integer &Session::getSessionId() {
        return sessionId_;
    }

}

QuantLib::Integer QuantLib::sessionId() {
    return QuantLibAddin::Session::instance().getSessionId();
}

#endif