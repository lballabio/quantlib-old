
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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
#include <ql/Utilities/strings.hpp>
#include <oh/exception.hpp>
#include <ohxl/conversions.hpp>
#include <ohxl/functioncall.hpp>
#include <exception>
#include <sstream>
#include <boost/regex.hpp>

namespace QuantLibAddin {

    void Session::setSessionId() {

        // get the name of the current book

        const XLOPER *xReftext = ObjHandler::FunctionCall::instance().getCallerAddress();
        std::string callerAddress;
        ObjHandler::operToScalar(callerAddress, *xReftext);
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

    }

    // Accept cell address in one of the following formats (case insensitive):
    // [BOOK.XLS]SHEET!R1C1     (standard)
    // '[BOOK.XLS]SHEET X'!R1C1 (where sheet name contains spaces)
    // BOOK.XLS!R1C1            (book w/single sheet having same name as book)
    // and extract substring "BOOK.XLS"
    // NB: We don't test for a match on the ".XLS" suffix as Excel omits this
    //     suffix for a new book that hasn't yet been saved.

    std::string Session::bookFromAddress(const std::string &cellAddress) {
        static const boost::regex REGEX_BOOK_STANDARD("\\[(.*)\\].*!.*", 
            boost::regex_constants::icase);
        static const boost::regex REGEX_BOOK_SPACE("'\\[(.*)\\].*'!.*", 
            boost::regex_constants::icase);
        static const boost::regex REGEX_BOOK_NOSHEET("(.*)!.*", 
            boost::regex_constants::icase);
        boost::smatch matchResult;
        if ((regex_match(cellAddress, matchResult, REGEX_BOOK_STANDARD))
        || (regex_match(cellAddress, matchResult, REGEX_BOOK_SPACE))
        || (regex_match(cellAddress, matchResult, REGEX_BOOK_NOSHEET))) {
            return matchResult.str(1);
        } else {
            std::ostringstream err;
            err << "unable to interpret cell address: " << cellAddress <<
                " because it is not in any of the three expected formats: "
                "1) [BOOK.XLS]SHEET!R1C1 "
                "2) '[BOOK.XLS]SHEET'!R1C1 "
                "3) BOOK.XLS!R1C1";
            throw ObjHandler::Exception(err.str());
        }
    }

    const QuantLib::Integer &Session::getSessionId() {
        return sessionId_;
    }

}

QuantLib::Integer QuantLib::sessionId() {
    return QuantLibAddin::Session::instance().getSessionId();
}

#endif
