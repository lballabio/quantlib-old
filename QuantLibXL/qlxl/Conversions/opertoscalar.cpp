
/*
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

#include <qlxl/Conversions/opertoscalar.hpp>
#include <qlo/Conversions/coercehandle.hpp>
#include <qlo/Conversions/coerceobject.hpp>
#include <qlo/quotes.hpp>
#include <ql/utilities/dataparsers.hpp>
#include <ql/time/period.hpp>
#include <ohxl/Conversions/opertoscalar.hpp>
#include <oh/object.hpp>
#include <oh/objecthandler.hpp>

namespace ObjectHandler {

    void operToScalar(const OPER &xScalar, QuantLib::Date &ret) {
        long dateNum;
        operToScalar(xScalar, dateNum);
        ret = QuantLib::Date(dateNum);
    }

    void operToScalar(const OPER &xScalar, QuantLib::Period &ret) {
        std::string id;
        operToScalar(xScalar, id);
        cppToLibrary(id, ret);
    }

    void operToScalar(const OPER &xScalar, QuantLib::Size &ret) {
        long sizeNum;
        operToScalar(xScalar, sizeNum);
        cppToLibrary(sizeNum, ret);
    }

    void operToScalar(const OPER &xScalar, QuantLib::RelinkableHandle<QuantLib::Quote> &ret) {    
        if (xScalar.xltype & xltypeNum) {
            ret.linkTo(boost::shared_ptr<QuantLib::Quote>(new QuantLib::SimpleQuote(xScalar.val.num)));
        } else if (xScalar.xltype & xltypeStr) {
            std::string id;
            operToScalar(xScalar, id);
            OH_GET_OBJECT(temp, id, ObjectHandler::Object)
            ret = CoerceHandle<QuantLibAddin::Quote, QuantLib::Quote>()(temp);
        } else {
            OH_FAIL("unable to convert input value to QuantLib::RelinkableHandle<QuantLib::Quote>");
        }
    }

    void operToScalar(const OPER &xScalar, boost::shared_ptr<QuantLib::Quote> &ret) {
        if (xScalar.xltype & xltypeNum) {
            ret = boost::shared_ptr<QuantLib::Quote>(new QuantLib::SimpleQuote(xScalar.val.num));
        } else if (xScalar.xltype & xltypeStr) {
            std::string id;
            operToScalar(xScalar, id);
            OH_GET_OBJECT(temp, id, ObjectHandler::Object)
            ret = CoerceObject<QuantLibAddin::Quote, QuantLib::Quote>()(temp);
        } else {
            OH_FAIL("unable to convert input value to QuantLib::Quote");
        }
    }

    //void cppToLibrary(const long &in, QuantLib::Date &ret) {
    //    ret = QuantLib::Date(dateNum);
    //}

    void cppToLibrary(const std::string &in, QuantLib::Period &ret) {
        ret = QuantLib::PeriodParser::parse(in);
    }

    void cppToLibrary(const long &in, QuantLib::Size &ret) {
        ret = QuantLib::Size(in);
    }

}
