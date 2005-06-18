
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

#ifndef qla_test_hpp
#define qla_test_hpp

#include <oh/objhandler.hpp>

namespace QuantLibAddin {

    class QlaTest : public ObjHandler::Object {

    public:
        QlaTest(ObjHandler::ArgumentStack &args);

        const long &echoScalarLong(const long &input);
        const std::vector < long > &echoVectorLong(const std::vector < long > &input);
        const std::vector < std::vector < long > > &echoMatrixLong(
            const std::vector < std::vector < long > > &input);

        const double &echoScalarDouble(const double &input);
        const std::vector < double > &echoVectorDouble(const std::vector < double > &input);
        const std::vector < std::vector < double > > &echoMatrixDouble(
            const std::vector < std::vector < double > > &input);

        const bool &echoScalarBool(const bool &input);
        const std::vector < bool > &echoVectorBool(const std::vector < bool > &input);
        const std::vector < std::vector < bool > > &echoMatrixBool(
            const std::vector < std::vector < bool > > &input);

        const std::string &echoScalarString(const std::string &input);
        const std::vector < std::string > &echoVectorString(const std::vector < std::string > &input);
        const std::vector < std::vector < std::string > > &echoMatrixString(
            const std::vector < std::vector < std::string > > &input);

        const boost::any &echoScalarAny(const boost::any &input);
        const std::vector < boost::any > &echoVectorAny(const std::vector < boost::any > &input);
        const std::vector < std::vector < boost::any > > &echoMatrixAny(
            const std::vector < std::vector < boost::any > > &input);

        virtual boost::shared_ptr<void> getReference() const {
            return boost::shared_ptr<void>();
        }
    };

}

#endif


