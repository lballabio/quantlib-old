
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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qla/config.hpp>
#endif
#include <qla/qlatest.hpp>

#define FIELD_SCALAR_LONG       "SCALAR_LONG"
#define FIELD_VECTOR_LONG       "VECTOR_LONG"
#define FIELD_MATRIX_LONG       "MATRIX_LONG"
#define FIELD_SCALAR_DOUBLE     "SCALAR_DOUBLE"
#define FIELD_VECTOR_DOUBLE     "VECTOR_DOUBLE"
#define FIELD_MATRIX_DOUBLE     "MATRIX_DOUBLE"
#define FIELD_SCALAR_BOOL       "SCALAR_BOOL"
#define FIELD_VECTOR_BOOL       "VECTOR_BOOL"
#define FIELD_MATRIX_BOOL       "MATRIX_BOOL"
#define FIELD_SCALAR_STRING     "SCALAR_STRING"
#define FIELD_VECTOR_STRING     "VECTOR_STRING"
#define FIELD_MATRIX_STRING     "MATRIX_STRING"
#define FIELD_SCALAR_ANY        "SCALAR_ANY"
#define FIELD_VECTOR_ANY        "VECTOR_ANY"
#define FIELD_MATRIX_ANY        "MATRIX_ANY"

#define IDX_SCALAR_LONG         0
#define IDX_VECTOR_LONG         1
#define IDX_MATRIX_LONG         2
#define IDX_SCALAR_DOUBLE       3
#define IDX_VECTOR_DOUBLE       4
#define IDX_MATRIX_DOUBLE       5
#define IDX_SCALAR_BOOL         6
#define IDX_VECTOR_BOOL         7
#define IDX_MATRIX_BOOL         8
#define IDX_SCALAR_STRING       9
#define IDX_VECTOR_STRING       10
#define IDX_MATRIX_STRING       11
#define IDX_SCALAR_ANY          12
#define IDX_VECTOR_ANY          13
#define IDX_MATRIX_ANY          14

namespace QuantLibAddin {

    QlaTest::QlaTest(ObjHandler::ArgumentStack &arguments) {

        // any

        std::vector < std::vector < boost::any > > matrixAny
            = OH_POP_ARGUMENT(std::vector < std::vector < boost::any > >, arguments);
        std::vector < boost::any > vectorAny
            = OH_POP_ARGUMENT(std::vector < boost::any >, arguments);
        boost::any scalarAny            = arguments.top();
        arguments.pop();

        // string

        std::vector < std::vector < std::string > > matrixString
            = OH_POP_ARGUMENT(std::vector < std::vector < std::string > >, arguments);
        std::vector < std::string > vectorString
            = OH_POP_ARGUMENT(std::vector < std::string >, arguments);
        std::string scalarString        = OH_POP_ARGUMENT(std::string, arguments);

        // bool

        std::vector < std::vector < bool > > matrixBool
            = OH_POP_ARGUMENT(std::vector < std::vector < bool > >, arguments);
        std::vector < bool > vectorBool
            = OH_POP_ARGUMENT(std::vector < bool >, arguments);
        bool scalarBool                 = OH_POP_ARGUMENT(bool, arguments);

        // double

        std::vector < std::vector < double > > matrixDouble
            = OH_POP_ARGUMENT(std::vector < std::vector < double > >, arguments);
        std::vector < double > vectorDouble
            = OH_POP_ARGUMENT(std::vector < double >, arguments);
        double scalarDouble             = OH_POP_ARGUMENT(double, arguments);

        // long

        std::vector < std::vector < long > > matrixLong
            = OH_POP_ARGUMENT(std::vector < std::vector < long > >, arguments);
        std::vector < long > vectorLong
            = OH_POP_ARGUMENT(std::vector < long >, arguments);
        long scalarLong                 = OH_POP_ARGUMENT(long, arguments);

        // any_ptr

        ObjHandler::any_ptr anyScalarLong(new boost::any(scalarLong));
        ObjHandler::any_ptr anyVectorLong(new boost::any(vectorLong));
        ObjHandler::any_ptr anyMatrixLong(new boost::any(matrixLong));
        ObjHandler::any_ptr anyScalarDouble(new boost::any(scalarDouble));
        ObjHandler::any_ptr anyVectorDouble(new boost::any(vectorDouble));
        ObjHandler::any_ptr anyMatrixDouble(new boost::any(matrixDouble));
        ObjHandler::any_ptr anyScalarBool(new boost::any(scalarBool));
        ObjHandler::any_ptr anyVectorBool(new boost::any(vectorBool));
        ObjHandler::any_ptr anyMatrixBool(new boost::any(matrixBool));
        ObjHandler::any_ptr anyScalarString(new boost::any(scalarString));
        ObjHandler::any_ptr anyVectorString(new boost::any(vectorString));
        ObjHandler::any_ptr anyMatrixString(new boost::any(matrixString));
        ObjHandler::any_ptr anyScalarAny(new boost::any(scalarAny));
        ObjHandler::any_ptr anyVectorAny(new boost::any(vectorAny));
        ObjHandler::any_ptr anyMatrixAny(new boost::any(matrixAny));

        // ObjectProperty

        ObjHandler::ObjectProperty propertyScalarLong(FIELD_SCALAR_LONG, anyScalarLong);
        ObjHandler::ObjectProperty propertyVectorLong(FIELD_VECTOR_LONG, anyVectorLong);
        ObjHandler::ObjectProperty propertyMatrixLong(FIELD_MATRIX_LONG, anyMatrixLong);
        ObjHandler::ObjectProperty propertyScalarDouble(FIELD_SCALAR_DOUBLE, anyScalarDouble);
        ObjHandler::ObjectProperty propertyVectorDouble(FIELD_VECTOR_DOUBLE, anyVectorDouble);
        ObjHandler::ObjectProperty propertyMatrixDouble(FIELD_MATRIX_DOUBLE, anyMatrixDouble);
        ObjHandler::ObjectProperty propertyScalarBool(FIELD_SCALAR_BOOL, anyScalarBool);
        ObjHandler::ObjectProperty propertyVectorBool(FIELD_VECTOR_BOOL, anyVectorBool);
        ObjHandler::ObjectProperty propertyMatrixBool(FIELD_MATRIX_BOOL, anyMatrixBool);
        ObjHandler::ObjectProperty propertyScalarString(FIELD_SCALAR_STRING, anyScalarString);
        ObjHandler::ObjectProperty propertyVectorString(FIELD_VECTOR_STRING, anyVectorString);
        ObjHandler::ObjectProperty propertyMatrixString(FIELD_MATRIX_STRING, anyMatrixString);
        ObjHandler::ObjectProperty propertyScalarAny(FIELD_SCALAR_ANY, anyScalarAny);
        ObjHandler::ObjectProperty propertyVectorAny(FIELD_VECTOR_ANY, anyVectorAny);
        ObjHandler::ObjectProperty propertyMatrixAny(FIELD_MATRIX_ANY, anyMatrixAny);

        // properties_

        properties_.push_back(propertyScalarLong);
        properties_.push_back(propertyVectorLong);
        properties_.push_back(propertyMatrixLong);
        properties_.push_back(propertyScalarDouble);
        properties_.push_back(propertyVectorDouble);
        properties_.push_back(propertyMatrixDouble);
        properties_.push_back(propertyScalarBool);
        properties_.push_back(propertyVectorBool);
        properties_.push_back(propertyMatrixBool);
        properties_.push_back(propertyScalarString);
        properties_.push_back(propertyVectorString);
        properties_.push_back(propertyMatrixString);
        properties_.push_back(propertyScalarAny);
        properties_.push_back(propertyVectorAny);
        properties_.push_back(propertyMatrixAny);

    }

    // long

    const long &QlaTest::echoScalarLong(const long &input) {
        *properties_[IDX_SCALAR_LONG]() = input;
        return input;
    }

    const std::vector < long > &QlaTest::echoVectorLong(const std::vector < long > &input) {
        *properties_[IDX_VECTOR_LONG]() = input;
        return input;
    }

    const std::vector < std::vector < long > > &QlaTest::echoMatrixLong(
            const std::vector < std::vector < long > > &input) {
        *properties_[IDX_MATRIX_LONG]() = input;
        return input;
    }

    // double

    const double &QlaTest::echoScalarDouble(const double &input) {
        *properties_[IDX_SCALAR_DOUBLE]() = input;
        return input;
    }

    const std::vector < double > &QlaTest::echoVectorDouble(const std::vector < double > &input) {
        *properties_[IDX_VECTOR_DOUBLE]() = input;
        return input;
    }

    const std::vector < std::vector < double > > &QlaTest::echoMatrixDouble(
            const std::vector < std::vector < double > > &input) {
        *properties_[IDX_MATRIX_DOUBLE]() = input;
        return input;
    }

    // bool

    const bool &QlaTest::echoScalarBool(const bool &input) {
        *properties_[IDX_SCALAR_BOOL]() = input;
        return input;
    }

    const std::vector < bool > &QlaTest::echoVectorBool(const std::vector < bool > &input) {
        *properties_[IDX_VECTOR_BOOL]() = input;
        return input;
    }

    const std::vector < std::vector < bool > > &QlaTest::echoMatrixBool(
            const std::vector < std::vector < bool > > &input) {
        *properties_[IDX_MATRIX_BOOL]() = input;
        return input;
    }

    // string

    const std::string &QlaTest::echoScalarString(const std::string &input) {
        *properties_[IDX_SCALAR_STRING]() = input;
        return input;
    }

    const std::vector < std::string > &QlaTest::echoVectorString(const std::vector < std::string > &input) {
        *properties_[IDX_VECTOR_STRING]() = input;
        return input;
    }

    const std::vector < std::vector < std::string > > &QlaTest::echoMatrixString(
            const std::vector < std::vector < std::string > > &input) {
        *properties_[IDX_MATRIX_STRING]() = input;
        return input;
    }

    // any

    const boost::any &QlaTest::echoScalarAny(const boost::any &input) {
        *properties_[IDX_SCALAR_ANY]() = input;
        return input;
    }

    const std::vector < boost::any > &QlaTest::echoVectorAny(const std::vector < boost::any > &input) {
        *properties_[IDX_VECTOR_ANY]() = input;
        return input;
    }

    const std::vector < std::vector < boost::any > > &QlaTest::echoMatrixAny(
            const std::vector < std::vector < boost::any > > &input) {
        *properties_[IDX_MATRIX_ANY]() = input;
        return input;
    }

}

