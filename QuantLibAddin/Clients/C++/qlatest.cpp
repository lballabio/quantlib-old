
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

#include <qla/qladdindefines.hpp>
#include <qla/utilities.hpp>
#include <qla/qlatest.hpp>

#include <sstream>
#include <iostream>

using namespace std;
using namespace QuantLib;
using namespace ObjHandler;
using namespace QuantLibAddin;

int main() {
    try {
        OH_LOGFILE("quantlib.log");         // specify log file
        OH_CONSOLE(1);                      // log messages to stdout
        OH_LOG_MESSAGE("begin test program");
    } catch (const exception &e) {
        cout << "Unable to initialize logging: " << e.what() << endl;
        return 1;
    } catch (...) {
        cout << "Unable to initialize logging." << endl;
        return 1;
    }

    try {
        int i, j;
        OH_LOG_MESSAGE(QL_VERSION());
        OH_LOG_MESSAGE(QL_OH_VERSION());

        // format the inputs to the constructor

        // long

        long scalarLong = 0;
        vector < long > vectorLong;
        vectorLong.push_back(0);
        vectorLong.push_back(1);
        vectorLong.push_back(2);
        vector < vector < long > > matrixLong;
        for (i=0; i<3; i++) {
            vector < long > temp;
            for (j=0; j<3; j++)
                temp.push_back(i * 3 + j);
            matrixLong.push_back(temp);
        }

        // double

        double scalarDouble = 0.123;
        vector < double > vectorDouble;
        vectorDouble.push_back(0.123);
        vectorDouble.push_back(1.123);
        vectorDouble.push_back(2.123);
        vector < vector < double > > matrixDouble;
        for (i=0; i<3; i++) {
            vector < double > temp;
            for (j=0; j<3; j++)
                temp.push_back(i * 3 + j + 0.123);
            matrixDouble.push_back(temp);
        }

        // bool

        bool scalarBool = true;
        vector < bool > vectorBool;
        vectorBool.push_back(true);
        vectorBool.push_back(false);
        vectorBool.push_back(true);
        vector < vector < bool > > matrixBool;
        for (i=0; i<3; i++) {
            vector < bool > temp;
            for (j=0; j<3; j++)
                temp.push_back((i * 3 + j) % 2 == 0);
            matrixBool.push_back(temp);
        }

        // string

        string scalarString("string0");
        vector < string > vectorString;
        vectorString.push_back("string0");
        vectorString.push_back("string1");
        vectorString.push_back("string2");
        vector < vector < string > > matrixString;
        for (i=0; i<3; i++) {
            vector < string > temp;
            for (j=0; j<3; j++) {
                int idx = i * 3 + j;
                ostringstream msg;
                msg << "string" << idx;
                temp.push_back(msg.str());
            }
            matrixString.push_back(temp);
        }

        // any

        boost::any scalarAny(0L);
        vector < boost::any > vectorAny;
        vectorAny.push_back(0L);
        vectorAny.push_back(1.123);
        vectorAny.push_back(string("string2"));
        vector < vector < boost::any > > matrixAny;
        for (i=0; i<3; i++) {
            vector < boost::any > temp;
            for (j=0; j<3; j++) {
                temp.push_back(static_cast < long > (i * 3 + j));
            }
            matrixAny.push_back(temp);
        }

        ArgumentStack testArgs;
        testArgs.push(scalarLong);
        testArgs.push(vectorLong);
        testArgs.push(matrixLong);
        testArgs.push(scalarDouble);
        testArgs.push(vectorDouble);
        testArgs.push(matrixDouble);
        testArgs.push(scalarBool);
        testArgs.push(vectorBool);
        testArgs.push(matrixBool);
        testArgs.push(scalarString);
        testArgs.push(vectorString);
        testArgs.push(matrixString);
        testArgs.push(scalarAny);
        testArgs.push(vectorAny);
        testArgs.push(matrixAny);

        // call the constructor

        Properties testProperties =
            OH_MAKE_OBJECT(QuantLibAddin::QlaTest, "my_qlatest", testArgs);

        // interrogate the object

        OH_LOG_MESSAGE("High-level interrogation: after construction");
        OH_LOG_OBJECT("my_qlatest");

        boost::shared_ptr<QuantLibAddin::QlaTest> qlaTest =
            OH_GET_OBJECT(QuantLibAddin::QlaTest, "my_qlatest");
        if (!qlaTest)
            QL_FAIL("Error retrieving object my_qlatest");

        // test the member functions

        // long

        long scalarLongOut = qlaTest->echoScalarLong(5);

        ostringstream msg;
        msg << "scalarOut: " << scalarLongOut;
        OH_LOG_MESSAGE(msg.str());

        vector < long > vectorLongIn;
        vectorLongIn.push_back(5);
        vectorLongIn.push_back(6);
        vectorLongIn.push_back(7);

        vector < long > vectorLongOut = qlaTest->echoVectorLong(vectorLongIn);
        msg.str("");
        msg << "vectorLongOut: " << vectorLongOut;
        OH_LOG_MESSAGE(msg.str());

        vector < vector < long > > matrixLongIn;
        for (i=0; i<3; i++) {
            vector < long > temp;
            for (j=0; j<3; j++) {
                temp.push_back(i * 3 + j + 100);
            }
            matrixLongIn.push_back(temp);
        }

        vector < vector < long > > matrixLongOut = qlaTest->echoMatrixLong(matrixLongIn);

        msg.str("");
        msg << "matrixLongOut:";
        OH_LOG_MESSAGE(msg.str());

        for (vector < vector < long > >::const_iterator it = matrixLongOut.begin();
                it != matrixLongOut.end(); it++) {
            vector < long > row = *it;
            msg.str("");
            msg << row;
            OH_LOG_MESSAGE(msg.str());
        }

        // double

        double scalarDoubleOut = qlaTest->echoScalarDouble(5.123);

        msg.str("");
        msg << "scalarOut: " << scalarDoubleOut;
        OH_LOG_MESSAGE(msg.str());

        vector < double > vectorDoubleIn;
        vectorDoubleIn.push_back(5.123);
        vectorDoubleIn.push_back(6.123);
        vectorDoubleIn.push_back(7.123);

        vector < double > vectorDoubleOut = qlaTest->echoVectorDouble(vectorDoubleIn);
        msg.str("");
        msg << "vectorDoubleOut: " << vectorDoubleOut;
        OH_LOG_MESSAGE(msg.str());

        vector < vector < double > > matrixDoubleIn;
        for (i=0; i<3; i++) {
            vector < double > temp;
            for (j=0; j<3; j++) {
                temp.push_back(i * 3 + j + 100.123);
            }
            matrixDoubleIn.push_back(temp);
        }

        vector < vector < double > > matrixDoubleOut = qlaTest->echoMatrixDouble(matrixDoubleIn);

        msg.str("");
        msg << "matrixDoubleOut:";
        OH_LOG_MESSAGE(msg.str());

        for (vector < vector < double > >::const_iterator it2 = matrixDoubleOut.begin();
                it2 != matrixDoubleOut.end(); it2++) {
            vector < double > row = *it2;
            msg.str("");
            msg << row;
            OH_LOG_MESSAGE(msg.str());
        }

        // bool

        bool scalarBoolOut = qlaTest->echoScalarBool(false);

        msg.str("");
        msg << "scalarOut: " << scalarBoolOut;
        OH_LOG_MESSAGE(msg.str());

        vector < bool > vectorBoolIn;
        vectorBoolIn.push_back(false);
        vectorBoolIn.push_back(true);
        vectorBoolIn.push_back(false);

        vector < bool > vectorBoolOut = qlaTest->echoVectorBool(vectorBoolIn);
        msg.str("");
        msg << "vectorBoolOut: " << vectorBoolOut;
        OH_LOG_MESSAGE(msg.str());

        vector < vector < bool > > matrixBoolIn;
        for (i=0; i<3; i++) {
            vector < bool > temp;
            for (j=0; j<3; j++) {
                temp.push_back((i * 3 + j) % 2 != 0);
            }
            matrixBoolIn.push_back(temp);
        }

        vector < vector < bool > > matrixBoolOut = qlaTest->echoMatrixBool(matrixBoolIn);

        msg.str("");
        msg << "matrixBoolOut:";
        OH_LOG_MESSAGE(msg.str());

        for (vector < vector < bool > >::const_iterator it3 = matrixBoolOut.begin();
                it3 != matrixBoolOut.end(); it3++) {
            vector < bool > row = *it3;
            msg.str("");
            msg << row;
            OH_LOG_MESSAGE(msg.str());
        }

        // string

        string scalarStringOut = qlaTest->echoScalarString("string5");

        msg.str("");
        msg << "scalarOut: " << scalarStringOut;
        OH_LOG_MESSAGE(msg.str());

        vector < string > vectorStringIn;
        vectorStringIn.push_back("string5");
        vectorStringIn.push_back("string6");
        vectorStringIn.push_back("string7");

        vector < string > vectorStringOut = qlaTest->echoVectorString(vectorStringIn);
        msg.str("");
        msg << "vectorStringOut: " << vectorStringOut;
        OH_LOG_MESSAGE(msg.str());

        vector < vector < string > > matrixStringIn;
        for (i=0; i<3; i++) {
            vector < string > temp;
            for (j=0; j<3; j++) {
                int idx = i * 3 + j + 100;
                ostringstream msg;
                msg << "string" << idx;
                temp.push_back(msg.str());
            }
            matrixStringIn.push_back(temp);
        }

        vector < vector < string > > matrixStringOut = qlaTest->echoMatrixString(matrixStringIn);

        msg.str("");
        msg << "matrixStringOut:";
        OH_LOG_MESSAGE(msg.str());

        for (vector < vector < string > >::const_iterator it4 = matrixStringOut.begin();
                it4 != matrixStringOut.end(); it4++) {
            vector < string > row = *it4;
            msg.str("");
            msg << row;
            OH_LOG_MESSAGE(msg.str());
        }

        // any

        boost::any scalarAnyOut = qlaTest->echoScalarAny(5L);

        msg.str("");
//        msg << "scalarOut: " << scalarAnyOut;
        OH_LOG_MESSAGE(msg.str());

        vector < boost::any > vectorAnyIn;
        vectorAnyIn.push_back(5L);
        vectorAnyIn.push_back(string("string6"));
        vectorAnyIn.push_back(7.123);

        vector < boost::any > vectorAnyOut = qlaTest->echoVectorAny(vectorAnyIn);
        msg.str("");
        msg << "vectorAnyOut: " << vectorAnyOut;
        OH_LOG_MESSAGE(msg.str());

        vector < vector < boost::any > > matrixAnyIn;
        for (i=0; i<3; i++) {
            vector < boost::any > temp;
            for (j=0; j<3; j++) {
                if (i==0) {
                    temp.push_back(i * 3 + j + 100);
                } else if (i==1) {
                    int idx = i * 3 + j + 100;
                    ostringstream msg;
                    msg << "string" << idx;
                    temp.push_back(msg.str());
                } else {
                    temp.push_back(i * 3 + j + 100.123);
                }
            }
            matrixAnyIn.push_back(temp);
        }

        vector < vector < boost::any > > matrixAnyOut = qlaTest->echoMatrixAny(matrixAnyIn);

        msg.str("");
        msg << "matrixAnyOut:";
        OH_LOG_MESSAGE(msg.str());

        for (vector < vector < boost::any > >::const_iterator it5 = matrixAnyOut.begin();
                it5 != matrixAnyOut.end(); it5++) {
            vector < boost::any > row = *it5;
            msg.str("");
            msg << row;
            OH_LOG_MESSAGE(msg.str());
        }

        OH_LOG_MESSAGE("end test program");
        return 0;
    } catch (const exception &e) {
        ostringstream s;
        s << "Error: " << e.what();
        OH_LOG_MESSAGE(s.str(), 1);
        return 1;
    } catch (...) {
        OH_LOG_MESSAGE("unknown error", 1);
        return 1;
    }
}

