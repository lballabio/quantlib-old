
/*!
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

#include <iostream>
#include <exception>
#include <interface.hpp>

using namespace std;

int main() {
    try {
        cout << "hi" << endl;

        // start ObjectHandler standard logging
        QL_LOGFILE("example.log");
        QL_LOGMESSAGE("hi");

        // construct some objects and store them in the object handler
        Properties w1Properties = WIDGET_MAKE("widget1", "abc", 123);
        Properties w2Properties = WIDGET_MAKE("widget2", "def", 456);

        // high level interrogation
        cout << endl << "high level interrogation - after constructor" << endl;
        Properties::const_iterator i;
        for (i = w2Properties.begin();
            i != w2Properties.end(); i++) {
            ObjectProperty property = *i;
            any_ptr any = property();
            cout << "property = " << property.name() << "\tvalue = " <<
                QL_ANY2STRING(any) << endl;
        }

        // update an object
        WIDGET_UPDATE("widget2", "ghi", 789);

        // high level interrogation
        cout << endl << "high level interrogation - after update" << endl;
        for (i = w2Properties.begin();
            i != w2Properties.end(); i++) {
            ObjectProperty property = *i;
            any_ptr any = property();
            cout << "property = " << property.name() << "\tvalue = " <<
                QL_ANY2STRING(any) << endl;
        }

        // low-level interrogation
        cout << endl << "low-level interrogation - after WIDGET_UPDATE" << endl;
        boost::shared_ptr<ObjectWidget> const objectWidget =
            boost::dynamic_pointer_cast<ObjectWidget>
            (ObjectHandler::instance().retrieveObject("widget2"));
        boost::shared_ptr<Widget> widget =
            boost::static_pointer_cast<Widget>
            (objectWidget->getReference());
        cout << "value of property s() of underlying widget = "
            << widget->s() << endl;

        QL_LOGMESSAGE("bye");
        cout << endl << "bye" << endl;
        return 0;
    } catch (const exception &e) {
        cout << "Error: " << e.what() << endl;
        QL_LOGMESSAGE("Error: " + string(e.what()));
        return 1;
    }
}

