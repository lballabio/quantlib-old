
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

#include <sstream>
#include <exception>
#include <interface.hpp>
#include <objectfoo.hpp>    // only required for low-level interrogation

using namespace std;

int main() {
    try {
        // specify log file
        QL_LOGFILE("example.log");
        // also direct log messages to stdout
        QL_CONSOLE(1);
        QL_LOGMESSAGE("begin example program");

        // construct some objects and store them in the object handler
        Properties f1Properties = QL_MAKE_OBJECT(ObjectFoo)(
            "foo1", "abc", 123);
        Properties f2Properties = QL_MAKE_OBJECT(ObjectFoo)(
            "foo2", "def", 456);

        // high level interrogation
        QL_LOGMESSAGE("high level interrogation - after constructor");
        Properties::const_iterator i;
        for (i = f2Properties.begin(); i != f2Properties.end(); i++) {
            ObjectProperty property = *i;
            ostringstream s;
            s << "property = " << property.name() << "\tvalue = " <<
                property();
            QL_LOGMESSAGE(s.str());
        }

        // update an object
        FOO_UPDATE("foo2", "ghi", 789);

        // high level interrogation
        QL_LOGMESSAGE("high level interrogation - after update");
        for (i = f2Properties.begin();
            i != f2Properties.end(); i++) {
            ObjectProperty property = *i;
            ostringstream s;
            s << "property = " << property.name() << "\tvalue = " <<
                property();
            QL_LOGMESSAGE(s.str());
        }

        // low-level interrogation
        QL_LOGMESSAGE("low-level interrogation - after FOO_UPDATE");
        boost::shared_ptr<ObjectFoo> const objectFoo =
            boost::dynamic_pointer_cast<ObjectFoo>
            (ObjectHandler::instance().retrieveObject("foo2"));
        boost::shared_ptr<Foo> foo =
            boost::static_pointer_cast<Foo>
            (objectFoo->getReference());
        QL_LOGMESSAGE("value of property s() of underlying foo = "
            + foo->s());

        QL_LOGMESSAGE("end example program");

        return 0;
    } catch (const exception &e) {
        ostringstream s;
        s << "Error: " << e.what();
        QL_LOGMESSAGE(s.str(), 1);
        return 1;
    }
}

