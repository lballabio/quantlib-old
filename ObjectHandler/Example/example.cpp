
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
#include <iostream>
#include <exception>
#include <objectfoo.hpp>

using namespace std;
using namespace ObjHandler;

int main() {
    try {
        // specify log file
        OH_LOGFILE("example.log");
        // also direct log messages to stdout
        OH_CONSOLE(1);
        OH_LOGMESSAGE("begin example program");
    } catch (const exception &e) {
        cout << "Unable to initialize logging: " << e.what() << endl;
        return 1;
    } catch (...) {
        cout << "Unable to initialize logging." << endl;
        return 1;
    }

    try {
        // construct some objects and store them in the object handler
        ArgStack f1Args;
        f1Args.push(string("abc"));
        f1Args.push(123);
        Properties f1Properties = OH_OBJECT_MAKE(ObjectFoo)("foo1", f1Args);

        ArgStack f2Args;
        f2Args.push(string("def"));
        f2Args.push(456);
        Properties f2Properties = OH_OBJECT_MAKE(ObjectFoo)("foo2", f2Args);

        // high level interrogation
        OH_LOGMESSAGE("high level interrogation - after constructor");
        OH_LOG_OBJECT("foo2");

        // update an object
        FOO_UPDATE("foo2", "ghi", 789);

        // high level interrogation
        OH_LOGMESSAGE("high level interrogation - after update");
        OH_LOG_OBJECT("foo2");

        // low-level interrogation
        OH_LOGMESSAGE("low-level interrogation - after FOO_UPDATE");
        boost::shared_ptr<ObjectFoo> const objectFoo =
            boost::dynamic_pointer_cast<ObjectFoo>
            (OH_OBJECT_GET("foo2"));
        boost::shared_ptr<Foo> foo =
            boost::static_pointer_cast<Foo>
            (objectFoo->getReference());
        OH_LOGMESSAGE("value of property s() of underlying foo = "
            + foo->s());

        OH_OBJECT_DELETE("foo2");
        OH_LOGMESSAGE("log all objects after deleting foo2:");
        OH_LOG_ALL_OBJECTS();

        OH_LOGMESSAGE("end example program");

        return 0;
    } catch (const exception &e) {
        ostringstream s;
        s << "Error: " << e.what();
        OH_LOGMESSAGE(s.str(), 1);
        return 1;
    } catch (...) {
        OH_LOGMESSAGE("Error", 1);
        return 1;
    }
}

