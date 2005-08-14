
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
        setLogFile("example.log");
        // also direct log messages to stdout
        setConsole(1);
        logMessage("begin example program");
    } catch (const exception &e) {
        cout << "Unable to initialize logging: " << e.what() << endl;
        return 1;
    } catch (...) {
        cout << "Unable to initialize logging." << endl;
        return 1;
    }

    try {
        // construct some objects and store them in the object handler
        obj_ptr objectFoo1(new ObjectFoo("abc", 123));
        storeObject("foo1", objectFoo1);

        obj_ptr objectFoo2(new ObjectFoo("def", 456));
        storeObject("foo2", objectFoo2);

        // high level interrogation
        logMessage("high level interrogation - after constructor");
        logObject("foo2");

        // update an object
        updateFoo("foo2", "ghi", 789);

        // high level interrogation
        logMessage("high level interrogation - after update");
        logObject("foo2");

        // low-level interrogation
        logMessage("low-level interrogation - after updateFoo");
        boost::shared_ptr<ObjectFoo> const objectFoo =
            OH_GET_OBJECT(ObjectFoo, "foo2");
        boost::shared_ptr<Foo> foo = 
            OH_GET_REFERENCE(Foo, objectFoo);
        logMessage("value of property s() of underlying foo = "
            + foo->s());

        ObjectHandler::instance().deleteObject("foo2");
        logMessage("log all objects after deleting foo2:");
        logAllObjects();

        logMessage("end example program");

        return 0;
    } catch (const exception &e) {
        ostringstream s;
        s << "Error: " << e.what();
        logMessage(s.str(), 1);
        return 1;
    } catch (...) {
        logMessage("Error", 1);
        return 1;
    }
}

