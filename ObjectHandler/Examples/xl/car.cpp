
/*!
 Copyright (C) 2004, 2005, 2006 Eric Ehlers

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

#include <car.hpp>
#include <iostream>
#include <sstream>

CarObject::CarObject(
        const int &wheelCount,
        const std::string &color) {
    car_ = boost::shared_ptr<Car>(new Car(wheelCount, color));
}

void CarObject::setSpeed(const int &speed) {
    car_->setSpeed(speed);
}

const int &CarObject::getSpeed() {
    return car_->getSpeed();
}

boost::shared_ptr<void> CarObject::getReference() const {
    return boost::static_pointer_cast<void>(car_);
}

const char* CarValueObject::mPropertyNames[] = {
    "wheelCount",
    "color"};

std::vector<std::string> CarValueObject::getPropertyNames() const {
#ifdef OBJHANDLER_PATCH_MSVC6
    std::vector<std::string> ret;
    const int max = sizeof(mPropertyNames)/sizeof(const char*);
    for (int i=0; i<max; i++) ret.push_back(mPropertyNames[i]);
    return ret;
#else
    return std::vector<std::string>(
        mPropertyNames, mPropertyNames + sizeof(mPropertyNames)/sizeof(const char*));
#endif
}

boost::any CarValueObject::getProperty(const std::string& name) const {
    if(name == "wheelCount") return wheelCount_;
    else if(name == "color") return color_;
    else 
        throw ObjHandler::Exception("Error: attempt to retrieve non-existent Property: '" + name + "'");
    return boost::any(); /* Dummy return - just to avoid stupid compiler warnings/errors */
}

