
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

#ifndef car_hpp
#define car_hpp

#include <oh/objhandler.hpp>
#include <oh/valueobject.hpp>

class Car {
public:
    Car(const unsigned int &wheelCount,
        const std::string &color,
        const int &speed = 0)
        : wheelCount_(wheelCount), color_(color), speed_(speed) {}
    void setSpeed(const int &speed) {
        speed_ = speed;
    }
    const int &getSpeed() {
        return speed_;
    }
private:
    unsigned int wheelCount_;
    std::string color_;
    int speed_;
};

class CarObject : public ObjHandler::Object {
public:
    CarObject(
        const unsigned int &wheelCount,
        const std::string &color);
    void setSpeed(const int &speed);
    const int &getSpeed();
    virtual boost::shared_ptr<void> getReference() const;
private:
    boost::shared_ptr<Car> car_;
};

typedef boost::shared_ptr<CarObject> CarObjectPtr;

class CarValueObject : public ObjHandler::ValueObject {
public:
    CarValueObject(
        const unsigned int &wheelCount,
        const std::string &color) 
        : wheelCount_(wheelCount), color_(color) {}
    std::vector<std::string> getPropertyNames() const;
    const boost::any getProperty(const std::string& name) const;
protected:
    static const char* mPropertyNames[];
    unsigned int wheelCount_;
    std::string color_;
};

#endif

