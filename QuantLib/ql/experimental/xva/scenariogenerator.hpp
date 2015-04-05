/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/*! \file scenariogenerator.hpp
    \brief base class for scenario generators
*/

#ifndef quantlib_xva_scenariogenerator_hpp
#define quantlib_xva_scenariogenerator_hpp

#include <ql/qldefines.hpp>
#include <ql/time/date.hpp>
#include <ql/time/calendars/nullcalendar.hpp>
#include <ql/time/daycounters/actual365fixed.hpp>
#include <ql/settings.hpp>

namespace QuantLib {

template <class State> class ScenarioGenerator {
  public:
    // Advance the horizon date by the required step.
    // The actual generator may produce a horizon date that
    // is not matching the step, the actual new horizon date
    // is returned. If a null date is returned, no more
    // scneario dates are available.
    virtual const Date advance(const Period &suggestedStep = 1 * Days);

    // Reset the horizon date and start a new path.
    // If no more path can be generated false is returned.
    virtual bool nextPath();

    // Return the current horizon date (or null if invalid)
    virtual const Date horizonDate() const;

    // Return the current path number (or null if invalid)
    virtual const Size pathNumber() const;

    // return the current state of the world.
    virtual const boost::shared_ptr<State> state() const = 0;

    // return the time of the scenario (or null if invalid)
    virtual const Time time() const;

  protected:
    ScenarioGenerator(const Calendar &calendar = NullCalendar(),
                      const DayCounter &dc = Actual365Fixed())
        : calendar_(calendar), dc_(dc), pathNumber_(0), validPath_(true),
          validHorizonDate_(true) {
        resetHorizonDate();
        baseDate_ = horizonDate_;
    }
    virtual ~ScenarioGenerator() {}

    // reset the horizon date to the start date of the generator,
    // which is normally the global evaluation date
    virtual const Date resetHorizonDate();

    Calendar calendar_;
    DayCounter dc_;
    Date horizonDate_, baseDate_;
    Size pathNumber_;
    bool validPath_, validHorizonDate_;
};

// inline

template <class State> inline bool ScenarioGenerator<State>::nextPath() {
    pathNumber_++;
    resetHorizonDate();
    return true;
}

template <class State>
inline const Date ScenarioGenerator<State>::advance(const Period &p) {
    if (horizonDate_ >= Date::maxDate())
        validHorizonDate_ = false;
    if (validHorizonDate_)
        horizonDate_ = calendar_.advance(horizonDate_, p);
    return horizonDate();
}

template <class State>
inline const Date ScenarioGenerator<State>::horizonDate() const {
    return validHorizonDate_ ? horizonDate_ : Null<Date>();
}

template <class State>
inline const Size ScenarioGenerator<State>::pathNumber() const {
    return validPath_ ? pathNumber_ : Null<Size>();
}

template <class State>
inline const Time ScenarioGenerator<State>::time() const {
    return validHorizonDate_ ? dc_.yearFraction(baseDate_, horizonDate_)
                             : Null<Real>();
}

template <class State>
inline const Date ScenarioGenerator<State>::resetHorizonDate() {
    horizonDate_ = Settings::instance().evaluationDate();
    validHorizonDate_ = true;
    return horizonDate();
}

} // namespace QuantLib

#endif
