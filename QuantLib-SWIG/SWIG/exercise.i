
/*
 Copyright (C) 2003 StatPro Italia srl

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

#ifndef quantlib_exercise_i
#define quantlib_exercise_i

%include common.i

// exercise conditions

%{
using QuantLib::Exercise;
typedef Exercise::Type ExerciseType;

Exercise::Type exerciseTypeFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "e" || s == "european")
        return Exercise::European;
    else if (s == "a" || s == "american")
        return Exercise::American;
    else if (s == "b" || s == "bermudan")
        return Exercise::Bermudan;
    else
        throw Error("unknown exercise type: "+s);
}

std::string exerciseTypeToString(Exercise::Type t) {
    switch (t) {
      case Exercise::European:
        return "European";
      case Exercise::American:
        return "American";
      case Exercise::Bermudan:
        return "Bermudan";
      default:
        throw Error("unknown exercise type");
    }
}
%}

MapToString(ExerciseType,exerciseTypeFromString,exerciseTypeToString);

%ignore Exercise;
class Exercise {
  public:
    ExerciseType type() const;
    std::vector<Date> dates() const;
};

%template(Exercise) Handle<Exercise>;

%{
using QuantLib::EuropeanExercise;
using QuantLib::AmericanExercise;
using QuantLib::BermudanExercise;
typedef Handle<Exercise> EuropeanExerciseHandle;
typedef Handle<Exercise> AmericanExerciseHandle;
typedef Handle<Exercise> BermudanExerciseHandle;
%}

%rename(EuropeanExercise) EuropeanExerciseHandle;
class EuropeanExerciseHandle : public Handle<Exercise> {
  public:
    %extend {
        EuropeanExerciseHandle(const Date& date) {
            return new EuropeanExerciseHandle(new EuropeanExercise(date));
        }
    }
};

%rename(AmericanExercise) AmericanExerciseHandle;
class AmericanExerciseHandle : public Handle<Exercise> {
  public:
    %extend {
        AmericanExerciseHandle(const Date& earliestDate, 
                               const Date& latestDate,
                               bool payoffAtExpiry = false) {
            return new AmericanExerciseHandle(
                                        new AmericanExercise(earliestDate,
                                                             latestDate,
                                                             payoffAtExpiry));
        }
    }
};

%rename(BermudanExercise) BermudanExerciseHandle;
class BermudanExerciseHandle : public Handle<Exercise> {
  public:
    %extend {
        BermudanExerciseHandle(const std::vector<Date>& dates,
                               bool payoffAtExpiry = false) {
            return new BermudanExerciseHandle(
                                        new BermudanExercise(dates,
                                                             payoffAtExpiry));
        }
    }
};


#endif
