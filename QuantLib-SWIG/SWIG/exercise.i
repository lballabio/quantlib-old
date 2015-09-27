
/*
 Copyright (C) 2003 StatPro Italia srl

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

#ifndef quantlib_exercise_i
#define quantlib_exercise_i

%include common.i

// exercise conditions

%{
using QuantLib::Exercise;
%}

#if defined(SWIGJAVA) || defined(SWIGCSHARP)
%rename(_Exercise) Exercise;
#else
%ignore Exercise;
#endif
class Exercise {
  public:
    enum Type { American, Bermudan, European };
    Exercise::Type type() const;
    const std::vector<Date>& dates() const;
    Date lastDate() const { return dates_.back(); }
#if defined(SWIGJAVA) || defined(SWIGCSHARP)
  private:
    Exercise();
#endif
};

%template(Exercise) boost::shared_ptr<Exercise>;
%extend boost::shared_ptr<Exercise> {
    static const Exercise::Type American = Exercise::American;
    static const Exercise::Type Bermudan = Exercise::Bermudan;
    static const Exercise::Type European = Exercise::European;

	Exercise::Type exerciseType() { 
		return boost::dynamic_pointer_cast<Exercise>(*self)->type(); 
	}
}


%{
using QuantLib::EuropeanExercise;
using QuantLib::AmericanExercise;
using QuantLib::BermudanExercise;
typedef boost::shared_ptr<Exercise> EuropeanExercisePtr;
typedef boost::shared_ptr<Exercise> AmericanExercisePtr;
typedef boost::shared_ptr<Exercise> BermudanExercisePtr;
%}

%rename(EuropeanExercise) EuropeanExercisePtr;
class EuropeanExercisePtr : public boost::shared_ptr<Exercise> {
  public:
    %extend {
        EuropeanExercisePtr(const Date& date) {
            return new EuropeanExercisePtr(new EuropeanExercise(date));
        }
	}
};

%rename(AmericanExercise) AmericanExercisePtr;
class AmericanExercisePtr : public boost::shared_ptr<Exercise> {
  public:
    %extend {
        AmericanExercisePtr(const Date& earliestDate,
                            const Date& latestDate,
                            bool payoffAtExpiry = false) {
            return new AmericanExercisePtr(
                                        new AmericanExercise(earliestDate,
                                                             latestDate,
                                                             payoffAtExpiry));

        }
    }
};

%rename(BermudanExercise) BermudanExercisePtr;
class BermudanExercisePtr : public boost::shared_ptr<Exercise> {
  public:
    %extend {
        BermudanExercisePtr(const std::vector<Date>& dates,
                            bool payoffAtExpiry = false) {
            return new BermudanExercisePtr(
                                        new BermudanExercise(dates,
                                                             payoffAtExpiry));
        }
    }
};


#endif
