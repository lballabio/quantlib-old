/*
 Copyright (C) 2008 StatPro Italia srl

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

#ifndef quantlib_credit_i
#define quantlib_credit_i

%include defaultprobability.i
%include stl.i

%{
using QuantLib::Seniority;
using QuantLib::SEN;
using QuantLib::SUB;
using QuantLib::SEC;
using QuantLib::TI0;
using QuantLib::TI1;
using QuantLib::PCL;
using QuantLib::AnySeniority;
%}

enum Seniority { SEN, SUB, SEC, TI0, TI1, PCL, AnySeniority };


%{
using QuantLib::Restructuring;
using QuantLib::NR;
using QuantLib::MR;
using QuantLib::MM;
using QuantLib::RR;
using QuantLib::AnyRestructuring;
%}

enum Restructuring { NR, MR, MM, RR, AnyRestructuring };

%{
using QuantLib::Protection;
%}

struct Protection {
    enum Side { Buyer, Seller };
};


%{
using QuantLib::DefaultEvent;
%}

%ignore DefaultEvent;
class DefaultEvent {
  public:
    Date date() const;
    Real recoveryRatio() const;
    Seniority seniority() const;
    Restructuring restructuring() const;
};

%template(DefaultEvent) boost::shared_ptr<DefaultEvent>;
%extend boost::shared_ptr<DefaultEvent> {
    boost::shared_ptr<DefaultEvent>(const Date& date,
                                    Real recoveryRatio,
                                    Seniority seniority,
                                    Restructuring restructuring) {
        return new boost::shared_ptr<DefaultEvent>(
             new DefaultEvent(date, recoveryRatio, seniority, restructuring));
    }
}

#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_SPECIALIZE( DefaultEvent, boost::shared_ptr<DefaultEvent> )
#endif
namespace std {
    %template(DefaultEventVector) vector<boost::shared_ptr<DefaultEvent> >;
}


%{
using QuantLib::Issuer;
%}

class Issuer {
  public:
    Issuer(const Handle<DefaultProbabilityTermStructure>& probability =
                                    Handle<DefaultProbabilityTermStructure>(),
           Real recoveryRate = 0.4,
           const std::vector<boost::shared_ptr<DefaultEvent> >& events =
                             std::vector<boost::shared_ptr<DefaultEvent> >());
    const Handle<DefaultProbabilityTermStructure>& defaultProbability() const;
    Real recoveryRate() const;
    boost::shared_ptr<DefaultEvent> defaultedBetween(
                        const Date& start,
                        const Date& end,
                        Seniority seniority = AnySeniority,
                        Restructuring restructuring = AnyRestructuring) const;
};


#endif
