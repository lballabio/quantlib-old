
/*
 Copyright (C) 2006, 2007 StatPro Italia srl

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

#ifndef quantlib_callability_i
#define quantlib_callability_i

%include date.i
%include vectors.i
%include common.i

%{
using QuantLib::Callability;
using QuantLib::SoftCallability;
typedef boost::shared_ptr<Callability> CallabilityPtr;
typedef boost::shared_ptr<Callability> SoftCallabilityPtr;
typedef Callability::Price CallabilityPrice;
using QuantLib::CallabilitySchedule;
%}

class CallabilityPrice {
  public:
    enum Type { Dirty, Clean };
    CallabilityPrice(Real amount, Type type);
    Real amount() const;
    Type type() const;
};

#if defined(SWIGJAVA) || defined(SWIGCSHARP)
%rename(_Callability) Callability;
#else
%ignore Callability;
#endif
class Callability {
  public:
    enum Type { Call, Put };
    const CallabilityPrice& price() const;
    Type type() const;
    Date date() const;
#if defined(SWIGJAVA) || defined(SWIGCSHARP)
  private:
    Callability();
#endif
};

%template(Callability) boost::shared_ptr<Callability>;
%extend boost::shared_ptr<Callability> {
    shared_ptr<Callability>(const CallabilityPrice& price,
                            Callability::Type type,
                            const Date& date) {
        return new boost::shared_ptr<Callability>(
                                            new Callability(price,type,date));
    }
    static const Callability::Type Call = Callability::Call;
    static const Callability::Type Put = Callability::Put;
}

%{
using QuantLib::SoftCallability;
typedef boost::shared_ptr<Callability> SoftCallabilityPtr;
%}

%rename(SoftCallability) SoftCallabilityPtr;
class SoftCallabilityPtr : public boost::shared_ptr<Callability> {
  public:
    %extend {
        SoftCallabilityPtr(const CallabilityPrice& price, const Date& date,
                           Real trigger) {
            return new SoftCallabilityPtr(new SoftCallability(price,date,
                                                              trigger));
        }
    }
};


#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_ENHANCED( boost::shared_ptr<Callability> )
#endif
namespace std {
    %template(CallabilitySchedule) vector<boost::shared_ptr<Callability> >;
}

#endif
