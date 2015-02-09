/*
 Copyright (C) 2012 Tawanda Gwena
 Copyright (C) 2012 Francis Duffy

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

#ifndef quantlib_forward_rate_agreement_i
#define quantlib_forward_rate_agreement_i
 
%include instruments.i
%include termstructures.i
%include interestrate.i

%{
using QuantLib::Position;
using QuantLib::ForwardRateAgreement;
typedef boost::shared_ptr<Instrument> ForwardRateAgreementPtr;
%}
 
struct Position {
    enum Type { Long, Short };
};

%rename(ForwardRateAgreement) ForwardRateAgreementPtr;
class ForwardRateAgreementPtr : public boost::shared_ptr<Instrument> {
  public:
    %extend {
        ForwardRateAgreementPtr(
                        const Date& valueDate,
                        const Date& maturityDate,
                        Position::Type type,
                        Rate strikeForwardRate,
                        Real notionalAmount,
                        const IborIndexPtr& index,
                        const Handle<YieldTermStructure>& discountCurve =
                                               Handle<YieldTermStructure>()) {
             
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);

            return new ForwardRateAgreementPtr(
                   new ForwardRateAgreement(valueDate, maturityDate, type,
                                            strikeForwardRate, notionalAmount,
                                            libor, discountCurve));
        }
        Real spotIncome(const Handle<YieldTermStructure>& discount) const {
            return boost::dynamic_pointer_cast<ForwardRateAgreement>(*self)
                ->spotIncome(discount);
        }
        Real spotValue() const {
            return boost::dynamic_pointer_cast<ForwardRateAgreement>(*self)
                ->spotValue();
        }
        InterestRate forwardRate() const {
            return boost::dynamic_pointer_cast<ForwardRateAgreement>(*self)
                ->forwardRate();
        }
    }
};
 

#endif
