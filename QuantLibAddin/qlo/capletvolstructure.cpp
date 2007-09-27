
/*
 Copyright (C) 2006 Ferdinando Ametrano

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/capletvolstructure.hpp>

#include <ql/voltermstructures/interestrate/optionlet/constantoptionletvol.hpp>
#include <ql/voltermstructures/interestrate/optionlet/capstripper.hpp>
#include <ql/voltermstructures/interestrate/optionlet/spreadedoptionletvol.hpp>
#include <ql/voltermstructures/interestrate/capfloor/capfloortermvolcurve.hpp>
#include <ql/voltermstructures/interestrate/capfloor/capfloortermvolsurface.hpp>
#include <ql/voltermstructures/interestrate/optionlet/optionletstripperadapter.hpp>
#include <ql/voltermstructures/interestrate/optionlet/optionletstripper2.hpp>

namespace QuantLibAddin {

    ConstantOptionletVol::ConstantOptionletVol(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Handle<QuantLib::Quote>& volatility,
        const QuantLib::Calendar& cal,
        const QuantLib::DayCounter& dayCounter,
        bool permanent) : OptionletVolatilityStructure(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ConstantOptionletVol(volatility, cal, dayCounter));
    }
        
    OptionletStripperAdapter::OptionletStripperAdapter(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Handle<QuantLib::OptionletStripper>& optionletStripper,
        bool permanent) : OptionletVolatilityStructure(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::OptionletStripperAdapter(optionletStripper));
    }

    CapsStripper::CapsStripper(
         const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
         const std::vector<QuantLib::Period>& tenors,
         const std::vector<QuantLib::Rate>& strikes,
         const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& vols,
         const boost::shared_ptr<QuantLib::IborIndex>& index,
         const QuantLib::Handle<QuantLib::YieldTermStructure> yieldTermStructure,
         const QuantLib::DayCounter& dayCounter,
         QuantLib::Real impliedVolatilityAccuracy,
         QuantLib::Size maxEvaluations,
         bool decoupledInterpolations,
         bool permanent) : OptionletVolatilityStructure(properties, permanent)
    {
        std::vector<boost::shared_ptr<QuantLib::SmileSection> > dummySmileSections;
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapsStripper(tenors, strikes, vols, 
                                   index, yieldTermStructure, dayCounter,
                                   impliedVolatilityAccuracy, 
                                   maxEvaluations, dummySmileSections, true, 
                                   decoupledInterpolations));
    }

    
 CapsStripper::CapsStripper(
      const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
      const std::vector<QuantLib::Period>& tenors,
      const std::vector<QuantLib::Rate>& strikes,
      const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& vols,
      const boost::shared_ptr<QuantLib::IborIndex>& index,
      const QuantLib::Handle<QuantLib::YieldTermStructure> yieldTermStructure,
      const QuantLib::DayCounter& dayCounter,
      QuantLib::Real impliedVolatilityAccuracy,
      QuantLib::Size maxEvaluations,
      const std::vector<boost::shared_ptr<QuantLib::SmileSection> >& 
        smileSectionInterfaces,
      bool decoupledInterpolation,
      bool permanent) : OptionletVolatilityStructure(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapsStripper(tenors, strikes, vols, 
                                   index, yieldTermStructure, dayCounter,
                                   impliedVolatilityAccuracy, 
                                   maxEvaluations, smileSectionInterfaces,
                                   decoupledInterpolation));
    }


  CapsStripper::CapsStripper(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                             const std::vector<QuantLib::Period>& tenors,
                             const std::vector<QuantLib::Rate>& strikes,
                             const boost::shared_ptr<QuantLib::CapFloorTermVolSurface>& surface,
                             const boost::shared_ptr<QuantLib::IborIndex>& index,
                             QuantLib::Period timeStep,
                             const QuantLib::Handle<QuantLib::YieldTermStructure> yts,
                             const QuantLib::DayCounter& volatilityDayCounter,
                             QuantLib::Real impliedVolatilityAccuracy,
                             QuantLib::Size maxEvaluations,
                             bool allowExtrapolation,
                             bool decoupleInterpolation,
                             bool permanent)
                 : OptionletVolatilityStructure(properties, permanent){
      //bool allowExtrapolation = true;
     libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapsStripper(tenors,
                                   strikes,
                                   surface,
                                   index,
                                   timeStep,
                                   yts,
                                   volatilityDayCounter,
                                   impliedVolatilityAccuracy,
                                   maxEvaluations,
                                   allowExtrapolation,
                                   decoupleInterpolation));
    }


    SmileSectionsVolStructure::SmileSectionsVolStructure(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Date& referenceDate,
        const QuantLib::DayCounter& dayCounter,
        const std::vector<boost::shared_ptr<QuantLib::SmileSection> >& smiles,
        bool permanent) : OptionletVolatilityStructure(properties, permanent)
    {
        libraryObject_ = 
            boost::shared_ptr<QuantLib::SmileSectionsVolStructure>(new
                QuantLib::SmileSectionsVolStructure(referenceDate,
                                                    dayCounter, smiles));
    }

    SpreadedOptionletVol::SpreadedOptionletVol(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Handle<QuantLib::OptionletVolatilityStructure>& baseVol,
        const QuantLib::Handle<QuantLib::Quote>& spread,
        bool permanent) : OptionletVolatilityStructure(properties, permanent)
    {
        libraryObject_ =
            boost::shared_ptr<QuantLib::SpreadedOptionletVol>(new
                QuantLib::SpreadedOptionletVol(baseVol,
                                                            spread));
    }

    CapFloorTermVolCurve::CapFloorTermVolCurve(
          const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
          QuantLib::Natural settlementDays,
          const QuantLib::Calendar& calendar,
          const std::vector<QuantLib::Period>& optionTenors,
          const std::vector<QuantLib::Handle<QuantLib::Quote> >& vols,
          const QuantLib::DayCounter& dayCounter,
          bool permanent) : CapFloorTermVolatilityStructure(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapFloorTermVolCurve(settlementDays,
                                           calendar,
                                           optionTenors,
                                           vols,
                                           QuantLib::Following,
                                           dayCounter));
    }

    CapFloorTermVolSurface::CapFloorTermVolSurface(
          const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
          QuantLib::Natural settlementDays,
          const QuantLib::Calendar& calendar,
          const std::vector<QuantLib::Period>& optionLengths,
          const std::vector<QuantLib::Rate>& strikes,
          const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& vols,
          const QuantLib::DayCounter& dc,
          bool permanent) : CapFloorTermVolatilityStructure(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapFloorTermVolSurface(settlementDays,
                                             calendar,
                                             optionLengths,
                                             strikes,
                                             vols,
                                             QuantLib::Following,
                                             dc));
    }

    OptionletStripper::OptionletStripper(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                                         const boost::shared_ptr<QuantLib::CapFloorTermVolSurface>& surface,
                                         const boost::shared_ptr<QuantLib::IborIndex>& index,
                                         QuantLib::Rate switchStrike,
                                         bool permanent) : dummyClass(properties, permanent) {
        
        libraryObject_ = boost::shared_ptr<QuantLib::OptionletStripper>(new
            QuantLib::OptionletStripper(surface, index, switchStrike));
    }

    OptionletStripper2::OptionletStripper2(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                                           const QuantLib::Handle<QuantLib::OptionletStripper>& optionletStripper,
                                           const QuantLib::Handle<QuantLib::CapFloorTermVolCurve>& atmCapFloorTermVolCurve,
                                           const std::vector<QuantLib::Rate>& atmStrikes,
                                           bool permanent) : dummyClass1(properties, permanent) {
        
        libraryObject_ = boost::shared_ptr<QuantLib::OptionletStripper2>(new
            QuantLib::OptionletStripper2(optionletStripper, atmCapFloorTermVolCurve, atmStrikes));
    }


}
