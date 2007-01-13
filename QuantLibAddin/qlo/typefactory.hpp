
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006 Eric Ehlers

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

#ifndef qla_typefactory_hpp
#define qla_typefactory_hpp

#include <qlo/typeregistry.hpp>
#include <ql/Instruments/payoffs.hpp>
#include <ql/Math/matrix.hpp>
#include <ql/Math/interpolation2D.hpp>
#include <ql/Indexes/euribor.hpp>
#include <ql/Indexes/euriborswapfixa.hpp>
#include <ql/Indexes/eurlibor.hpp>
#include <ql/Indexes/eurliborswapfixa.hpp>
#include <ql/Indexes/eurliborswapfixb.hpp>
#include <ql/Indexes/eurliborswapfixifr.hpp>
#include <ql/Indexes/euriborswapfixifr.hpp>
#include <ql/TermStructures/ratehelpers.hpp>
#include <ql/CashFlows/cmscoupon.hpp>
#include <ql/CashFlows/conundrumpricer.hpp>
#include <ql/Calendars/jointcalendar.hpp>
#include <oh/exception.hpp>
#include <boost/algorithm/string/case_conv.hpp>

namespace QuantLibAddin {

    template<typename T, typename RegistryClass>
    class RegistryManager {
    protected:
        template<typename KeyClass>
        void *getType(const KeyClass& id) {
            typename RegistryClass::TypeMapPtr type_map = getTypeMap();
            KeyClass idUpper = QuantLibAddin::uppercase(id);
            typename RegistryClass::TypeMap::iterator i;
            for (i = type_map->begin(); i != type_map->end(); i++)
                if (uppercase(i->first) == idUpper)
                    return i->second;
            QL_FAIL("Unknown id for Type: " << id);
        }

        bool checkType(const std::string& id) {
            typename RegistryClass::TypeMapPtr type_map;
            typename RegistryClass::AllTypeMap::const_iterator i =
                RegistryClass::instance().getAllTypesMap().find(typeid(T).name());
            if (i == RegistryClass::instance().getAllTypesMap().end()) {
                return false;
            } else {
                type_map = i->second;
            }
            std::string idUpper = boost::algorithm::to_upper_copy(id);
            for (typename RegistryClass::TypeMap::iterator i = type_map->begin(); i != type_map->end(); i++)
                if (boost::algorithm::to_upper_copy(i->first) == idUpper)
                    return true;
            return false;
        }

        void registerType(const std::string& id, void *type) {
            typename RegistryClass::TypeMapPtr type_map = getTypeMap();
            (*type_map)[id] = type;
        }
    private:
        const typename RegistryClass::TypeMapPtr &getTypeMap() {
            static typename RegistryClass::TypeMapPtr type_map;
            if(!type_map) {
                typename RegistryClass::AllTypeMap::const_iterator i =
                    RegistryClass::instance().getAllTypesMap().find(typeid(T).name());
                QL_REQUIRE(i != RegistryClass::instance().getAllTypesMap().end(), 
                    "Error retrieving Enumeration from Registry - the type '"
                    << typeid(T).name() << "' is not available!");
                type_map = i->second;
            }
            return type_map;
        }
    };

    /* *** Enumerated Types *** */

    /* *** Generic *** */
    template<typename T>
    class Create : private RegistryManager<T, EnumTypeRegistry> {
    public:
        T operator()(const std::string& id) {
            return *(static_cast<T*>(this->getType(id)));
        }
        using RegistryManager<T, EnumTypeRegistry>::checkType;
    };

    /* *** Calendar *** */

    template<>
    class Create<QuantLib::Calendar> : 
        private RegistryManager<QuantLib::Calendar, EnumTypeRegistry> {
    public:
        QuantLib::Calendar operator()(const std::string& id);
        using RegistryManager<QuantLib::Calendar, EnumTypeRegistry>::checkType;
    private:
        std::vector<std::string> calendarIDs;
        QuantLib::JointCalendarRule jointCalendarRule;
        std::string idOriginal, idUpper, idFull;
        bool testID();
        void parseID();
        QuantLib::Calendar *makeJointCalendar(const unsigned int&);
        QuantLib::Calendar *makeJointCalendar2();
        QuantLib::Calendar *makeJointCalendar3();
        QuantLib::Calendar *makeJointCalendar4();
    };

    /* *** Enumerated Classes *** */

    /* *** StrikedTypePayoff *** */
    typedef boost::shared_ptr<QuantLib::Payoff>(*StrikedTypePayoffConstructor1)(
        const QuantLib::Option::Type&, const double);
    typedef boost::shared_ptr<QuantLib::Payoff>(*StrikedTypePayoffConstructor2)(
        const QuantLib::Option::Type&, const double, const double);

    template<>
    class Create<boost::shared_ptr<QuantLib::Payoff> > :
        private RegistryManager<QuantLib::Payoff, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::Payoff> operator()(
                const std::string& payoffID,
                const QuantLib::Option::Type& optionType,
                const double strike) {
            StrikedTypePayoffConstructor1 strikedTypePayoffConstructor =
                (StrikedTypePayoffConstructor1)(getType(payoffID));
            return strikedTypePayoffConstructor(optionType, strike);
        }
        boost::shared_ptr<QuantLib::Payoff> operator()(
                const std::string& payoffID,
                const QuantLib::Option::Type& optionType,
                const double strike,
                const double strikeIncrement) {
            StrikedTypePayoffConstructor2 strikedTypePayoffConstructor =
                (StrikedTypePayoffConstructor2)(getType(payoffID));
            return strikedTypePayoffConstructor(optionType, strike, strikeIncrement);
        }
    };

    /* *** PricingEngine *** */
    typedef boost::shared_ptr<QuantLib::PricingEngine>(*PricingEngineConstructor)(
        const long&);

    template<>
    class Create<boost::shared_ptr<QuantLib::PricingEngine> > :
        private RegistryManager<QuantLib::PricingEngine, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::PricingEngine> operator()(const std::string& engineID,
                                                              const long& timeSteps) {
            // FIXME move this validation into QL
            QL_REQUIRE(timeSteps>0, "timeSteps must be positive");  
            PricingEngineConstructor pricingEngineConstructor =
                (PricingEngineConstructor)(getType(engineID));
            return pricingEngineConstructor(timeSteps);
        }
    };

    /* *** Linear 1D Interpolation *** */
    typedef const std::vector<double>::const_iterator dbl_itr;
    typedef boost::shared_ptr<QuantLib::Interpolation>(*InterpolationConstructor)(
        dbl_itr&, dbl_itr&, dbl_itr&);

    template<>
    class Create<boost::shared_ptr<QuantLib::Interpolation> > :
        private RegistryManager<QuantLib::Interpolation, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::Interpolation> operator() (
                const std::string& interpolationID, 
                dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin) {
            InterpolationConstructor interpolationConstructor = 
                (InterpolationConstructor)(getType(interpolationID));
            return interpolationConstructor(xBegin, xEnd, yBegin);
        }
    };

    /* *** Interpolation2D *** */
    typedef boost::shared_ptr<QuantLib::Interpolation2D>(*Interpolation2DConstructor)(
        dbl_itr&, dbl_itr&, dbl_itr&, dbl_itr&, const QuantLib::Matrix&);

    template<>
    class Create<boost::shared_ptr<QuantLib::Interpolation2D> > :
        private RegistryManager<QuantLib::Interpolation2D, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::Interpolation2D> operator() (
                const std::string& interpolationID, 
                dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin, dbl_itr& yEnd,
                const QuantLib::Matrix& zData) {
            Interpolation2DConstructor interpolation2DConstructor = 
                (Interpolation2DConstructor)(getType(interpolationID));
            return interpolation2DConstructor(xBegin, xEnd, yBegin, yEnd, zData);
        }
    };

    /* *** YieldTermStructure *** */
    typedef boost::shared_ptr<QuantLib::YieldTermStructure>(*YieldTermStructureConstructor)(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);

    template<>
    class Create<boost::shared_ptr<QuantLib::YieldTermStructure> > :
        private RegistryManager<QuantLib::YieldTermStructure, EnumCurveRegistry> {
    public:
        boost::shared_ptr<QuantLib::YieldTermStructure> operator() (
                const std::string& traitsID, 
                const std::string& interpolatorID, 
                const long &nDays,
                const QuantLib::Calendar &calendar,
                const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
                const QuantLib::DayCounter &dayCounter) {
            KeyPair key(traitsID, interpolatorID);
            YieldTermStructureConstructor yieldTermStructureConstructor = 
                (YieldTermStructureConstructor)(getType(key));
            return yieldTermStructureConstructor(nDays, calendar, rateHelpers, dayCounter);
        }
    };

    /* *** VanillaCMSCouponPricer *** */
    typedef boost::shared_ptr<QuantLib::VanillaCMSCouponPricer>(*VanillaCMSCouponPricerConstructor)( 
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
            const QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
            QuantLib::Real meanReversion);

    template<>
    class Create<boost::shared_ptr<QuantLib::VanillaCMSCouponPricer> > :
        private RegistryManager<QuantLib::VanillaCMSCouponPricer, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::VanillaCMSCouponPricer> operator() (
                const std::string& vanillaCMSCouponPricerID,
                const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
                const QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
                QuantLib::Real meanReversion) {
            VanillaCMSCouponPricerConstructor vanillaCMSCouponPricerConstructor =
                (VanillaCMSCouponPricerConstructor)(getType(vanillaCMSCouponPricerID));
            return vanillaCMSCouponPricerConstructor(swaptionVol,modelOfYieldCurve, meanReversion);
        }
    };

    // a singleton to store the Handle<YieldTermStructure>
    // shared by all enumerated Euribor classes
    class EuriborHandle : public QuantLib::Singleton<EuriborHandle> {
        friend class QuantLib::Singleton<EuriborHandle>;
    public:
        const QuantLib::Handle<QuantLib::YieldTermStructure> &handleYieldTermStructure() const {
            return handleYieldTermStructure_;
        }
        void linkEuriborHandle(boost::shared_ptr<QuantLib::YieldTermStructure> yieldTermStructure) {
            handleYieldTermStructure_.linkTo(yieldTermStructure);
        }
    private:
        QuantLib::Handle<QuantLib::YieldTermStructure> handleYieldTermStructure_;
    };
    
    ///* *** Index *** */
    typedef boost::shared_ptr<QuantLib::Index>(*IndexConstructor)();

    template<>
    class Create<boost::shared_ptr<QuantLib::Index> > :
        private RegistryManager<QuantLib::Index, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::Index> operator() (
                const std::string& euriborID) {
            IndexConstructor euriborConstructor = 
                (IndexConstructor)(getType(euriborID));
            return euriborConstructor();
        }
        using RegistryManager<QuantLib::Index, EnumClassRegistry>::checkType;
    };

    // some utilities required by class RegistryManager

    inline std::string uppercase(const std::string &s) {
        return boost::algorithm::to_upper_copy(s);
    }

    inline KeyPair uppercase(const KeyPair &s) {
        return KeyPair(boost::algorithm::to_upper_copy(s.first),
                       boost::algorithm::to_upper_copy(s.second));
    }

    inline std::ostream& operator<<(std::ostream& left, const KeyPair &right) {
        left << right.first << ":" << right.second;
        return left;
    }

 }

#endif

