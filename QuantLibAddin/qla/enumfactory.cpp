
/*
 Copyright (C) 2005 Plamen Neykov

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qla/config.hpp>
#endif
#include <qla/enumfactory.hpp>
#include <ql/Currencies/all.hpp>
#include <ql/interestrate.hpp>
#include <ql/DayCounters/all.hpp>
#include <ql/Calendars/all.hpp>
#include <ql/Instruments/asianoption.hpp>
#include <ql/Instruments/basketoption.hpp>

#define REG_ENUM(Type, Body) \
	{ \
		TypeMapPtr typeMap(new TypeMap); \
		Body \
		allTypesMap[typeid(Type).name()] = typeMap; \
	}

#define MAP(str_id, constructor) \
	(*typeMap)[str_id] = new constructor

namespace QuantLibAddin {

	EnumTypeFactory::EnumTypeFactory() {
		REG_ENUM(QuantLib::Compounding,
			MAP("SIMPLE", QuantLib::Compounding(QuantLib::Simple));
			MAP("COMPOUNDED", QuantLib::Compounding(QuantLib::Compounded));
			MAP("CONTINUOUS", QuantLib::Compounding(QuantLib::Continuous));
			MAP("SIMPLETHENCOMPOUNDED", QuantLib::Compounding(QuantLib::SimpleThenCompounded));
		);

		REG_ENUM(QuantLib::Currency,
			// Africa
			MAP("ZAR", QuantLib::ZARCurrency());
			// America
			MAP("ARS", QuantLib::ARSCurrency());
			MAP("BRL", QuantLib::BRLCurrency());
			MAP("CAD", QuantLib::CADCurrency());
			MAP("CLP", QuantLib::CLPCurrency());
			MAP("COP", QuantLib::COPCurrency());
			MAP("MXN", QuantLib::MXNCurrency());
			MAP("TTD", QuantLib::TTDCurrency());
			MAP("USD", QuantLib::USDCurrency());
			MAP("VEB", QuantLib::VEBCurrency());
			// Asia
			MAP("BDT", QuantLib::BDTCurrency());
			MAP("CNY", QuantLib::CNYCurrency());
			MAP("HKD", QuantLib::HKDCurrency());
			MAP("ILS", QuantLib::ILSCurrency());
			MAP("INR", QuantLib::INRCurrency());
			MAP("IQD", QuantLib::IQDCurrency());
			MAP("IRR", QuantLib::IRRCurrency());
			MAP("JPY", QuantLib::JPYCurrency());
			MAP("KRW", QuantLib::KRWCurrency());
			MAP("KWD", QuantLib::KWDCurrency());
			MAP("NPR", QuantLib::NPRCurrency());
			MAP("PKR", QuantLib::PKRCurrency());
			MAP("SAR", QuantLib::SARCurrency());
			MAP("SGD", QuantLib::SGDCurrency());
			MAP("THB", QuantLib::THBCurrency());
			MAP("TWD", QuantLib::TWDCurrency());
			// Europe
			MAP("BGL", QuantLib::BGLCurrency());
			MAP("BYR", QuantLib::BYRCurrency());
			MAP("CHF", QuantLib::CHFCurrency());
			MAP("CYP", QuantLib::CYPCurrency());
			MAP("CZK", QuantLib::CZKCurrency());
			MAP("DKK", QuantLib::DKKCurrency());
			MAP("EEK", QuantLib::EEKCurrency());
			MAP("EUR", QuantLib::EURCurrency());
			MAP("GBP", QuantLib::GBPCurrency());
			MAP("HUF", QuantLib::HUFCurrency());
			MAP("ISK", QuantLib::ISKCurrency());
			MAP("LTL", QuantLib::LTLCurrency());
			MAP("LVL", QuantLib::LVLCurrency());
			MAP("MTL", QuantLib::MTLCurrency());
			MAP("NOK", QuantLib::NOKCurrency());
			MAP("PLN", QuantLib::PLNCurrency());
			MAP("ROL", QuantLib::ROLCurrency());
			MAP("SEK", QuantLib::SEKCurrency());
			MAP("SIT", QuantLib::SITCurrency());
			MAP("SKK", QuantLib::SKKCurrency());
			MAP("TRL", QuantLib::TRLCurrency());
			MAP("ATS", QuantLib::ATSCurrency());
			MAP("BEF", QuantLib::BEFCurrency());
			MAP("DEM", QuantLib::DEMCurrency());
			MAP("ESP", QuantLib::ESPCurrency());
			MAP("FIM", QuantLib::FIMCurrency());
			MAP("FRF", QuantLib::FRFCurrency());
			MAP("GRD", QuantLib::GRDCurrency());
			MAP("IEP", QuantLib::IEPCurrency());
			MAP("ITL", QuantLib::ITLCurrency());
			MAP("LUF", QuantLib::LUFCurrency());
			MAP("NLG", QuantLib::NLGCurrency());
			MAP("PTE", QuantLib::PTECurrency());
			// Oceania
			MAP("AUD", QuantLib::AUDCurrency());
			MAP("NZD", QuantLib::NZDCurrency());
		);

		REG_ENUM(QuantLib::Frequency,
			MAP("NOFREQUENCY", QuantLib::Frequency(QuantLib::NoFrequency));
			MAP("ONCE", QuantLib::Frequency(QuantLib::Once));
			MAP("ANNUAL", QuantLib::Frequency(QuantLib::Annual));
			MAP("SEMIANNUAL", QuantLib::Frequency(QuantLib::Semiannual));
			MAP("EVERY4MONTH", QuantLib::Frequency(QuantLib::EveryFourthMonth));
			MAP("QUARTERLY", QuantLib::Frequency(QuantLib::Quarterly));
			MAP("BIMONTHLY", QuantLib::Frequency(QuantLib::Bimonthly));
			MAP("MONTHLY", QuantLib::Frequency(QuantLib::Monthly));
		);

		REG_ENUM(QuantLib::DayCounter,
			MAP("ACTUAL365FIXED", QuantLib::Actual365Fixed());
			MAP("ACTUAL360", QuantLib::Actual360());
			MAP("ACTUALACTUAL", QuantLib::ActualActual());
			MAP("ACTUALACTUAL_ISDA", QuantLib::ActualActual(QuantLib::ActualActual::ISDA));
			MAP("ACTUALACTUAL_ISMA", QuantLib::ActualActual(QuantLib::ActualActual::ISMA));
			MAP("ACTUALACTUAL_BOND", QuantLib::ActualActual(QuantLib::ActualActual::Bond));
			MAP("ACTUALACTUAL_HISTORICAL", QuantLib::ActualActual(QuantLib::ActualActual::Historical));
			MAP("ACTUALACTUAL_AFB", QuantLib::ActualActual(QuantLib::ActualActual::AFB));
			MAP("ACTUALACTUAL_EURO", QuantLib::ActualActual(QuantLib::ActualActual::Euro));
			MAP("THIRTY360", QuantLib::Thirty360());
			MAP("THIRTY360_USA", QuantLib::Thirty360(QuantLib::Thirty360::USA));
			MAP("THIRTY360_BOND", QuantLib::Thirty360(QuantLib::Thirty360::BondBasis));
			MAP("THIRTY360_EU", QuantLib::Thirty360(QuantLib::Thirty360::European));
			MAP("THIRTY360_EU_BND", QuantLib::Thirty360(QuantLib::Thirty360::EurobondBasis));
			MAP("THIRTY360_ITL", QuantLib::Thirty360(QuantLib::Thirty360::Italian));
			MAP("ONE", QuantLib::OneDayCounter());
			MAP("SIMPLE", QuantLib::SimpleDayCounter());
		);

		REG_ENUM(QuantLib::Calendar,
			MAP("BEIJING", QuantLib::Beijing());
			MAP("BRATISLAVA", QuantLib::Bratislava());
			MAP("BUDAPEST", QuantLib::Budapest());
			MAP("COPENHAGEN", QuantLib::Copenhagen());
			MAP("GERMANY", QuantLib::Germany());
			MAP("GERMANY_FFTEXCHNG", QuantLib::Germany(QuantLib::Germany::FrankfurtStockExchange));
			MAP("GERMANY_SETTL", QuantLib::Germany(QuantLib::Germany::Settlement));
			MAP("GERMANY_XETRA", QuantLib::Germany(QuantLib::Germany::Xetra));
			MAP("GERMANY_EUREX", QuantLib::Germany(QuantLib::Germany::Eurex));
			MAP("HELSINKI", QuantLib::Helsinki());
			MAP("HONGKONG", QuantLib::HongKong());
			MAP("ITALY", QuantLib::Italy());
			MAP("ITALY_EXCHNG", QuantLib::Italy(QuantLib::Italy::Exchange));
			MAP("ITALY_SETTL", QuantLib::Italy(QuantLib::Italy::Settlement));
			MAP("JOHANNESBURG", QuantLib::Johannesburg());
			MAP("NULLCALENDAR", QuantLib::NullCalendar());
			MAP("OSLO", QuantLib::Oslo());
			MAP("PRAGUE", QuantLib::Prague());
			MAP("RIYADH", QuantLib::Riyadh());
			MAP("SEOUL", QuantLib::Seoul());
			MAP("SINGAPORE", QuantLib::Singapore());
			MAP("STOCKHOLM", QuantLib::Stockholm());
			MAP("SYDNEY", QuantLib::Sydney());
			MAP("TAIWAN", QuantLib::Taiwan());
			MAP("TARGET", QuantLib::TARGET());
			MAP("TOKYO", QuantLib::Tokyo());
			MAP("TORONTO", QuantLib::Toronto());
			MAP("UNITEDKINGDOM", QuantLib::UnitedKingdom());
			MAP("UNITEDKINGDOM_SETTL", QuantLib::UnitedKingdom(QuantLib::UnitedKingdom::Settlement));
			MAP("UNITEDKINGDOM_EXCHNG", QuantLib::UnitedKingdom(QuantLib::UnitedKingdom::Exchange));
			MAP("UNITEDKINGDOM_METALS", QuantLib::UnitedKingdom(QuantLib::UnitedKingdom::Metals));
			MAP("UNITEDSTATES", QuantLib::UnitedStates());
			MAP("UNITEDSTATES_SETTL", QuantLib::UnitedStates(QuantLib::UnitedStates::Settlement));
			MAP("UNITEDSTATES_EXCHNG", QuantLib::UnitedStates(QuantLib::UnitedStates::Exchange));
			MAP("UNITEDSTATES_GOV", QuantLib::UnitedStates(QuantLib::UnitedStates::GovernmentBond));
			MAP("WARSAW", QuantLib::Warsaw());
			MAP("WELLINGTON", QuantLib::Wellington());
			MAP("ZURICH", QuantLib::Zurich());
		);

		REG_ENUM(QuantLib::BusinessDayConvention,
			MAP("UNADJUSTED", QuantLib::BusinessDayConvention(QuantLib::Unadjusted));
			MAP("PRECEDING", QuantLib::BusinessDayConvention(QuantLib::Preceding));
			MAP("MODIFIEDPRECEDING", QuantLib::BusinessDayConvention(QuantLib::ModifiedPreceding));
			MAP("FOLLOWING", QuantLib::BusinessDayConvention(QuantLib::Following));
			MAP("MODIFIEDFOLLOWING", QuantLib::BusinessDayConvention(QuantLib::ModifiedFollowing));
			MAP("MONTHENDREFERENCE", QuantLib::BusinessDayConvention(QuantLib::MonthEndReference));
		);

		REG_ENUM(QuantLib::TimeUnit,
			MAP("DAYS", QuantLib::TimeUnit(QuantLib::Days));
			MAP("WEEKS", QuantLib::TimeUnit(QuantLib::Weeks));
			MAP("MONTHS", QuantLib::TimeUnit(QuantLib::Months));
			MAP("YEARS", QuantLib::TimeUnit(QuantLib::Years));
		);

		REG_ENUM(QuantLib::Average::Type,
			MAP("ARITHMETIC", QuantLib::Average::Type(QuantLib::Average::Arithmetic));
			MAP("GEOMETRIC", QuantLib::Average::Type(QuantLib::Average::Geometric));
		);

		REG_ENUM(QuantLib::Option::Type,
			MAP("PUT", QuantLib::Option::Type(QuantLib::Option::Put));
			MAP("CALL", QuantLib::Option::Type(QuantLib::Option::Call));
		);

		REG_ENUM(QuantLib::BasketOption::BasketType,
			MAP("MIN", QuantLib::BasketOption::BasketType(QuantLib::BasketOption::Min));
			MAP("MAX", QuantLib::BasketOption::BasketType(QuantLib::BasketOption::Max));
		);
	}

	std::vector<std::string> EnumTypeFactory::getAllRegisteredEnums() const {
		std::vector<std::string> ret;
		for(std::map<std::string, TypeMapPtr>::const_iterator i = allTypesMap.begin(); i != allTypesMap.end();++i)
			ret.push_back(i->first);
		return ret;
	}

	std::vector<std::string> EnumTypeFactory::getEnumElements(const std::string& id) const {
		std::map<std::string, TypeMapPtr>::const_iterator map = allTypesMap.find(id);
		QL_REQUIRE(map != allTypesMap.end(), "EnumTypeFactory::getEnumElements: invalid enum id: " + id);
		std::vector<std::string> ret;
		for(TypeMap::const_iterator i = map->second->begin(); i != map->second->end(); ++i)
			ret.push_back(i->first);
		return ret;
	}
}

