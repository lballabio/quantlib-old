
/*
 Copyright (C) 2004 Eric Ehlers

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

#ifndef objectoption_h
#define objectoption_h

#include <QuantLibAddin/objects/objectstochastic.hpp>

namespace QuantLibAddin {

	// FIXME
	#define FIELD_NPV 						"NPV"
	#define FIELD_ENGINE 					"ENGINE"
	#define IDX_NPV 						0
	#define IDX_ENGINE 						1
	#define BINOMIAL_JARROW_RUDD			"BINOMIAL JARROW-RUDD"
	#define BINOMIAL_COX_ROSS_RUBINSTEIN	"BINOMIAL COX-ROSS-RUBINSTEIN"
	#define ADDITIVE_EQUIPROBABILITIES		"ADDITIVE EQUIPROBABILITIES"

	class ObjectOption : public ObjHandler::Object {
	public:
		ObjectOption(boost::shared_ptr<ObjectStochastic>,
			const std::string &typestr,
			const QuantLib::Real &strike,
			const QuantLib::Size &timeSteps,
			const QuantLib::Date &exerciseDate,
			const QuantLib::Date &settlementDate);
	//	~ObjectOption();
		void setEngine(const std::string &engineName,
			const QuantLib::Size &timeSteps);
		virtual boost::shared_ptr<void> getReference() const {
			return boost::static_pointer_cast<void>(vanillaOption_);
		}
	private:
		boost::shared_ptr<QuantLib::VanillaOption> vanillaOption_;
	};

}

#endif
