
/*
 Copyright (C) 2007 Cristina Duminuco
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2006 Eric Ehlers
 Copyright (C) 2006 Giorgio Facchinetti
 Copyright (C) 2005 Aurelien Chanudet

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
    #include <qlo/config.hpp>
#endif

#include <qlo/qladdindefines.hpp>
#include <qlo/Factories/conundrumpricerfactory.hpp>
#include <qlo/conundrumpricer.hpp>


namespace QuantLibAddin {

   CmsCouponPricer::CmsCouponPricer(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& v,
            const std::string& typeOfCmsCouponPricer,
            QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
            const QuantLib::Handle<QuantLib::Quote>& meanReversion) {
        libraryObject_ = Create<boost::shared_ptr<QuantLib::CmsCouponPricer> >()
            (typeOfCmsCouponPricer, v, modelOfYieldCurve, meanReversion);
    }


	ConundrumPricerByNumericalIntegration::ConundrumPricerByNumericalIntegration(
			const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
            QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
			const QuantLib::Handle<QuantLib::Quote>& meanReversion,
            QuantLib::Rate lowerLimit,
            QuantLib::Rate upperLimit,
			QuantLib::Real precision){
				/*QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve
					= QuantLib::GFunctionFactory::ModelOfYieldCurve(2);*/
				libraryObject_ = 
					boost::shared_ptr<QuantLib::ConundrumPricerByNumericalIntegration> (new
					QuantLib::ConundrumPricerByNumericalIntegration(swaptionVol,
                                                                    modelOfYieldCurve,
																	meanReversion,
																	lowerLimit,
																	upperLimit,
																	precision));
	};
}
