
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Silvia Frasson
 Copyright (C) 2006 Mario Pucci
 Copyright (C) 2006 Giorgio Facchinetti

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/swaptionvolstructure.hpp>
#include <ql/Volatilities/swaptionconstantvol.hpp>
#include <ql/Volatilities/swaptionvolcube2.hpp>
#include <ql/Volatilities/swaptionvolcube1.hpp>
#include <ql/Volatilities/swaptionvolmatrix.hpp>

namespace QuantLibAddin {

    SwaptionConstantVolatility::SwaptionConstantVolatility(
        const QuantLib::Date& referenceDate,
        const QuantLib::Handle<QuantLib::Quote>& vol,
        const QuantLib::DayCounter& dayCounter)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::SwaptionConstantVolatility(referenceDate,
                                                 vol,
                                                 dayCounter));
    }

    SwaptionVolatilityMatrix::SwaptionVolatilityMatrix(
            const QuantLib::Calendar& calendar,
            const std::vector<QuantLib::Period>& optionTenors,
            const std::vector<QuantLib::Period>& swapTenors,
            const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& vols,
            const QuantLib::DayCounter& dayCounter,
            const QuantLib::BusinessDayConvention bdc) {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::SwaptionVolatilityMatrix(calendar,
                                               optionTenors,
                                               swapTenors,
                                               vols,
                                               dayCounter,
                                               bdc));
    }

    std::vector<long> SwaptionVolatilityMatrix::locate(
                                                const QuantLib::Date& d,
                                                const QuantLib::Period& p) {
        std::pair<QuantLib::Size, QuantLib::Size> indexes =
            boost::dynamic_pointer_cast<QuantLib::SwaptionVolatilityMatrix>(
                libraryObject_)->locate(d,p);
        std::vector<long> result(2);
        result[0]=indexes.first;
        result[1]=indexes.second;
        return result;
    }

    SwaptionVolCube2::SwaptionVolCube2(
        const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& atmVol,
        const std::vector<QuantLib::Period>& optionTenors,
        const std::vector<QuantLib::Period>& swapTenors,
        const std::vector<QuantLib::Spread>& strikeSpreads,
        const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& volSpreads,
        const boost::shared_ptr<QuantLib::SwapIndex>& swapIndexBase,
        bool vegaWeightedSmileFit)
    {
        QL_REQUIRE(!atmVol.empty(), "atm vol handle not linked to anything");
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::SwaptionVolCube2(atmVol,
                                                     optionTenors,
                                                     swapTenors,
                                                     strikeSpreads,
                                                     volSpreads,
                                                     swapIndexBase,
                                                     vegaWeightedSmileFit));
    }

    SwaptionVolCube1::SwaptionVolCube1(
        const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& atmVol,
        const std::vector<QuantLib::Period>& optionTenors,
        const std::vector<QuantLib::Period>& swapTenors,
        const std::vector<QuantLib::Spread>& strikeSpreads,
        const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& volSpreads,
        const boost::shared_ptr<QuantLib::SwapIndex>& swapIndexBase,
        bool vegaWeightedSmileFit,
        const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& parametersGuess,
        const std::vector<bool>& isParameterFixed,
        bool isAtmCalibrated,
        const boost::shared_ptr<QuantLib::EndCriteria>& endCriteria,
        QuantLib::Real maxErrorTolerance)
    {
        QL_REQUIRE(!atmVol.empty(), "atm vol handle not linked to anything");
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::SwaptionVolCube1(atmVol,
                                                   optionTenors,
                                                   swapTenors,
                                                   strikeSpreads,
                                                   volSpreads,
                                                   swapIndexBase,
                                                   vegaWeightedSmileFit, 
                                                   parametersGuess,
                                                   isParameterFixed,
                                                   isAtmCalibrated,
                                                   endCriteria,
                                                   maxErrorTolerance ));
    }

    std::vector<std::vector<boost::any> >
    SwaptionVolCube1::getSparseSabrParameters() {
        const boost::shared_ptr<QuantLib::SwaptionVolCube1>&
            volCube = boost::dynamic_pointer_cast<
                    QuantLib::SwaptionVolCube1>(libraryObject_);
        return getSabrParameters(volCube->sparseSabrParameters());
    }

    std::vector<std::vector<boost::any> >
    SwaptionVolCube1::getDenseSabrParameters() {
        const boost::shared_ptr<QuantLib::SwaptionVolCube1>&
            volCube = boost::dynamic_pointer_cast<
                    QuantLib::SwaptionVolCube1>(libraryObject_);
        return getSabrParameters(volCube->denseSabrParameters());
    }

    std::vector<std::vector<boost::any> >
    SwaptionVolCube1::getMarketVolCube() {
        const boost::shared_ptr<QuantLib::SwaptionVolCube1>&
            volCube = boost::dynamic_pointer_cast<
                    QuantLib::SwaptionVolCube1>(libraryObject_);
        return getVolCube(volCube->marketVolCube());
    }

    std::vector<std::vector<boost::any> >
    SwaptionVolCube1::getVolCubeAtmCalibrated() {
        const boost::shared_ptr<QuantLib::SwaptionVolCube1>&
            volCube = boost::dynamic_pointer_cast<
                    QuantLib::SwaptionVolCube1>(libraryObject_);
        return getVolCube(volCube->volCubeAtmCalibrated());
    }

        
    //std::vector<std::vector<boost::any> > getSabrParameters(QuantLib::Matrix & sabrParameters)
    std::vector<std::vector<boost::any> > getSabrParameters(QuantLib::Matrix sabrParameters)
    {
        std::vector<std::vector<boost::any> > sparseSabrParameters;
        QuantLib::Size numberOfColumn = 10;

        std::vector<boost::any> headings(numberOfColumn);
        headings[0]=std::string("Swap Length");
        headings[1]=std::string("Expiriy");

        headings[2]=std::string("Alpha");
        headings[3]=std::string("Beta");
        headings[4]=std::string("Nu");
        headings[5]=std::string("Rho");
        headings[6]=std::string("Forward");
        headings[7]=std::string("Error");
        headings[8]=std::string("Max Error");
        headings[9]=std::string("End Criteria");


        sparseSabrParameters.push_back(headings);

        for(QuantLib::Size i=0; i<sabrParameters.rows(); ++i)
        {
            std::vector<boost::any> par(numberOfColumn, std::string("N/A"));
            for(QuantLib::Size j=0; j<sabrParameters.columns()-1; ++j)
            {
               par[j] = sabrParameters[i][j]; 
            }
            // FIXME with ostream
            switch (int(sabrParameters[i][numberOfColumn-1])) {
                case QuantLib::EndCriteria::None:
                    par[numberOfColumn-1] = std::string("None");
                    break;
                case QuantLib::EndCriteria::MaxIterations:
                    par[numberOfColumn-1] = std::string("MaxIterations");
                    break;
                case QuantLib::EndCriteria::StationaryPoint:
                    par[numberOfColumn-1] = std::string("StationaryPoint");
                    break;
                case QuantLib::EndCriteria::StationaryGradient:
                    par[numberOfColumn-1] = std::string("StationaryGradient");
                    break;
                case QuantLib::EndCriteria::Unknown:
                    par[numberOfColumn-1] = std::string("Unknown");
                    break;
                default:
                    par[numberOfColumn-1] = std::string("unknown EndCriteria::Type ");
                    break;
                }
            sparseSabrParameters.push_back(par);
        }
        return sparseSabrParameters;
    }

    //std::vector<std::vector<boost::any> > getVolCube(QuantLib::Matrix & volCube)
    std::vector<std::vector<boost::any> > getVolCube(QuantLib::Matrix volCube)
    {
        std::vector<std::vector<boost::any> > volatilityCube;
        QuantLib::Size numberOfColumn = 11;

        std::vector<boost::any> headings(numberOfColumn);
        headings[0]=std::string("Swap Length");
        headings[1]=std::string("Expiriy");

        headings[2]=std::string("-200 bps");
        headings[3]=std::string("-100 bps");
        headings[4]=std::string("-50 bps");
        headings[5]=std::string("-25 bps");
        headings[6]=std::string("0 bps");
        headings[7]=std::string("25 bps");
        headings[8]=std::string("50 bps");
        headings[9]=std::string("100 bps");
        headings[10]=std::string("200 bps");

        volatilityCube.push_back(headings);

        for(QuantLib::Size i=0; i<volCube.rows(); ++i)
        {
            std::vector<boost::any> vol(numberOfColumn, std::string("N/A"));
            for(QuantLib::Size j=0; j<volCube.columns(); ++j)
            {
               vol[j] = volCube[i][j];
            }
            volatilityCube.push_back(vol);
        }
        return volatilityCube;
    }

    SmileSectionByCube::SmileSectionByCube(
            const boost::shared_ptr<QuantLib::SwaptionVolatilityCube>& cube,
            const QuantLib::Period& optionTenor,
            const QuantLib::Period& swapTenors){
             libraryObject_ = cube->smileSection(optionTenor,swapTenors);
    }

    SmileSectionByCube::SmileSectionByCube(
            const boost::shared_ptr<QuantLib::SwaptionVolatilityCube>& cube,
            const QuantLib::Date& optionDate,
            const QuantLib::Period& swapTenors){
             libraryObject_ = cube->smileSection(optionDate,swapTenors);
    }

}
