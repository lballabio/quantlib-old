
/*
 Copyright (C) 2008 Eric Ehlers

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

#include <Addins/Cpp/addincpp.hpp>
#include <ql/qldefines.hpp>
#if defined BOOST_MSVC
#include <oh/auto_link.hpp>
#include <ql/auto_link.hpp>
#endif
#include <iostream>

#ifdef LOG_MESSAGE
#undef LOG_MESSAGE
#endif
#ifdef LOG_ERROR
#undef LOG_ERROR
#endif

#define OH_NULL ObjectHandler::property_t()
#define LOG_MESSAGE(msg) std::cerr << msg << std::endl
#define LOG_ERROR(msg) std::cerr << msg << std::endl

using namespace QuantLibAddinCpp;

int main(int argc, char** argv) {

    try {
        if(argc < 5) {
            std::cerr << "Usage:" << argv[0] << "  evaldate tradeid tradefile marketfile_1 marketfile_2 ... marketfile_n\n";
            return -1;
        }

        long lEvaldate = atol(argv[1]);
        std::string tradeID = argv[2];
        std::string tradeFile = argv[3];
        std::vector<std::string> vecMaketfiles;

        for(int i = 4; i < argc; i += 1) {
            vecMaketfiles.push_back(argv[i]);
        }
        /*
        //print the information of the command line
        std::cout<< "evaldate = " << lEvaldate<<"\n tradeid is" << tradeID 
            << "\n trade file" << tradeFile;
        int i = 0;
        for(int i = 0; i < vecMaketfiles.size(); ++i)
            std::cout<<"\nmarket file" << (i+1) <<" is "<<vecMaketfiles[i];
        */

        // Initialize the environment

        initializeAddin();

        ohSetLogFile("qla_demo.log", 4L, OH_NULL);
        ohSetConsole(1, 4L, OH_NULL);
        LOG_MESSAGE("Begin example program.");
        LOG_MESSAGE("QuantLibAddin version = " << qlAddinVersion(OH_NULL));
        LOG_MESSAGE("ObjectHandler version = " << ohVersion(OH_NULL));

        // Set the evaluation date

        qlSettingsSetEvaluationDate(lEvaldate, OH_NULL);

        // Deserialize the objects
        for(unsigned int index = 0; index < vecMaketfiles.size(); ++index) {
            ohObjectLoad("MarketData", vecMaketfiles[index], false, OH_NULL, OH_NULL);
        }
        ohObjectLoad("Models", tradeFile, false, OH_NULL, OH_NULL);

        // Output the PV of the deal

        LOG_MESSAGE("Instrument PV = " << qlInstrumentNPV(tradeID, OH_NULL));

        LOG_MESSAGE("End example program.");

        return 0;

    } catch (const std::exception &e) {
        LOG_ERROR("Error: " << e.what());
        return 1;
    } catch (...) {
        LOG_ERROR("Unknown error");
        return 1;
    }
}

