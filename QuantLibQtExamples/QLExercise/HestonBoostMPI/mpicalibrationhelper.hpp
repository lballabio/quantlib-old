/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2012 Klaus Spanderen

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.
*/

#ifndef quantlib_mpi_calibration_helper_hpp
#define quantlib_mpi_calibration_helper_hpp

#include <ql/models/calibrationhelper.hpp>

namespace boost { 
    class thread; 
    namespace mpi { 
        class communicator; 
    } 
}

namespace QuantLib {
    
    class MPICalibrationHelper : public CalibrationHelper {
      public:
        MPICalibrationHelper(
            Integer id,
            const Handle<Quote>& volatility,
            const Handle<YieldTermStructure>& termStructure,
            const boost::shared_ptr<CalibrationHelper>& helper,
            CalibrationHelper::CalibrationErrorType errorType
                = CalibrationHelper::RelativePriceError);
        
        Real modelValue() const;
        Real blackPrice(Real volatility) const;
        void addTimesTo(std::list<Time>&) const;

        void update();
        void setPricingEngine(const boost::shared_ptr<PricingEngine>& engine);

    private:
        boost::shared_ptr<boost::thread> worker_;
        const boost::shared_ptr<CalibrationHelper> helper_;
        const boost::shared_ptr<boost::mpi::communicator> world_;

        const Integer rankId_;
        mutable Real modelValue_;
    };
}

#endif
