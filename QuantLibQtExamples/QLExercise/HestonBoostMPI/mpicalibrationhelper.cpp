/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2012 Klaus Spanderen

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.

 compile:
g++ -I. -I/usr/lib/mpich/include -I/home/spanderen/workspace/QuantLib -O3 -c mpihestonmodelhelper.cpp 
*/

#include <ql/pricingengine.hpp>

#include <assert.h>
#include <boost/mpi.hpp>
#include <boost/thread.hpp>

#include <mpicalibrationhelper.hpp>

namespace mpi = boost::mpi;

namespace QuantLib {

    namespace {
        class Worker {
          public:
            Worker(const boost::shared_ptr<CalibrationHelper>& helper,
                   Real& modelValue)
            : helper_(helper), modelValue_(modelValue) { }

            void operator()() {
                boost::mutex::scoped_lock lock(mutex_);
                modelValue_ = helper_->modelValue();
            }

          private:
            static boost::mutex mutex_;
            Real& modelValue_;
            const boost::shared_ptr<CalibrationHelper> helper_;
        };

        boost::mutex Worker::mutex_;
    }
    
    MPICalibrationHelper::MPICalibrationHelper(
        Integer id,
        const Handle<Quote>& volatility,
        const Handle<YieldTermStructure>& termStructure,
        const boost::shared_ptr<CalibrationHelper>& helper,
        CalibrationHelper::CalibrationErrorType errorType)
    : CalibrationHelper(volatility, termStructure, errorType),
      helper_(helper), 
      world_ (new mpi::communicator()),
      rankId_(id % world_->size()) {
    }

    void MPICalibrationHelper::setPricingEngine(
        const boost::shared_ptr<PricingEngine>& engine) {

        unregisterWith(engine_);
        CalibrationHelper::setPricingEngine(engine);
        registerWith(engine_);

        helper_->setPricingEngine(engine_);
    }

    Real MPICalibrationHelper::modelValue() const {
        if (world_->rank() == rankId_) {
            worker_->join();
        }
        mpi::broadcast(*world_, modelValue_, rankId_);

        return modelValue_;
    }

    void MPICalibrationHelper::update() {
        if (world_->rank() == rankId_) {
            if (worker_) {
                worker_->join();
            }
            worker_.reset(new boost::thread(Worker(helper_, modelValue_)));
        }
        CalibrationHelper::update();
    }

    Real MPICalibrationHelper::blackPrice(Real volatility) const {
        return helper_->blackPrice(volatility);
    }
    void MPICalibrationHelper::addTimesTo(std::list<Time>& t) const {
        helper_->addTimesTo(t);
    }
}
