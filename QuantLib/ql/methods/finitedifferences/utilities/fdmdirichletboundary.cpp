/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2008 Andreas Gaida
 Copyright (C) 2008 Ralph Schreyer
 Copyright (C) 2008 Klaus Spanderen

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


#include <ql/methods/finitedifferences/meshers/fdmmesher.hpp>
#include <ql/methods/finitedifferences/operators/fdmlinearoplayout.hpp>
#include <ql/methods/finitedifferences/utilities/fdmdirichletboundary.hpp>
#include <ql/methods/finitedifferences/utilities/fdmindicesonboundary.hpp>

namespace QuantLib {

    FdmDirichletBoundary::FdmDirichletBoundary(
                            const boost::shared_ptr<FdmMesher>& mesher,
                            Real valueOnBoundary, Size direction,
                            FdmDirichletBoundary::Side side)
    : mesher_(mesher), direction_(direction), side_(side),
      valueOnBoundary_(valueOnBoundary), timeDependentBoundary_(false), spatialDependentBoundary_(false) {

		  init();                  
        
    }

	FdmDirichletBoundary::FdmDirichletBoundary(
                            const boost::shared_ptr<FdmMesher>& mesher,
                            const Array valuesOnBoundary, Size direction, Side side)
    : mesher_(mesher), direction_(direction), side_(side),
      valuesOnBoundary_(valuesOnBoundary), timeDependentBoundary_(false), spatialDependentBoundary_(true) {

		  init();                  
        
    }

	FdmDirichletBoundary::FdmDirichletBoundary(
                            const boost::shared_ptr<FdmMesher>& mesher,
							const boost::function<Real (Real)>& valueOnBoundary, Size direction, Side side)
    : mesher_(mesher), direction_(direction), side_(side),
      timeDependentBoundary_(true), spatialDependentBoundary_(false), valueOnBoundaryTimeDep_(valueOnBoundary) {

		  init();                  
        
    }

	FdmDirichletBoundary::FdmDirichletBoundary(
                            const boost::shared_ptr<FdmMesher>& mesher,
							const boost::function<Disposable<Array> (Real)>& valuesOnBoundary, Size direction, Side side)
    : mesher_(mesher), direction_(direction), side_(side),
      timeDependentBoundary_(true), spatialDependentBoundary_(true), valueOnBoundary_(0.0) {

		  init();                  
        
    }

	void FdmDirichletBoundary::init() {

		const boost::shared_ptr<FdmLinearOpLayout> layout = mesher_->layout();
                                
        std::vector<Size> newDim(layout->dim());
        newDim[direction_] = 1;
        const Size hyperSize = std::accumulate(newDim.begin(), newDim.end(),
                                               Size(1), std::multiplies<Size>());
        indicies_.resize(hyperSize);

        Size i=0;
        const FdmLinearOpIterator endIter = layout->end();
        for (FdmLinearOpIterator iter = layout->begin(); iter != endIter;
            ++iter) {
            if (   (side_ == Lower && iter.coordinates()[direction_] == 0)
                || (side_ == Upper && iter.coordinates()[direction_] 
                                            == layout->dim()[direction_]-1)) {

                QL_REQUIRE(hyperSize > i, "index missmatch");
                indicies_[i++] = iter.index();
            }
        }
        
        if (side_ == Lower) {
            xExtreme_ = mesher_->locations(direction_)[0];
        }
        else if (side_ == Upper) {
            xExtreme_ 
                = mesher_->locations(direction_)[layout->dim()[direction_]-1];
        }
	}

    void FdmDirichletBoundary::applyAfterApplying(Array& rhs) const {
		QL_REQUIRE(!spatialDependentBoundary_ || indicies_.size() == valuesOnBoundary_.size(), "values on boundary size (" << valuesOnBoundary_.size() << ") does not match hypersurface size (" << indicies_.size() << ")");
		for (std::vector<Size>::const_iterator iter = indicies_.begin();
             iter != indicies_.end(); ++iter) {
            rhs[*iter] = spatialDependentBoundary_ ? valuesOnBoundary_[iter - indicies_.begin()] : valueOnBoundary_;
        }
    }

	void FdmDirichletBoundary::applyBeforeSolving(operator_type&, Array& rhs) const {
		this->applyAfterApplying(rhs);
	}
    
    void FdmDirichletBoundary::applyAfterSolving(Array& rhs) const {
        this->applyAfterApplying(rhs);
    }
    
    Real FdmDirichletBoundary::applyAfterApplying(Real x, Real value) const {
        return (   (side_ == Lower && x < xExtreme_) 
                || (side_ == Upper && x > xExtreme_)) ? valueOnBoundary_ : value;
    }

	void FdmDirichletBoundary::setTime(Time t) {
		if(timeDependentBoundary_) {
			if(spatialDependentBoundary_) {
				valuesOnBoundary_ = valuesOnBoundaryTimeDep_(t);
			}
			else {
				valueOnBoundary_ = valueOnBoundaryTimeDep_(t);
			}
		}
	}

}
