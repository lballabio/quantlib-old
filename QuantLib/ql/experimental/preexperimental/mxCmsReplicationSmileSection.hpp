/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2012 Peter Caspers

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

/*! \file mxCmsReplicationSmileSection.hpp
    \brief smile section that takes a source smile section and then applies the cut rules of the Mx 3.1.22 CMS Replication Model
*/

#ifndef quantlib_mx_cmsrepl_smile_section_hpp
#define quantlib_mx_cmsrepl_smile_section_hpp

#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/math/interpolations/linearinterpolation.hpp>
#include <vector>

namespace QuantLib {

    class MxCmsReplSmileSection : public SmileSection {

	  public:

        MxCmsReplSmileSection(const boost::shared_ptr<SmileSection> source, const std::vector<Real>& strikeSpreads, const Real slope = Null<Real>(), const Real inputCutoff = 1.0, const Real flatCutoff = 3.0);

	    Real minStrike () const { return 0.0; }
        Real maxStrike () const { return QL_MAX_REAL; }
        Real atmLevel() const { return f_; }

      protected:
		Volatility volatilityImpl(Rate strike) const;
     
	  private:
		boost::shared_ptr<SmileSection> source_;
		std::vector<Real> strikeSpreads_;
		std::vector<Real> logMoneynessStrikes_;
		std::vector<Real> vols_;
		Real slope_, f_, inputCutoff_, flatCutoff_;
		boost::shared_ptr<Interpolation> volI_;

    };


}

#endif
