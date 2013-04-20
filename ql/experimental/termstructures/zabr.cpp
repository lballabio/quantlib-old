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

#include <ql/termstructures/volatility/zabr.hpp>
#include <ql/termstructures/volatility/sabr.hpp>
#include <ql/errors.hpp>
#include <ql/math/comparison.hpp>

#include <boost/function.hpp>
#include <boost/lambda/lambda.hpp>
//#include <boost/lambda/if.hpp>
#include <boost/lambda/bind.hpp>

#include <adaptiveRungeKutta.hpp>

#include <ql/experimental/finitedifferences/fdmlinearoplayout.hpp>
#include <ql/experimental/finitedifferences/fdm1dmesher.hpp>
#include <ql/experimental/finitedifferences/uniform1dmesher.hpp>
#include <ql/experimental/finitedifferences/concentrating1dmesher.hpp>
#include <ql/experimental/finitedifferences/glued1dmesher.hpp>
#include <ql/experimental/finitedifferences/fdmmeshercomposite.hpp>
#include <ql/methods/finitedifferences/operatortraits.hpp>
#include <ql/experimental/finitedifferences/fdmdirichletboundary.hpp>
#include <ql/experimental/finitedifferences/fdmbackwardsolver.hpp>

#include <ql/experimental/finitedifferences/fdmdupire1dop.hpp>
#include <ql/experimental/finitedifferences/fdmzabrop.hpp>

//#include <iostream>
//#include <fstream>
//#include <sstream>

namespace QuantLib {

		ZabrModel::ZabrModel(const Real forward, const  Real expiryTime, const  Real alpha, const  Real beta, const  Real nu, const  Real rho, const Real gamma) :
				forward_(forward), expiryTime_(expiryTime), alpha_(alpha), beta_(beta), nu_(nu), rho_(rho), gamma_(gamma) {

					validateSabrParameters(alpha,beta,nu,rho);
			        QL_REQUIRE(gamma>=0.0 /*&& gamma<=1.0*/, "gamma must be non negative: " << gamma << " not allowed");
					QL_REQUIRE(forward >= 0.0,"forward must be non negative: " << forward << " not allowed");
					QL_REQUIRE(expiryTime > 0.0,"expiry time must be positive: " << expiryTime << " not allowed");

				}

		Real ZabrModel::lognormalVolatilityHelper(const Real strike, const Real x) const {
			if(close(strike,forward_)) return std::pow(forward_,beta_-1.0)*alpha_;
			else return std::log(forward_/strike)/x;
		}

		Real ZabrModel::lognormalVolatility(const Real strike) const {
			return lognormalVolatility(std::vector<Real>(1,strike))[0];
		}

		Disposable<std::vector<Real>> ZabrModel::lognormalVolatility(const std::vector<Real>& strikes) const {
			std::vector<Real> x_ = x(strikes);
			std::vector<Real> result(strikes.size());
			std::transform(strikes.begin(),strikes.end(),x_.begin(),result.begin(),boost::lambda::bind(&ZabrModel::lognormalVolatilityHelper,this,boost::lambda::_1,boost::lambda::_2));
			return result;
		}

		Real ZabrModel::normalVolatilityHelper(const Real strike, const Real x) const {
			if(close(strike,forward_)) return std::pow(forward_,beta_)*alpha_;
			 else return (forward_-strike)/x;
		}


		Real ZabrModel::normalVolatility(const Real strike) const {
			return normalVolatility(std::vector<Real>(1,strike))[0];
		}

		Disposable<std::vector<Real>> ZabrModel::normalVolatility(const std::vector<Real>& strikes) const {
			std::vector<Real> x_ = x(strikes);
			std::vector<Real> result(strikes.size());
			std::transform(strikes.begin(),strikes.end(),x_.begin(),result.begin(),boost::lambda::bind(&ZabrModel::normalVolatilityHelper,this,boost::lambda::_1,boost::lambda::_2));
			return result;
		}

		Real ZabrModel::localVolatilityHelper(const Real f, const Real x) const {
			return alpha_*std::pow(f,beta_) / F( y(f), std::pow(alpha_, gamma_-1.0) * x ); // TODO optimize this, y is comoputed together with x already
		}

		Real ZabrModel::localVolatility(const Real f) const {
			return localVolatility(std::vector<Real>(1,f))[0];
		}

		Disposable<std::vector<Real>> ZabrModel::localVolatility(const std::vector<Real>& f) const {
			std::vector<Real> x_ = x(f);
			std::vector<Real> result(f.size());
			std::transform(f.begin(),f.end(),x_.begin(),result.begin(),boost::lambda::bind(&ZabrModel::localVolatilityHelper,this,boost::lambda::_1,boost::lambda::_2));
			return result;	
		}

		Real ZabrModel::fdPrice(const Real strike) const {
			return fdPrice(std::vector<Real>(1,strike))[0];
		}

		Disposable<std::vector<Real>> ZabrModel::fdPrice(const std::vector<Real>& strikes) const {

			//TODO put these magic numbers somewhere ...
			const Real start = 0.00001;          // lowest strike for grid
			const Real end = forward_ * 20.0;    // highest strike for grid
			const Size size = 200;				 // grid points
			const Real density = 0.005;          // density for non concentrating mesher
			const Size steps = (Size)std::ceil(expiryTime_*50);    // number of steps in dimension t
			const Size dampingSteps = 10;         // thereof damping steps

			// Layout
			std::vector<Size> dim(1,size);
		    const boost::shared_ptr<FdmLinearOpLayout> layout(new FdmLinearOpLayout(dim));
			// Mesher
			const boost::shared_ptr<Fdm1dMesher> m1(new Concentrating1dMesher(start,end,size,std::pair<Real,Real>(forward_,density),true));
			//const boost::shared_ptr<Fdm1dMesher> m1(new Uniform1dMesher(start,end,size));
			const std::vector<boost::shared_ptr<Fdm1dMesher>> meshers(1,m1);
			const boost::shared_ptr<FdmMesher> mesher(new FdmMesherComposite(layout,meshers));
			// Boundary conditions
			FdmBoundaryConditionSet boundaries;
			//boundaries.push_back(boost::shared_ptr<BoundaryCondition<FdmLinearOp>>(new FdmDirichletBoundary(mesher,0.0,0,FdmDirichletBoundary::Upper))); // for strike = \infty call is worth zero
			// initial values
			Array rhs(mesher->layout()->size()); 
			for (FdmLinearOpIterator iter = layout->begin(); iter != layout->end(); ++iter) {
				Real k = mesher->location(iter,0);
	            rhs[iter.index()] = std::max(forward_-k,0.0);
			}
			// local vols (TODO how can we avoid these Array / vector copying?)
			Array k = mesher->locations(0); 
			std::vector<Real> kv(k.size());
			std::copy(k.begin(),k.end(),kv.begin());
			std::vector<Real> locVolv = localVolatility(kv);
			Array locVol(locVolv.size());
			std::copy(locVolv.begin(),locVolv.end(),locVol.begin());
			/*std::cout << "LocalVol Function:" << std::endl;
			for(int i=0;i<locVol.size();i++) {
				std::cout << k[i] << ";" << locVol[i] << std::endl;
			}*/
			// solver
			boost::shared_ptr<FdmDupire1dOp> map(new FdmDupire1dOp(mesher,locVol));
			FdmBackwardSolver solver(map,boundaries,boost::shared_ptr<FdmStepConditionComposite>(),FdmSchemeDesc::Douglas());
			solver.rollback(rhs,expiryTime_,0.0,steps,dampingSteps);

			// test
			/*for(int i=0;i<rhs.size();i++) {
				std::cout << "0;" << rhs[i] << std::endl;
			}
			for(int k=1;k<10;k++) {
				solver.rollback(rhs,0.1,0.0,1,0);
				for(int i=0;i<rhs.size();i++) {
					std::cout << k << ";" << rhs[i] << std::endl;
				}
			}*/
			// end test

			// interpolate solution
			boost::shared_ptr<Interpolation> solution(new CubicInterpolation(k.begin(),k.end(),rhs.begin(),CubicInterpolation::Spline,true,
																CubicInterpolation::SecondDerivative,0.0,CubicInterpolation::SecondDerivative,0.0));
			//boost::shared_ptr<Interpolation> solution(new LinearInterpolation(k.begin(),k.end(),rhs.begin()));
			solution->disableExtrapolation();
			/*std::cout << "Solution (within Zabr::fdPrice())" << std::endl;
			for(int i=0;i<k->size();i++) {
				std::cout << k[i] << ";" << (*solution)((*k)[i]) << std::endl;
			}*/
			//return solution;
			std::vector<Real> result(strikes.size());
			std::transform(strikes.begin(),strikes.end(),result.begin(),*solution);
			return result;
		}

		Real ZabrModel::fullFdPrice(const Real strike) const {
			
			//TODO put these magic numbers somewhere ...
			const Real f0 = 0.00001, f1 = forward_ * 75;	    // forward
			const Real v0 = 0.00001, v1 = alpha_ * 75;	     // volatility
			const Size sizef = 100, sizev = 100;			    // grid points
			const Real densityf = 0.01, densityv = 0.01;		// density for concentrating mesher
			const Size steps = (Size)std::ceil(expiryTime_*50); // number of steps in dimension t
			const Size dampingSteps = 20;						// thereof damping steps

			QL_REQUIRE(strike >=f0 && strike <=f1,"strike (" << strike << ") must be inside pde grid [" << f0 << ";" << f1 << "]");

			// Layout
			std::vector<Size> dim;
			dim.push_back(sizef);
			dim.push_back(sizev);
		    const boost::shared_ptr<FdmLinearOpLayout> layout(new FdmLinearOpLayout(dim));
			// Mesher
			
			// two concentrating mesher around f and k to get the mesher for the forward
			//const Real x0 = std::min(forward_,strike);
			//const Real x1 = std::max(forward_,strike);
			//const Size sizefa = (Size)std::ceil(((x0+x1)/2.0-f0)/(f1-f0)*(Real)sizef);
			//const Size sizefb = sizef-sizefa+1; // common point, so we can spend one more here
			//const boost::shared_ptr<Fdm1dMesher> mfa(new Concentrating1dMesher(f0,(x0+x1)/2.0,sizefa,std::pair<Real,Real>(x0,densityf),true));
			//const boost::shared_ptr<Fdm1dMesher> mfb(new Concentrating1dMesher((x0+x1)/2.0,f1,sizefb,std::pair<Real,Real>(x1,densityf),true));
			//const boost::shared_ptr<Fdm1dMesher> mf(new Glued1dMesher(*mfa,*mfb));

			// concentraing mesher around k to get the forward mesher
			const boost::shared_ptr<Fdm1dMesher> mf(new Concentrating1dMesher(f0,f1,sizef,std::pair<Real,Real>(strike,densityf),true));

			// volatility mesher
			const boost::shared_ptr<Fdm1dMesher> mv(new Concentrating1dMesher(v0,v1,sizev,std::pair<Real,Real>(alpha_,densityv),true));

			// uniform meshers
			//const boost::shared_ptr<Fdm1dMesher> mf(new Uniform1dMesher(f0,f1,sizef));
			//const boost::shared_ptr<Fdm1dMesher> mv(new Uniform1dMesher(v0,v1,sizev));

			std::vector<boost::shared_ptr<Fdm1dMesher>> meshers;
			meshers.push_back(mf);
			meshers.push_back(mv);
			const boost::shared_ptr<FdmMesher> mesher(new FdmMesherComposite(layout,meshers));
			// initial values
			Array rhs(mesher->layout()->size()); 
			std::vector<Real> f_;
			std::vector<Real> v_;
			for (FdmLinearOpIterator iter = layout->begin(); iter != layout->end(); ++iter) {
				Real f = mesher->location(iter,0);
				Real v = mesher->location(iter,0);
	            rhs[iter.index()] = std::max(f-strike,0.0);
				if(!iter.coordinates()[1]) f_.push_back(mesher->location(iter,0));
				if(!iter.coordinates()[0]) v_.push_back(mesher->location(iter,1));
			}
			// Boundary conditions
			FdmBoundaryConditionSet boundaries;
			boost::shared_ptr<FdmDirichletBoundary> b_dirichlet(new FdmDirichletBoundary(mesher,f1-strike,0,FdmDirichletBoundary::Upper)); // put is worth zero for forward = \infty
			boundaries.push_back(b_dirichlet);
			//test
			//std::cout << "f grid." << std::endl;
			//for(int i=0;i<f_.size();i++) std::cout << f_[i] << std::endl;
			//std::cout << "v grid." << std::endl;
			//for(int i=0;i<v_.size();i++) std::cout << v_[i] << std::endl;
			// solver
			boost::shared_ptr<FdmZabrOp> map(new FdmZabrOp(mesher,beta_,nu_,rho_,gamma_));
			FdmBackwardSolver solver(map,boundaries,boost::shared_ptr<FdmStepConditionComposite>(),FdmSchemeDesc::CraigSneyd());

			//test output result
			/*std::vector<Size> coors(2);
			Real dt=expiryTime_/100.0;
			for(int ts=0;ts<100;ts++) {
				std::ofstream os;
				std::ostringstream name;
				name << "pde_" << ts << ".txt";
				os.open(name.str());
				for(int j=0;j<f_.size();j++) {
					for(int k=0;k<v_.size();k++) {
						coors[0]=j; coors[1]=k;
						Size idx=layout->index(coors);
						os << f_[j] << " " << v_[k] << " " << rhs[idx] << std::endl;
					}
					os << std::endl;
				}
				os.close();
				solver.rollback(rhs,dt,0.0,1, ts < dampingSteps ? 1 : 0);
			}*/
			//end test

			solver.rollback(rhs,expiryTime_,0.0,steps,dampingSteps);

			// interpolate solution (this is not necessary when using concentrating meshers with required point)
			Matrix result(f_.size(),v_.size());
			for (Size j=0; j < v_.size(); ++j)
	            std::copy(rhs.begin()+j*f_.size(), rhs.begin()+(j+1)*f_.size(),result.row_begin(j));

			boost::shared_ptr<BicubicSpline> interpolation = boost::shared_ptr<BicubicSpline> (new BicubicSpline(f_.begin(), f_.end(), v_.begin(), v_.end(), result));
			interpolation->disableExtrapolation();
			return (*interpolation)(forward_,alpha_);
		}

		Real ZabrModel::x(const Real strike) const {
			return x(std::vector<Real>(1,strike))[0];
		}

		Disposable<std::vector<Real>> ZabrModel::x(const std::vector<Real>& strikes) const {

			QL_REQUIRE(strikes[0]>0.0,"strikes must be positive (" << strikes[0] << ")");
			for(std::vector<Real>::const_iterator i=strikes.begin()+1; i!=strikes.end(); i++)
				QL_REQUIRE(*i > *(i-1),"strikes must be strictly ascending (" << *(i-1) << "," << *i << ")");

			AdaptiveRungeKutta<Real> rk(1.0E-8,1.0E-5,0.0); // TODO move the magic numbers here as parameters with default values to the constructor 
			std::vector<Real> y(strikes.size()), result(strikes.size());
			std::transform(strikes.rbegin(),strikes.rend(),y.begin(),boost::lambda::bind(&ZabrModel::y,this,boost::lambda::_1));

			if(close(gamma_,1.0)) {
				for(Size m=0; m<y.size();m++) {
					Real J = std::sqrt( 1.0 + nu_*nu_ * y[m]*y[m] - 2.0*rho_*nu_* y[m] );
					result[y.size()-1-m] = std::log( (J + nu_*y[m] - rho_) / (1.0 - rho_) ) / nu_;
				}
			}
			else {
				Size ynz = std::upper_bound(y.begin(),y.end(),0.0)-y.begin();
				if(ynz > 0)
					if(close(y[ynz-1],0.0)) ynz--;
				if(ynz == y.size()) ynz--;

				for(int dir=1;dir>=-1;dir-=2) {
					Real y0=0.0, u0 = 0.0;
					for(int m=ynz + (dir==-1 ? -1 : 0) ; dir==-1 ? m>=0 : m<(int)y.size(); m+=dir) {
						Real u = rk(boost::lambda::bind(&ZabrModel::F,this,boost::lambda::_1,boost::lambda::_2),u0,y0,y[m]);
						result[y.size()-1-m] = u*pow(alpha_,1.0-gamma_);
						u0 = u;
						y0 = y[m];
					}
				}
			}

			return result;
		}

		Real ZabrModel::y(const Real strike) const {
			return close(beta_,1.0) ? std::log(forward_ / strike) * std::pow(alpha_,gamma_-2.0) : 
												( std::pow(forward_, 1.0-beta_) - std::pow(strike, 1.0-beta_) ) * std::pow(alpha_,gamma_-2.0) / (1.0- beta_) ;
		}

		Real ZabrModel::F(const Real y, const Real u) const {
			Real A = 1.0+(gamma_-2.0)*(gamma_-2.0)*nu_*nu_*y*y+2.0*rho_*(gamma_-2.0)*nu_*y;
			Real B = 2.0*rho_*(1.0-gamma_)*nu_+2.0*(1.0-gamma_)*(gamma_-2.0)*nu_*nu_* y;
			Real C = (1.0-gamma_)*(1.0-gamma_)*nu_*nu_;
			return (-B*u+std::sqrt(B*B*u*u-4.0*A*(C*u*u-1.0)))/(2.0*A);
		}

		//std::pair<Real,Real> ZabrModel::mcCallPrice(Real strike, Size timestepsPerYear, Size numberOfPaths, unsigned long seed) const {
		//	IncrementalStatistics stat;
		//	Size steps=(int)(timestepsPerYear*expiryTime_);
		//	boost::shared_ptr<MTBrownianGenerator> mtBg_(new MTBrownianGenerator(2,steps,seed));
		//	//boost::shared_ptr<SobolBrownianGenerator> mtBg_(new SobolBrownianGenerator(2,steps,SobolBrownianGenerator::Factors,seed));
		//	std::vector<double> n(2);
		//	double c1,c2,a,s,weight;
		//	double h=expiryTime_/(double)steps;
		//	//FILE* out=fopen("mc.log","a");
		//	for(Size i=0;i<numberOfPaths;i++) {
		//		a=alpha_;
		//		s=forward_;
		//		mtBg_->nextPath();
		//		weight=1.0;
		//		for(Size d=0;d<steps;d++) {
		//			weight*=mtBg_->nextStep(n); 
		//			c1=n[0];
		//			c2=c1*rho_+n[1]*sqrt(1.0-rho_*rho_);
		//			if(s<-10.0) s=-10.0; 
		//			if(a<-10.0) a=-10.0;
		//			if(s>10.0) s=10.0; 
		//			if(a>10.0) a=10.0;
		//			//std::cout << "s=" << s << " a=" << a << " ";
		//			s+=a*pow(abs(s),beta_)*c1*sqrt(h);
		//			a+=nu_*std::pow(abs(a),gamma_)*c2*sqrt(h);
		//			//a*=exp(nu_*c2*sqrt(h)-0.5*nu_*nu_*h);
		//		}
		//		stat.add(weight*std::max(s-strike,0.0));
		//		//fprintf(out,"%f;%f;%f\n",s,strike,max(s-strike,0.0));
		//		//std::cout << weight*std::max(s-strike,0.0) << std::endl;
		//	}
		//	//fclose(out);
		//	return std::pair<Real,Real>(stat.mean(),stat.standardDeviation() / std::sqrt((Real)numberOfPaths));
		//}

}
