
/*
 Copyright (C) 2003 Ferdinando Ametrano
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_random_numbers_i
#define quantlib_random_numbers_i

%include distributions.i

#if defined(SWIGRUBY)
#pragma SWIG nowarn=314
#endif

%{
using QuantLib::Sample;

using QuantLib::LecuyerUniformRng;
using QuantLib::KnuthUniformRng;
using QuantLib::MersenneTwisterUniformRng;

typedef QuantLib::PseudoRandom::urng_type UniformRandomGenerator;

using QuantLib::CLGaussianRng;
using QuantLib::BoxMullerGaussianRng;
using QuantLib::InverseCumulativeRng;

typedef QuantLib::PseudoRandom::rng_type GaussianRandomGenerator;

using QuantLib::RandomSequenceGenerator;

typedef QuantLib::PseudoRandom::ursg_type UniformRandomSequenceGenerator;

using QuantLib::HaltonRsg;
using QuantLib::SobolRsg;

typedef QuantLib::LowDiscrepancy::ursg_type
    UniformLowDiscrepancySequenceGenerator;

using QuantLib::InverseCumulativeRsg;

typedef QuantLib::PseudoRandom::rsg_type GaussianRandomSequenceGenerator;
typedef QuantLib::LowDiscrepancy::rsg_type
    GaussianLowDiscrepancySequenceGenerator;
%}

template <class T>
class Sample {
  private:
    Sample();
  public:
    %extend {
        T value() { return self->value; }
        Real weight() { return self->weight; }
    }
};

%template(SampleNumber) Sample<Real>;
%template(SampleArray) Sample<Array>;
%template(SampleRealVector) Sample<std::vector<Real> >; 

/************* Uniform number generators *************/

class LecuyerUniformRng {
  public:
    LecuyerUniformRng(BigInteger seed=0);
    Sample<Real> next() const;
};

class KnuthUniformRng {
  public:
    KnuthUniformRng(BigInteger seed=0);
    Sample<Real> next() const;
};

class MersenneTwisterUniformRng {
  public:
    MersenneTwisterUniformRng(BigInteger seed = 0);
    Sample<Real> next() const;
};

class UniformRandomGenerator {
  public:
    UniformRandomGenerator(BigInteger seed=0);
    Sample<Real> next() const;

	%extend {
		// improve performance for direct access. faster version
		Real nextValue() const {
			return (*self).next().value;
		}
	}    
};


/************* Gaussian number generators *************/

template<class RNG> class CLGaussianRng {
  public:
    CLGaussianRng(const RNG& rng);
    Sample<Real> next() const;
};

%template(CentralLimitLecuyerGaussianRng) CLGaussianRng<LecuyerUniformRng>;
%template(CentralLimitKnuthGaussianRng)   CLGaussianRng<KnuthUniformRng>;
%template(CentralLimitMersenneTwisterGaussianRng)
    CLGaussianRng<MersenneTwisterUniformRng>;

template<class RNG> class BoxMullerGaussianRng {
  public:
    BoxMullerGaussianRng(const RNG& rng);
    Sample<Real> next() const;
};

%template(BoxMullerLecuyerGaussianRng) BoxMullerGaussianRng<LecuyerUniformRng>;
%template(BoxMullerKnuthGaussianRng)   BoxMullerGaussianRng<KnuthUniformRng>;
%template(BoxMullerMersenneTwisterGaussianRng)
    BoxMullerGaussianRng<MersenneTwisterUniformRng>;

template<class RNG, class F> class InverseCumulativeRng {
  public:
    InverseCumulativeRng(const RNG& rng);
    Sample<Real> next() const;
};

%template(MoroInvCumulativeLecuyerGaussianRng)
    InverseCumulativeRng<LecuyerUniformRng,MoroInverseCumulativeNormal>;
%template(MoroInvCumulativeKnuthGaussianRng)
    InverseCumulativeRng<KnuthUniformRng,MoroInverseCumulativeNormal>;
%template(MoroInvCumulativeMersenneTwisterGaussianRng)
    InverseCumulativeRng<MersenneTwisterUniformRng,
                         MoroInverseCumulativeNormal>;

%template(InvCumulativeLecuyerGaussianRng)
    InverseCumulativeRng<LecuyerUniformRng,InverseCumulativeNormal>;
%template(InvCumulativeKnuthGaussianRng)
    InverseCumulativeRng<KnuthUniformRng,InverseCumulativeNormal>;
%template(InvCumulativeMersenneTwisterGaussianRng)
    InverseCumulativeRng<MersenneTwisterUniformRng,InverseCumulativeNormal>;

class GaussianRandomGenerator {
  public:
    GaussianRandomGenerator(const UniformRandomGenerator& rng);
    Sample<Real> next() const;

	%extend {
		// improve performance for direct access, faster version
		Real nextValue() const {
			return (*self).next().value;
		}
	}    
};

/************* Uniform sequence generators *************/


class HaltonRsg {
  public:
    HaltonRsg(Size dimensionality);
    const Sample<std::vector<Real> >& nextSequence() const;
    Size dimension() const;
};

class SobolRsg {
  public:
    SobolRsg(Size dimensionality, BigInteger seed=0);
    const Sample<std::vector<Real> >& nextSequence() const;
    Size dimension() const;
};

template<class RNG> class RandomSequenceGenerator {
  public:
    RandomSequenceGenerator(Size dimensionality,
                            const RNG& rng);
    const Sample<std::vector<Real> >& nextSequence() const;
    Size dimension() const;
};

%template(LecuyerUniformRsg)
    RandomSequenceGenerator<LecuyerUniformRng>;
%template(KnuthUniformRsg)
    RandomSequenceGenerator<KnuthUniformRng>;
%template(MersenneTwisterUniformRsg)
    RandomSequenceGenerator<MersenneTwisterUniformRng>;

class UniformRandomSequenceGenerator {
  public:
    UniformRandomSequenceGenerator(Size dimensionality,
                                   const UniformRandomGenerator& rng);
    const Sample<std::vector<Real> >& nextSequence() const;
    Size dimension() const;
};

class UniformLowDiscrepancySequenceGenerator {
  public:
    UniformLowDiscrepancySequenceGenerator(Size dimensionality);
    const Sample<std::vector<Real> >& nextSequence() const;
    Size dimension() const;
};

/************* Gaussian sequence generators *************/

template <class U, class I>
class InverseCumulativeRsg {
  public:
    InverseCumulativeRsg(const U& uniformSequenceGenerator);
    const Sample<std::vector<Real> >& nextSequence() const;
    Size dimension() const;
};


%template(MoroInvCumulativeLecuyerGaussianRsg)
    InverseCumulativeRsg<RandomSequenceGenerator<LecuyerUniformRng>,
                         MoroInverseCumulativeNormal>;
%template(MoroInvCumulativeKnuthGaussianRsg)
    InverseCumulativeRsg<RandomSequenceGenerator<KnuthUniformRng>,
                         MoroInverseCumulativeNormal>;
%template(MoroInvCumulativeMersenneTwisterGaussianRsg)
    InverseCumulativeRsg<RandomSequenceGenerator<MersenneTwisterUniformRng>,
                         MoroInverseCumulativeNormal>;
%template(MoroInvCumulativeHaltonGaussianRsg)
    InverseCumulativeRsg<HaltonRsg,MoroInverseCumulativeNormal>;

%template(InvCumulativeLecuyerGaussianRsg)
    InverseCumulativeRsg<RandomSequenceGenerator<LecuyerUniformRng>,
                         InverseCumulativeNormal>;
%template(InvCumulativeKnuthGaussianRsg)
    InverseCumulativeRsg<RandomSequenceGenerator<KnuthUniformRng>,
                         InverseCumulativeNormal>;
%template(InvCumulativeMersenneTwisterGaussianRsg)
    InverseCumulativeRsg<RandomSequenceGenerator<MersenneTwisterUniformRng>,
                         InverseCumulativeNormal>;
%template(InvCumulativeHaltonGaussianRsg)
    InverseCumulativeRsg<HaltonRsg,InverseCumulativeNormal>;

class GaussianRandomSequenceGenerator {
  public:
    GaussianRandomSequenceGenerator(
        const UniformRandomSequenceGenerator& uniformSequenceGenerator);
    const Sample<std::vector<Real> >& nextSequence() const;
    Size dimension() const;
};

class GaussianLowDiscrepancySequenceGenerator {
  public:
    GaussianLowDiscrepancySequenceGenerator(
        const UniformLowDiscrepancySequenceGenerator& u);
    const Sample<std::vector<Real> >& nextSequence() const;
    Size dimension() const;
};


#endif
