
/*
 Copyright (C) 2003 Ferdinando Ametrano
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_random_numbers_i
#define quantlib_random_numbers_i

%include distributions.i

%{
using QuantLib::Sample;

using QuantLib::LecuyerUniformRng;
using QuantLib::KnuthUniformRng;
using QuantLib::MersenneTwisterUniformRng;

typedef QuantLib::PseudoRandom::urng_type UniformRandomGenerator;

using QuantLib::CLGaussianRng;
using QuantLib::BoxMullerGaussianRng;
using QuantLib::ICGaussianRng;

typedef QuantLib::PseudoRandom::rng_type GaussianRandomGenerator;

using QuantLib::RandomSequenceGenerator;

typedef QuantLib::PseudoRandom::ursg_type UniformRandomSequenceGenerator;

using QuantLib::HaltonRsg;
using QuantLib::SobolRsg;

typedef QuantLib::LowDiscrepancy::ursg_type 
    UniformLowDiscrepancySequenceGenerator;

using QuantLib::ICGaussianRsg;

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
        double weight() { return self->weight; }
    }
};

%template(SampleNumber) Sample<double>;
%template(SampleArray) Sample<Array>;

/************* Uniform number generators *************/

class LecuyerUniformRng {
  public:
    LecuyerUniformRng(long seed=0);
    Sample<double> next() const;
};

class KnuthUniformRng {
    KnuthUniformRng(long seed=0);
    Sample<double> next() const;
};

class MersenneTwisterUniformRng {
    MersenneTwisterUniformRng(long seed=0);
    Sample<double> next() const;
};

class UniformRandomGenerator {
  public:
    UniformRandomGenerator(long seed=0);
    Sample<double> next() const;
};


/************* Gaussian number generators *************/

template<class RNG> class CLGaussianRng {
  public:
    CLGaussianRng(const RNG& rng);
    Sample<double> next() const;
};

%template(CentralLimitLecuyerGaussianRng) CLGaussianRng<LecuyerUniformRng>;
%template(CentralLimitKnuthGaussianRng)   CLGaussianRng<KnuthUniformRng>;
%template(CentralLimitMersenneTwisterGaussianRng)
    CLGaussianRng<MersenneTwisterUniformRng>;

template<class RNG> class BoxMullerGaussianRng {
  public:
    BoxMullerGaussianRng(const RNG& rng);
    Sample<double> next() const;
};

%template(BoxMullerLecuyerGaussianRng) BoxMullerGaussianRng<LecuyerUniformRng>;
%template(BoxMullerKnuthGaussianRng)   BoxMullerGaussianRng<KnuthUniformRng>;
%template(BoxMullerMersenneTwisterGaussianRng)
    BoxMullerGaussianRng<MersenneTwisterUniformRng>;

template<class RNG, class F> class ICGaussianRng {
  public:
    ICGaussianRng(const RNG& rng);
    Sample<double> next() const;
};

%template(MoroInvCumulativeLecuyerGaussianRng)
    ICGaussianRng<LecuyerUniformRng,MoroInverseCumulativeNormal>;
%template(MoroInvCumulativeKnuthGaussianRng)
    ICGaussianRng<KnuthUniformRng,MoroInverseCumulativeNormal>;
%template(MoroInvCumulativeMersenneTwisterGaussianRng)
    ICGaussianRng<MersenneTwisterUniformRng,MoroInverseCumulativeNormal>;

%template(InvCumulativeLecuyerGaussianRng)
    ICGaussianRng<LecuyerUniformRng,InverseCumulativeNormal>;
%template(InvCumulativeKnuthGaussianRng)
    ICGaussianRng<KnuthUniformRng,InverseCumulativeNormal>;
%template(InvCumulativeMersenneTwisterGaussianRng)
    ICGaussianRng<MersenneTwisterUniformRng,InverseCumulativeNormal>;

class GaussianRandomGenerator {
  public:
    GaussianRandomGenerator(const UniformRandomGenerator& rng);
    Sample<double> next() const;
};

/************* Uniform sequence generators *************/


class HaltonRsg {
  public:
    HaltonRsg(long dimensionality);
    const Sample<Array>& nextSequence() const;
    Size dimension() const;
};

class SobolRsg {
  public:
    SobolRsg(long dimensionality, long seed=0);
    const Sample<Array>& nextSequence() const;
    Size dimension() const;
};

template<class RNG> class RandomSequenceGenerator {
  public:
    RandomSequenceGenerator(long dimensionality,
                            const RNG& rng);
    const Sample<Array>& nextSequence() const;
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
    UniformRandomSequenceGenerator(long dimensionality,
                                   const UniformRandomGenerator& rng);
    const Sample<Array>& nextSequence() const;
    Size dimension() const;
};

class UniformLowDiscrepancySequenceGenerator {
  public:
    UniformLowDiscrepancySequenceGenerator(long dimensionality);
    const Sample<Array>& nextSequence() const;
    Size dimension() const;
};

/************* Gaussian sequence generators *************/
template <class U, class I>
class ICGaussianRsg {
  public:
    ICGaussianRsg(const U& uniformSequenceGenerator);
    const Sample<Array>& nextSequence() const;
    Size dimension() const;
};


%template(MoroInvCumulativeLecuyerGaussianRsg)
    ICGaussianRsg<RandomSequenceGenerator<LecuyerUniformRng>,
                  MoroInverseCumulativeNormal>;
%template(MoroInvCumulativeKnuthGaussianRsg)
    ICGaussianRsg<RandomSequenceGenerator<KnuthUniformRng>,
                  MoroInverseCumulativeNormal>;
%template(MoroInvCumulativeMersenneTwisterGaussianRsg)
    ICGaussianRsg<RandomSequenceGenerator<MersenneTwisterUniformRng>,
                  MoroInverseCumulativeNormal>;
%template(MoroInvCumulativeHaltonGaussianRsg)
    ICGaussianRsg<HaltonRsg,MoroInverseCumulativeNormal>;

%template(InvCumulativeLecuyerGaussianRsg)
    ICGaussianRsg<RandomSequenceGenerator<LecuyerUniformRng>,
                  InverseCumulativeNormal>;
%template(InvCumulativeKnuthGaussianRsg)
    ICGaussianRsg<RandomSequenceGenerator<KnuthUniformRng>,
                  InverseCumulativeNormal>;
%template(InvCumulativeMersenneTwisterGaussianRsg)
    ICGaussianRsg<RandomSequenceGenerator<MersenneTwisterUniformRng>,
                  InverseCumulativeNormal>;
%template(InvCumulativeHaltonGaussianRsg)
    ICGaussianRsg<HaltonRsg,InverseCumulativeNormal>;

class GaussianRandomSequenceGenerator {
  public:
    GaussianRandomSequenceGenerator(
        const UniformRandomSequenceGenerator& uniformSequenceGenerator);
    const Sample<Array>& nextSequence() const;
    Size dimension() const;
};

class GaussianLowDiscrepancySequenceGenerator {
  public:
    GaussianLowDiscrepancySequenceGenerator(
        const UniformLowDiscrepancySequenceGenerator& u);
    const Sample<Array>& nextSequence() const;
    Size dimension() const;
};


#endif
