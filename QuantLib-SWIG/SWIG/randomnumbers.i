
/*
 Copyright (C) 2003 Ferdinando Ametrano
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

// $Id$

#ifndef quantlib_random_numbers_i
#define quantlib_random_numbers_i

%include distributions.i

%{
using QuantLib::MonteCarlo::Sample;

using QuantLib::RandomNumbers::LecuyerUniformRng;
using QuantLib::RandomNumbers::KnuthUniformRng;
using QuantLib::RandomNumbers::UniformRandomGenerator;

using QuantLib::RandomNumbers::BoxMullerGaussianRng;
using QuantLib::RandomNumbers::CLGaussianRng;
using QuantLib::RandomNumbers::ICGaussianRng;
using QuantLib::RandomNumbers::GaussianRandomGenerator;

using QuantLib::RandomNumbers::RandomSequenceGenerator;
using QuantLib::RandomNumbers::HaltonRsg;

using QuantLib::RandomNumbers::ICGaussianRsg;
using QuantLib::RandomNumbers::LecuyerUniformRsg;
using QuantLib::RandomNumbers::KnuthUniformRsg;
using QuantLib::RandomNumbers::UniformRandomSequenceGenerator;
using QuantLib::RandomNumbers::UniformLowDiscrepancySequenceGenerator;
using QuantLib::RandomNumbers::GaussianRandomSequenceGenerator;
using QuantLib::RandomNumbers::GaussianLowDiscrepancySequenceGenerator;
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
  public:
    KnuthUniformRng(long seed=0);
    Sample<double> next() const;
};

class UniformRandomGenerator {
  public:
    UniformRandomGenerator(long seed=0);
    Sample<double> next() const;
};

/************* Gaussian number generators *************/

template<class RNG> class BoxMullerGaussianRng {
  public:
    BoxMullerGaussianRng(const RNG& rng);
    BoxMullerGaussianRng(long seed = 0);
    Sample<double> next() const;
};

%template(BoxMullerLecuyerGaussianRng) BoxMullerGaussianRng<LecuyerUniformRng>;
%template(BoxMullerKnuthGaussianRng)   BoxMullerGaussianRng<KnuthUniformRng>;

template<class RNG> class CLGaussianRng {
  public:
    CLGaussianRng(const RNG& rng);
    CLGaussianRng(long seed = 0);
    Sample<double> next() const;
};

%template(CentralLimitLecuyerGaussianRng) CLGaussianRng<LecuyerUniformRng>;
%template(CentralLimitKnuthGaussianRng)   CLGaussianRng<KnuthUniformRng>;

template<class RNG, class F> class ICGaussianRng {
  public:
    ICGaussianRng(const RNG& rng);
    ICGaussianRng(long seed = 0);
    Sample<double> next() const;
};

%template(InvCumulativeLecuyerGaussianRng)
    ICGaussianRng<LecuyerUniformRng,InverseCumulativeNormal>;
%template(InvCumulativeKnuthGaussianRng)
    ICGaussianRng<KnuthUniformRng,InverseCumulativeNormal>;
%template(MoroInvCumulativeLecuyerGaussianRng)
    ICGaussianRng<LecuyerUniformRng,MoroInverseCumulativeNormal>;
%template(MoroInvCumulativeKnuthGaussianRng)
    ICGaussianRng<KnuthUniformRng,MoroInverseCumulativeNormal>;

class GaussianRandomGenerator {
  public:
    GaussianRandomGenerator(const UniformRandomGenerator& rng);
    GaussianRandomGenerator(long seed=0);
    Sample<double> next() const;
};

/************* Uniform sequence generators *************/

class HaltonRsg {
  public:
    HaltonRsg(long dimensionality);
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
    ICGaussianRsg<LecuyerUniformRsg,MoroInverseCumulativeNormal>;
%template(InvCumulativeLecuyerGaussianRsg)
    ICGaussianRsg<LecuyerUniformRsg,InverseCumulativeNormal>;
%template(MoroInvCumulativeKnuthGaussianRsg)
    ICGaussianRsg<KnuthUniformRsg,MoroInverseCumulativeNormal>;
%template(InvCumulativeKnuthGaussianRsg)
    ICGaussianRsg<KnuthUniformRsg,InverseCumulativeNormal>;
%template(MoroInvCumulativeHaltonGaussianRsg)
    ICGaussianRsg<HaltonRsg,MoroInverseCumulativeNormal>;
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
        const UniformLowDiscrepancySequenceGenerator& uniformSequenceGenerator);
    const Sample<Array>& nextSequence() const;
    Size dimension() const;
};

#endif
