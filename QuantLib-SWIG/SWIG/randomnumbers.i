
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

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
using QuantLib::RandomNumbers::UniformRandomGenerator;
using QuantLib::RandomNumbers::GaussianRandomGenerator;
using QuantLib::RandomNumbers::LecuyerUniformRng;
using QuantLib::RandomNumbers::KnuthUniformRng;
using QuantLib::RandomNumbers::BoxMullerGaussianRng;
using QuantLib::RandomNumbers::CLGaussianRng;
using QuantLib::RandomNumbers::ICGaussianRng;
using QuantLib::MonteCarlo::Sample;
%}

template <class T>
class Sample {
    ReturnByValue(Sample<T>);
  private:
    Sample();
  public:
    %extend {
        T value() { return self->value; }
        double weight() { return self->weight; }
    }
};

%template(SampleNumber) Sample<double>;


class UniformRandomGenerator {
  public:
    UniformRandomGenerator(long seed=0);
    Sample<double> next() const;
};

class GaussianRandomGenerator {
  public:
    GaussianRandomGenerator(long seed=0);
    Sample<double> next() const;
};

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

template<class RNG> class BoxMullerGaussianRng {
  public:
    BoxMullerGaussianRng(long seed = 0);
    Sample<double> next() const;
};

%template(BoxMullerLecuyerGaussianRng) BoxMullerGaussianRng<LecuyerUniformRng>;
%template(BoxMullerKnuthGaussianRng)   BoxMullerGaussianRng<KnuthUniformRng>;

template<class RNG> class CLGaussianRng {
  public:
    CLGaussianRng(long seed = 0);
    Sample<double> next() const;
};

%template(CentralLimitLecuyerGaussianRng) CLGaussianRng<LecuyerUniformRng>;
%template(CentralLimitKnuthGaussianRng)   CLGaussianRng<KnuthUniformRng>;

template<class RNG, class F> class ICGaussianRng {
  public:
    ICGaussianRng(long seed = 0);
    Sample<double> next() const;
};

%template(InvCumulativeLecuyerGaussianRng)
    ICGaussianRng<LecuyerUniformRng,InvCumulativeNormalDistribution>;
%template(InvCumulativeKnuthGaussianRng)
    ICGaussianRng<KnuthUniformRng,InvCumulativeNormalDistribution>;


#endif
