
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

%{
using QuantLib::RandomNumbers::UniformRandomGenerator;
using QuantLib::RandomNumbers::GaussianRandomGenerator;
using QuantLib::RandomNumbers::LecuyerUniformRng;
using QuantLib::RandomNumbers::KnuthUniformRng;
typedef QuantLib::MonteCarlo::Sample<double> SampleNumber;
%}

class SampleNumber {
  public:
    %readonly
    double value;
    double weight;
    %readwrite
  private:
    SampleNumber();
};

class UniformRandomGenerator {
  public:
    UniformRandomGenerator(long seed=0);
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    SampleNumber next() const;
    #endif
};

#if defined(SWIGMZSCHEME)
%addmethods UniformRandomGenerator {
    SampleNumber* next() {
        return new SampleNumber(self->next());
    }
}
#endif

class GaussianRandomGenerator {
  public:
    GaussianRandomGenerator(long seed=0);
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    SampleNumber next() const;
    #endif
};

#if defined(SWIGMZSCHEME)
%addmethods GaussianRandomGenerator {
    SampleNumber* next() {
        return new SampleNumber(self->next());
    }
}
#endif

class LecuyerUniformRng {
  public:
    LecuyerUniformRng(long seed=0);
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    SampleNumber next() const;
    #endif
};

#if defined(SWIGMZSCHEME)
%addmethods LecuyerUniformRng {
    SampleNumber* next() {
        return new SampleNumber(self->next());
    }
}
#endif

class KnuthUniformRng {
  public:
    KnuthUniformRng(long seed=0);
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    SampleNumber next() const;
    #endif
};

#if defined(SWIGMZSCHEME)
%addmethods KnuthUniformRng {
    SampleNumber* next() {
        return new SampleNumber(self->next());
    }
}
#endif


#endif
