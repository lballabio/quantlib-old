
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005 StatPro Italia srl
 Copyright (C) 2008 Tito Ingargiola
 Copyright (C) 2014 Felix Lee       

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

#ifndef quantlib_montecarlo_tools_i
#define quantlib_montecarlo_tools_i

%include <boost_shared_ptr.i>
%include stochasticprocess.i
%include linearalgebra.i
%include randomnumbers.i
%include types.i

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("get-covariance") getCovariance;
#endif
%inline %{
Matrix getCovariance(const Array& volatilities, const Matrix& correlations) {
    return QuantLib::getCovariance(volatilities.begin(),
                                   volatilities.end(),
                                   correlations);
}
%}

%{
using QuantLib::Path;
%}

#if defined(SWIGRUBY)
%mixin Path "Enumerable";
#endif
class Path {
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    %rename(__len__) length;
    #endif
  private:
    Path();
  public:
    Size length() const;
    Real value(Size i) const;
    Real front() const;
    Real back() const;
    Time time(Size i) const;
    %extend {
        #if defined(SWIGPYTHON) || defined(SWIGRUBY)
        Real __getitem__(Integer i) {
            Integer size_ = Integer(self->length());
            if (i>=0 && i<size_) {
                return (*self)[i];
            } else if (i<0 && -i<=size_) {
                return (*self)[size_+i];
            } else {
                throw std::out_of_range("path index out of range");
            }
        }
        #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
        Real ref(Size i) {
            if (i<self->length()) {
                return (*self)[i];
            } else {
                throw std::out_of_range("path index out of range");
            }
        }
        #endif
        #if defined(SWIGRUBY)
        void each() {
            for (Size i=0; i<self->length(); i++)
                rb_yield(rb_float_new((*self)[i]));
        }
        #elif defined(SWIGMZSCHEME)
        void for_each(Scheme_Object* proc) {
            for (Size i=0; i<self->length(); ++i) {
                Scheme_Object* x = scheme_make_double((*self)[i]);
                scheme_apply(proc,1,&x);
            }
        }
        #elif defined(SWIGGUILE)
        void for_each(SCM proc) {
            for (Size i=0; i<self->length(); ++i) {
                SCM x = gh_double2scm((*self)[i]);
                gh_call1(proc,x);
            }
        }
        #endif
    }
};

%{
typedef QuantLib::PathGenerator<GaussianRandomSequenceGenerator>
    GaussianPathGenerator;
%}
%template(SamplePath) Sample<Path>;
class GaussianPathGenerator {
  public:
    %extend {
        GaussianPathGenerator(const StochasticProcess1DPtr& process,
                              Time length, Size steps,
                              const GaussianRandomSequenceGenerator& rsg,
                              bool brownianBridge) {
            boost::shared_ptr<StochasticProcess1D> process1d =
                boost::dynamic_pointer_cast<StochasticProcess1D>(process);
            return new GaussianPathGenerator(process1d,length,steps,
                                             rsg,brownianBridge);
        }
    }
    Sample<Path> next() const;
    Sample<Path> antithetic() const;
};

%{
typedef QuantLib::PathGenerator<GaussianLowDiscrepancySequenceGenerator>
    GaussianSobolPathGenerator;
%}
class GaussianSobolPathGenerator {
  public:
    %extend {
        GaussianSobolPathGenerator(
                           const StochasticProcess1DPtr& process,
                           Time length, Size steps,
                           const GaussianLowDiscrepancySequenceGenerator& rsg,
                           bool brownianBridge) {
            boost::shared_ptr<StochasticProcess1D> process1d =
                boost::dynamic_pointer_cast<StochasticProcess1D>(process);
            return new GaussianSobolPathGenerator(process1d,length,steps,
                                                  rsg,brownianBridge);
        }
    }
    Sample<Path> next() const;
    Sample<Path> antithetic() const;
};


%{
using QuantLib::MultiPath;
%}

class MultiPath {
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    %rename(__len__)        pathSize;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("length")       pathSize;
    %rename("asset-number") assetNumber;
    #endif
  private:
    MultiPath();
  public:
    Size pathSize() const;
    Size assetNumber() const;
	Path& at(Size j);

    %extend {
        #if defined(SWIGPYTHON) || defined(SWIGRUBY)
        const Path& __getitem__(Integer i) {
            Integer assets_ = Integer(self->assetNumber());
            if (i>=0 && i<assets_) {
                return (*self)[i];
            } else if (i<0 && -i<=assets_) {
                return (*self)[assets_+i];
            } else {
                throw std::out_of_range("multi-path index out of range");
            }
        }
        #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
        Real ref(Size i, Size j) {
            if (i<self->assetNumber() && j<self->pathSize()) {
                return (*self)[i][j];
            } else {
                throw std::out_of_range("multi-path index out of range");
            }
        }
        #endif
        #if defined(SWIGRUBY)
        void each_path() {
            for (Size i=0; i<self->assetNumber(); i++)
                rb_yield(SWIG_NewPointerObj(&((*self)[i]),
                                            $descriptor(Path *), 0));
        }
        void each_step() {
            for (Size j=0; j<self->pathSize(); j++) {
                VALUE v = rb_ary_new2(self->assetNumber());
                for (Size i=0; i<self->assetNumber(); i++)
                    rb_ary_store(v,i,rb_float_new((*self)[i][j]));
                rb_yield(v);
            }
        }
        #elif defined(SWIGMZSCHEME)
        void for_each_path(Scheme_Object* proc) {
            for (Size i=0; i<self->assetNumber(); i++) {
                Scheme_Object* x =
                    SWIG_NewPointerObj(&((*self)[i]), $descriptor(Path *), 0);
                scheme_apply(proc,1,&x);
            }
        }
        void for_each_step(Scheme_Object* proc) {
            for (Size j=0; j<self->pathSize(); j++) {
                Scheme_Object* v = scheme_make_vector(self->assetNumber(),
                                                      scheme_undefined);
                Scheme_Object** els = SCHEME_VEC_ELS(v);
                for (Size i=0; i<self->assetNumber(); i++)
                    els[i] = scheme_make_double((*self)[i][j]);
                scheme_apply(proc,1,&v);
            }
        }
        #elif defined(SWIGGUILE)
        void for_each_path(SCM proc) {
            for (Size i=0; i<self->assetNumber(); ++i) {
                SCM x =
                    SWIG_NewPointerObj(&((*self)[i]), $descriptor(Path *), 0);
                gh_call1(proc,x);
            }
        }
        void for_each_step(SCM proc) {
            for (Size j=0; j<self->pathSize(); ++j) {
                SCM v = gh_make_vector(gh_long2scm(self->assetNumber()),
                                       SCM_UNSPECIFIED);
                for (Size i=0; i<self->assetNumber(); i++) {
                    gh_vector_set_x(v,gh_long2scm(i),
                                    gh_double2scm((*self)[i][j]));
                }
                gh_call1(proc,v);
            }
        }
        #endif
    }
};

%{
typedef QuantLib::MultiPathGenerator<GaussianRandomSequenceGenerator>
    GaussianMultiPathGenerator;
%}
%template(SampleMultiPath) Sample<MultiPath>;
class GaussianMultiPathGenerator {
  public:
    %extend {
      GaussianMultiPathGenerator(
                   const boost::shared_ptr<StochasticProcess>& process,
                   const std::vector<Time>& times,
                   const GaussianRandomSequenceGenerator& generator,
                   bool brownianBridge = false) {
          return new GaussianMultiPathGenerator(process,
                                                QuantLib::TimeGrid(
                                                    times.begin(),
                                                    times.end()),
                                                generator,
                                                brownianBridge);
      }
    }
    Sample<MultiPath> next() const;
	Sample<MultiPath> antithetic() const;
};

/************* Wrap the boost::shared_ptr **********/
%shared_ptr(PathGeneratorPseudoRandom)
%shared_ptr(PathPricer<Path>)

%{
using QuantLib::PathGenerator;
typedef QuantLib::PathGenerator<PseudoRandom::rsg_type> PathGeneratorPseudoRandom;

using QuantLib::PathPricer;
using QuantLib::Path;

using QuantLib::MonteCarloModel;
using QuantLib::SingleVariate;
using QuantLib::PseudoRandom;
typedef QuantLib::MonteCarloModel<SingleVariate,PseudoRandom>
    MonteCarloModelSingleVariatePseudoRandom;
%}

/************* Path generators *********************/
    class PathGeneratorPseudoRandom {
      public:
        typedef Sample<Path> sample_type;
        // constructors
        PathGeneratorPseudoRandom(const boost::shared_ptr<StochasticProcess>&,
                      Time length,
                      Size timeSteps,
                      const InverseCumulativeRsg<RandomSequenceGenerator<MersenneTwisterUniformRng>,
                                                 InverseCumulativeNormal>& generator,
                      bool brownianBridge);
        PathGeneratorPseudoRandom(const boost::shared_ptr<StochasticProcess>&,
                      const TimeGrid& timeGrid,
                      const InverseCumulativeRsg<RandomSequenceGenerator<MersenneTwisterUniformRng>,
                                                 InverseCumulativeNormal>& generator,
                      bool brownianBridge);
        //! \name inspectors
        //@{
        const sample_type& next() const;
        const sample_type& antithetic() const;
        Size size() const { return dimension_; }
        const TimeGrid& timeGrid() const { return timeGrid_; }
        //@}
      private:
        const sample_type& next(bool antithetic) const;
        bool brownianBridge_;
        InverseCumulativeRsg<RandomSequenceGenerator<MersenneTwisterUniformRng>,
                             InverseCumulativeNormal> generator_;
        Size dimension_;
        TimeGrid timeGrid_;
        boost::shared_ptr<StochasticProcess1D> process_;
        mutable sample_type next_;
        mutable std::vector<Real> temp_;
        BrownianBridge bb_;
    };

/************* Path pricer *********************/
%feature("director") PathPricer;

    template<class PathType, class ValueType=Real>
    class PathPricer {
      public:
        virtual ~PathPricer() {}
        virtual ValueType operator()(const PathType& path) const=0;
    };

%template(PathPricerPath) PathPricer<Path>;

/************* Monte Carlo Model **************/
    class MonteCarloModelSingleVariatePseudoRandom {
      public:
        typedef PathGeneratorPseudoRandom path_generator_type;
        typedef PathPricer<Path> path_pricer_type;
        typedef typename path_generator_type::sample_type sample_type;
        typedef typename path_pricer_type::result_type result_type;
        typedef Statistics stats_type;
        // constructor
        MonteCarloModelSingleVariatePseudoRandom(
                  const boost::shared_ptr<path_generator_type>& pathGenerator,
                  const boost::shared_ptr<path_pricer_type>& pathPricer,
                  const stats_type& sampleAccumulator,
                  bool antitheticVariate,
                  const boost::shared_ptr<path_pricer_type>& cvPathPricer
                        = boost::shared_ptr<path_pricer_type>(),
                  result_type cvOptionValue = result_type(),
                  const boost::shared_ptr<path_generator_type>& cvPathGenerator
                        = boost::shared_ptr<path_generator_type>())
        : pathGenerator_(pathGenerator), pathPricer_(pathPricer),
          sampleAccumulator_(sampleAccumulator),
          isAntitheticVariate_(antitheticVariate),
          cvPathPricer_(cvPathPricer), cvOptionValue_(cvOptionValue),
          cvPathGenerator_(cvPathGenerator) {
            if (!cvPathPricer_)
                isControlVariate_ = false;
            else
                isControlVariate_ = true;
        }
        void addSamples(Size samples);
        const stats_type& sampleAccumulator(void) const;
      private:
        boost::shared_ptr<path_generator_type> pathGenerator_;
        boost::shared_ptr<path_pricer_type> pathPricer_;
        stats_type sampleAccumulator_;
        bool isAntitheticVariate_;
        boost::shared_ptr<path_pricer_type> cvPathPricer_;
        result_type cvOptionValue_;
        bool isControlVariate_;
        boost::shared_ptr<path_generator_type> cvPathGenerator_;
    };

#endif
