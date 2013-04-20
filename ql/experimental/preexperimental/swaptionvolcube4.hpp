/*! \file swaptionvolcube4.hpp, based on swaptionvolcube1.hpp, but with hybrid sabr / rbs smile
    \brief Swaption volatility cube, fit-early-interpolate-later approach
*/

#ifndef quantlib_swaption_volcube_4_h
#define quantlib_swaption_volcube_4_h

#include <ql/termstructures/volatility/swaption/swaptionvolcube.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolcube1.hpp>
#include <ql/math/interpolations/flatextrapolation2d.hpp>
#include <ql/math/interpolations/bilinearinterpolation.hpp>
#include <ql/math/matrix.hpp>
#include <ql/quote.hpp>
#include <sabrrbssmilesection.hpp>
#include <wgz_utilities.hpp>

namespace QuantLib {

    class Interpolation2D;
    class EndCriteria;
    class OptimizationMethod;

    class SwaptionVolCube4 : public SwaptionVolatilityCube {
		class Cube {
          public:
            Cube() {}
            Cube(const std::vector<Date>& optionDates,
                 const std::vector<Period>& swapTenors,
                 const std::vector<Time>& optionTimes,
                 const std::vector<Time>& swapLengths,
                 Size nLayers,
                 bool extrapolation = true);
            Cube& operator=(const Cube& o);
            Cube(const Cube&);
            virtual ~Cube() {}
            void setElement(Size IndexOfLayer,
                            Size IndexOfRow,
                            Size IndexOfColumn,
                            Real x);
            void setPoints(const std::vector<Matrix>& x);
            void setPoint(const Date& optionDate,
                          const Period& swapTenor,
                          const Time optionTime,
                          const Time swapLengths,
                          const std::vector<Real>& point);
            void setLayer(Size i,
                          const Matrix& x);
            void expandLayers(Size i,
                              bool expandOptionTimes,
                              Size j,
                              bool expandSwapLengths);
            const std::vector<Date>& optionDates() const {
                return optionDates_;
            }
            const std::vector<Period>& swapTenors() const {
                return swapTenors_;
            }
            const std::vector<Time>& optionTimes() const;
            const std::vector<Time>& swapLengths() const;
            const std::vector<Matrix>& points() const;
            std::vector<Real> operator()(const Time optionTime,
                                         const Time swapLengths) const;
            void updateInterpolators()const;
            Matrix browse() const;
          private:
            std::vector<Time> optionTimes_, swapLengths_;
            std::vector<Date> optionDates_;
            std::vector<Period> swapTenors_;
            Size nLayers_;
            std::vector<Matrix> points_;
            mutable std::vector<Disposable<Matrix> > transposedPoints_;
            bool extrapolation_;
            mutable std::vector< boost::shared_ptr<Interpolation2D> > interpolators_;
         };
      public:
        SwaptionVolCube4(
            const Handle<SwaptionVolatilityStructure>& atmVolStructure,
            const std::vector<Period>& optionTenors,
            const std::vector<Period>& swapTenors,
            const std::vector<Spread>& strikeSpreads,
            const std::vector<std::vector<Handle<Quote> > >& volSpreads,
            const boost::shared_ptr<SwapIndex>& swapIndexBase,
            const boost::shared_ptr<SwapIndex>& shortSwapIndexBase,
            bool vegaWeightedSmileFit,
            const std::vector<std::vector<Handle<Quote> > >& parametersGuess,
            const std::vector<bool>& isParameterFixed,
            bool isAtmCalibrated,
			const double leftCoreSpread, const double rightCoreSpread,
			const int checkAfGridPoints,
			const Real acceptRmse = 0.0020, const int haltonIterations = 150,
            const Real rejectRmse = 0.02,
			const Real h = 1.0E-4, const Real minStrike = 0.0001,
            const boost::shared_ptr<EndCriteria>& endCriteria
                = boost::shared_ptr<EndCriteria>(),
            const boost::shared_ptr<OptimizationMethod>& optMethod
                = boost::shared_ptr<OptimizationMethod>());
        //! \name LazyObject interface
        //@{
        void performCalculations() const;
        //@}
		//! \name VolatilityTermStructure interface
        //@{
        Rate minStrike() const { return 0.0; }
        Rate maxStrike() const { return 10.0; }
        //@}
        //! \name SwaptionVolatilityCube interface
        //@{
  	    boost::shared_ptr<SmileSection> smileSectionImpl(
                                              const Date& optionDate,
                                              const Period& swapTenor) const;
        boost::shared_ptr<SmileSection> smileSectionImpl(
                                              Time optionTime,
                                              Time swapLength) const;
        //@}
        //! \name Other inspectors
        //@{
        const Matrix& marketVolCube(Size i) const {
            return marketVolCube_.points()[i];
        }
        Matrix sparseSabrRbsParameters()const;
        Matrix denseSabrRbsParameters() const;
        Matrix marketVolCube() const;
        Matrix volCubeAtmCalibrated() const;
        //@}
		void sabrRbsCalibrationSection(const Cube& marketVolCube,
									Cube& parametersCube,
                                    const Period& swapTenor) const;
        void recalibration(const std::vector<Real>& beta,
                           const Period& swapTenor);
		void recalibrationNu(const std::vector<Real>& nu,
                           const Period& swapTenor);
		void updateAfterRecalibration();
     protected:
        void registerWithParametersGuess();
        boost::shared_ptr<SmileSection> smileSection(
                                    Time optionTime,
                                    Time swapLength,
									const Cube& sabrParametersCube) const;
		boost::shared_ptr<SmileSection> smileSection(
                                    const Date& optionDate,
                                    const Period& swapTenor,
									const Cube& sabrParametersCube) const;
		Cube sabrRbsCalibration(const Cube& marketVolCube) const;
        void fillVolatilityCube() const;
        void createSparseSmiles() const;
        std::vector<Real> spreadVolInterpolation(const Date& atmOptionDate,
                                                 const Period& atmSwapTenor) const;
      private:
        mutable Cube marketVolCube_;
        mutable Cube volCubeAtmCalibrated_;
        mutable Cube sparseParameters_;
        mutable Cube denseParameters_;
        mutable std::vector< std::vector<boost::shared_ptr<SmileSection> > >
                                                                sparseSmiles_;
        std::vector<std::vector<Handle<Quote> > > parametersGuessQuotes_;
        mutable Cube parametersGuess_;
        std::vector<bool> isParameterFixed_;
        bool isAtmCalibrated_;
		double leftCoreSpread_,rightCoreSpread_;
		int checkAfGridPoints_;
        const boost::shared_ptr<EndCriteria> endCriteria_;
        const Real acceptRmse_,rejectRmse_,h_,minStrike_;
		const int haltonIterations_;
        const boost::shared_ptr<OptimizationMethod> optMethod_;
    };

}

#endif
