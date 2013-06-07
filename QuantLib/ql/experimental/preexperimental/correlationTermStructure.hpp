/*! \file correlationTermStructure.hpp
    \brief Term structure for integrated correlations.
*/

#ifndef quantlib_correlation_termstructure_hpp
#define quantlib_correlation_termstructure_hpp

#include <ql/quantlib.hpp>

namespace QuantLib {

    //! correlation term structure
    class CorrelationTermStructure {
      public:
        //! Constructor
        CorrelationTermStructure(const Date& referenceDate,
							   const std::vector<Period>& maturities,
							   const std::vector<double>& correlations,
							   const Calendar& calendar = Calendar(),
							   const BusinessDayConvention& bdc = BusinessDayConvention(),
							   bool endOfMonth=false,
							   bool leftFlat=true, bool rightFlat=true,
                               const DayCounter& dayCounter = DayCounter());

		Time timeFromReference(const Date& date) const;
		double correlation(Date d, bool allowExtrapolation);
		double correlation(Period p, bool allowExtrapolation);
		bool setPillarCorrelation(const Period& p, double correlation);
		bool setPillarCorrelation(int i, double correlation);
		std::vector<double> pillarCorrelations();
		double pillarCorrelation(int i);
		int numberOfPillars();

	  private:
		  Date referenceDate_;
		  Calendar calendar_;
		  DayCounter dayCounter_;
		  std::vector<Period> maturities_;
		  std::vector<double> correlations_;
		  std::vector<Date> dates_;
		  std::vector<double> times_;
		  double minTime_,maxTime_;
		  Interpolation interpolation_;
		  bool endOfMonth_;
		  bool leftFlat_,rightFlat_;
		  BusinessDayConvention bdc_;
		  
		  
	};

}

#endif
