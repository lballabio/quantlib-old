#include <ql/quantlib.hpp>
#include <math.h>

namespace QuantLib {

	class HW2FAdj {

	public:

		HW2FAdj(const double a, const double b, 
		      	const std::vector<double>& rho,
				const std::vector<double>& times,
				const std::vector<double>& sigma,
				const std::vector<double>& eta,
				boost::shared_ptr<YieldTermStructure> yieldTS,
				const Size hermitePointsInner=16,
				const Size hermitePointsOuter=16,
				const Size mcpaths=500,
				const Size mcseed=1234);
			
		// payoff is max(w*(SR1-SR2-Strike),0) (digital = false)
		//           1.0 if w*(SR1-SR2-Strike)>=0, 0.0 otherwise (digital = true)
		// mode: 0 = cms spread caplet npv, 1 = cms Spread digital npv, 2 = adjusted swap rate 1, 3 = adjusted swap rate 2 
		// mode: 4 = swaption (only swap 1 is used)
		double spreadOptionPrice(const Size fixing, const Size payment,const double strike,
			const Size swapLength1, const Size swapLength2, const Size swapPayFreq, const double w=1.0, const Size mode=0, 
			const double adjuster=1.0, const double lastCorr=2.0, const bool useMC=false);
		
		// only for digitals and w=1 (digital call = dc)
		double spreadOptionPriceDC(const Size fixing, const Size payment,const double strike,
			const Size swapLength1, const Size swapLength2, const Size swapPayFreq, const double w=1.0, const Size mode=0, 
			const double adjuster=1.0, const bool useMC=false);
	
		double expectationX(const Size index, const Size payIndex);
		double expectationY(const Size index, const Size payIndex);
		double varianceX(const Size index);
		double varianceY(const Size index);
		double covarianceXY(const Size index);
		double correlationXY(const Size index);
		double zeroCouponPrice(const Size startIndex, const Size endIndex, const double x, const double y, const double adjuster=1.0,const double startTimeMin=0.0, const double endTimeMax=500.0);
		double swapRate(const Size startIndex, const Size swapLength, const Size payFreq, const double x, const double y,const double adjuster=1.0);
		double swapAnnuity(const Size startIndex, const Size swapLength, const Size payFreq, const double x, const double y,const double adjuster=1.0);
		double varianceI(const int index0, const int index1,const double adjuster=1.0,const double startTimeMin=0.0, const double endTimeMax=500.0);
		
		GaussHermiteIntegration *innerInt_;
		//SimpsonIntegral *innerInt_;
		GaussHermiteIntegration *outerInt_;

	private:

		double a_, b_;
		std::vector<double> times_, sigma_, eta_,rho_;
		boost::shared_ptr<YieldTermStructure> yieldTS_;
		Size mcpaths_,mcseed_;
		
	};

	class PayOff {
	public:
		PayOff(HW2FAdj* hw, const Size fixing, const Size payment, const double strike,
			const Size swapLength1, const Size swapLength2, const Size swapPayFreq, const double w, const Size mode,
			const double mux, const double muy, const double sigmax, const double sigmay, const double rho, const double adjuster);
		double operator()(double x, double y) const;
		double operator()(double x) const;

		double y_;
	//private:
		HW2FAdj* hw_;
		Size fixing_, payment_, swapLength1_, swapLength2_, swapPayFreq_;
		double strike_,w_;
		Size mode_;
		double mux_,muy_,sigmax_,sigmay_,rho_,adjuster_;
	};

	class PayOffX {
	public:
		PayOffX(PayOff* p);
		double operator()(double x) const;
	private:
		PayOff* p_; 
	};


	// for digital call (w=1) only !

	class PayOffD {
	public:
		PayOffD(HW2FAdj* hw, const Size fixing, const Size payment, const double strike,
			const Size swapLength1, const Size swapLength2, const Size swapPayFreq, const double w, const Size mode,
			const double mux, const double muy, const double sigmax, const double sigmay, const double rho, const double adjuster);
		double operator()(double x, double y) const;
		double operator()(double y) const;

	private:
		HW2FAdj* hw_;
		Size fixing_, payment_, swapLength1_, swapLength2_, swapPayFreq_;
		double strike_,w_;
		Size mode_;
		double mux_,muy_,sigmax_,sigmay_,rho_,adjuster_;
	};

	class PayOffH {
	public:
		PayOffH(const PayOffD* p, const double y);
		double operator()(double x) const;

	private:
		const PayOffD *p_;
		double y;
	};
	

}