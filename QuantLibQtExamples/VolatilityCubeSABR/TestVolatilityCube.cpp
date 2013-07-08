/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

#include "customutilities.hpp"

#include <ql/experimental/volatility/abcdatmvolcurve.hpp>
#include <ql/experimental/volatility/blackvolsurface.hpp>
#include <ql/experimental/volatility/equityfxvolsurface.hpp>
#include <ql/experimental/volatility/extendedblackvariancecurve.hpp>
#include <ql/experimental/volatility/extendedblackvariancesurface.hpp>
#include <ql/experimental/volatility/interestratevolsurface.hpp>
#include <ql/experimental/volatility/sabrvolsurface.hpp>
#include <ql/experimental/volatility/volcube.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolcube.hpp>

#include <ql/quantlib.hpp>
#include <boost/timer.hpp>
#include <boost/shared_ptr.hpp>
#include <iostream>
#include <iomanip>

using namespace QuantLib;


namespace QuantLib {
    
    // swaptionvolstructuresutilities.hpp

    struct SwaptionTenors {
        std::vector<Period> options;
        std::vector<Period> swaps;
    };

    struct SwaptionMarketConventions {
        Calendar calendar;
        BusinessDayConvention optionBdc;
        DayCounter dayCounter;
        void setConventions() {
            calendar = TARGET();
            optionBdc = ModifiedFollowing;
            dayCounter = Actual365Fixed();
        }
    };

    struct AtmVolatilitySample {
        SwaptionTenors tenors;
        Matrix vols;
        std::vector<std::vector<Handle<Quote> > > volsHandle;
        void setMarketData() {
            tenors.options.resize(6);
            tenors.options[0] = Period(1, Months);
            tenors.options[1] = Period(6, Months);
            tenors.options[2] = Period(1, Years);
            tenors.options[3] = Period(5, Years);
            tenors.options[4] = Period(10, Years);
            tenors.options[5] = Period(30, Years);
            tenors.swaps.resize(4);
            tenors.swaps[0] = Period(1, Years);
            tenors.swaps[1] = Period(5, Years);
            tenors.swaps[2] = Period(10, Years);
            tenors.swaps[3] = Period(30, Years);
            vols = Matrix(tenors.options.size(), tenors.swaps.size());
            vols[0][0]=0.1300; vols[0][1]=0.1560; vols[0][2]=0.1390; vols[0][3]=0.1220;
            vols[1][0]=0.1440; vols[1][1]=0.1580; vols[1][2]=0.1460; vols[1][3]=0.1260;
            vols[2][0]=0.1600; vols[2][1]=0.1590; vols[2][2]=0.1470; vols[2][3]=0.1290;
            vols[3][0]=0.1640; vols[3][1]=0.1470; vols[3][2]=0.1370; vols[3][3]=0.1220;
            vols[4][0]=0.1400; vols[4][1]=0.1300; vols[4][2]=0.1250; vols[4][3]=0.1100;
            vols[5][0]=0.1130; vols[5][1]=0.1090; vols[5][2]=0.1070; vols[5][3]=0.0930;
            volsHandle.resize(tenors.options.size());
            for (Size i=0; i<tenors.options.size(); i++){
                volsHandle[i].resize(tenors.swaps.size());
                for (Size j=0; j<tenors.swaps.size(); j++)
                    // every handle must be reassigned, as the ones created by
                    // default are all linked together.
                    volsHandle[i][j] = Handle<Quote>(boost::shared_ptr<Quote>(new
                        SimpleQuote(vols[i][j])));
            }
        }
    };

    struct VolatilityCubeSample {
        SwaptionTenors tenors;
        Matrix volSpreads;
        std::vector<std::vector<Handle<Quote> > > volSpreadsHandle;
        std::vector<Spread> strikeSpreads;
        void setMarketData() {
            tenors.options.resize(3);
            tenors.options[0] = Period(1, Years);
            tenors.options[1] = Period(10, Years);
            tenors.options[2] = Period(30, Years);
            tenors.swaps.resize(3);
            tenors.swaps[0] = Period(2, Years);
            tenors.swaps[1] = Period(10, Years);
            tenors.swaps[2] = Period(30, Years);
            strikeSpreads.resize(5);
            strikeSpreads[0] = -0.020;
            strikeSpreads[1] = -0.005;
            strikeSpreads[2] = +0.000;
            strikeSpreads[3] = +0.005;
            strikeSpreads[4] = +0.020;
            volSpreads = Matrix(tenors.options.size()*tenors.swaps.size(), strikeSpreads.size());
            volSpreads[0][0] = 0.0599; volSpreads[0][1] = 0.0049;
            volSpreads[0][2] = 0.0000;
            volSpreads[0][3] =-0.0001; volSpreads[0][4] = 0.0127;
            volSpreads[1][0] = 0.0729; volSpreads[1][1] = 0.0086;
            volSpreads[1][2] = 0.0000;
            volSpreads[1][3] =-0.0024; volSpreads[1][4] = 0.0098;
            volSpreads[2][0] = 0.0738; volSpreads[2][1] = 0.0102;
            volSpreads[2][2] = 0.0000;
            volSpreads[2][3] =-0.0039; volSpreads[2][4] = 0.0065;
            volSpreads[3][0] = 0.0465; volSpreads[3][1] = 0.0063;
            volSpreads[3][2] = 0.0000;
            volSpreads[3][3] =-0.0032; volSpreads[3][4] =-0.0010;
            volSpreads[4][0] = 0.0558; volSpreads[4][1] = 0.0084;
            volSpreads[4][2] = 0.0000;
            volSpreads[4][3] =-0.0050; volSpreads[4][4] =-0.0057;
            volSpreads[5][0] = 0.0576; volSpreads[5][1] = 0.0083;
            volSpreads[5][2] = 0.0000;
            volSpreads[5][3] =-0.0043; volSpreads[5][4] = -0.0014;
            volSpreads[6][0] = 0.0437; volSpreads[6][1] = 0.0059;
            volSpreads[6][2] = 0.0000;
            volSpreads[6][3] =-0.0030; volSpreads[6][4] =-0.0006;
            volSpreads[7][0] = 0.0533; volSpreads[7][1] = 0.0078;
            volSpreads[7][2] = 0.0000;
            volSpreads[7][3] =-0.0045; volSpreads[7][4] =-0.0046;
            volSpreads[8][0] = 0.0545; volSpreads[8][1] = 0.0079;
            volSpreads[8][2] = 0.0000;
            volSpreads[8][3] =-0.0042; volSpreads[8][4] =-0.0020;
            volSpreadsHandle = std::vector<std::vector<Handle<Quote> > >(tenors.options.size()*tenors.swaps.size());
            for (Size i=0; i<tenors.options.size()*tenors.swaps.size(); i++){
                volSpreadsHandle[i] = std::vector<Handle<Quote> >(strikeSpreads.size());
                for (Size j=0; j<strikeSpreads.size(); j++) {
                    // every handle must be reassigned, as the ones created by
                    // default are all linked together.
                    volSpreadsHandle[i][j] = Handle<Quote>(boost::shared_ptr<Quote>(new
                        SimpleQuote(volSpreads[i][j])));
                }
            }
        }
    };


 /*   static void setupCubeUtilities() {
        conventions_.calendar = TARGET();
        conventions_.optionBdc = Following;
        conventions_.dayCounter = Actual365Fixed();
        atm_.setMarketData();
        cube_.setMarketData();
        atmVolMatrix_ = RelinkableHandle<SwaptionVolatilityStructure>(
            boost::shared_ptr<SwaptionVolatilityStructure>(new
                SwaptionVolatilityMatrix(conventions_.calendar,
                                         atm_.tenors.options,
                                         atm_.tenors.swaps,
                                         atm_.volsHandle,
                                         conventions_.dayCounter,
                                         conventions_.optionBdc)));
    }*/
}

namespace {

    // swaptionvolatilitycube.cpp

    struct CommonVars {
        // global data
        SwaptionMarketConventions conventions;
        AtmVolatilitySample atm;
        RelinkableHandle<SwaptionVolatilityStructure> atmVolMatrix;
        VolatilityCubeSample cube;

        RelinkableHandle<YieldTermStructure> termStructure;

        boost::shared_ptr<SwapIndex> swapIndexBase, shortSwapIndexBase;
        bool vegaWeighedSmileFit;

        // cleanup
        SavedSettings backup;

        // utilities
        void makeAtmVolTest(const SwaptionVolatilityCube& volCube,
            Real tolerance) {

                for (Size i=0; i<atm.tenors.options.size(); i++) {
                    for (Size j=0; j<atm.tenors.swaps.size(); j++) {
                        Rate strike = volCube.atmStrike(atm.tenors.options[i],
                            atm.tenors.swaps[j]);
                        Volatility expVol =
                            atmVolMatrix->volatility(atm.tenors.options[i],
                            atm.tenors.swaps[j],
                            strike, true);
                        Volatility actVol = volCube.volatility(atm.tenors.options[i],
                            atm.tenors.swaps[j],
                            strike, true);
                        Volatility error = std::abs(expVol-actVol);
                        if (error>tolerance)
                            BOOST_ERROR("\nrecovery of atm vols failed:"
                            "\nexpiry time = " << atm.tenors.options[i] <<
                            "\nswap length = " << atm.tenors.swaps[j] <<
                            "\n atm strike = " << io::rate(strike) <<
                            "\n   exp. vol = " << io::volatility(expVol) <<
                            "\n actual vol = " << io::volatility(actVol) <<
                            "\n      error = " << io::volatility(error) <<
                            "\n  tolerance = " << tolerance);
                    }
                }
        }

        void makeVolSpreadsTest(const SwaptionVolatilityCube& volCube,
            Real tolerance) {

                for (Size i=0; i<cube.tenors.options.size(); i++) {
                    for (Size j=0; j<cube.tenors.swaps.size(); j++) {
                        for (Size k=0; k<cube.strikeSpreads.size(); k++) {
                            Rate atmStrike = volCube.atmStrike(cube.tenors.options[i],
                                cube.tenors.swaps[j]);
                            Volatility atmVol =
                                atmVolMatrix->volatility(cube.tenors.options[i],
                                cube.tenors.swaps[j],
                                atmStrike, true);
                            Volatility vol =
                                volCube.volatility(cube.tenors.options[i],
                                cube.tenors.swaps[j],
                                atmStrike+cube.strikeSpreads[k], true);
                            Volatility spread = vol-atmVol;
                            Volatility expVolSpread =
                                cube.volSpreads[i*cube.tenors.swaps.size()+j][k];
                            Volatility error = std::abs(expVolSpread-spread);
                            if (error>tolerance)
                                BOOST_FAIL("\nrecovery of smile vol spreads failed:"
                                "\n    option tenor = " << cube.tenors.options[i] <<
                                "\n      swap tenor = " << cube.tenors.swaps[j] <<
                                "\n      atm strike = " << io::rate(atmStrike) <<
                                "\n   strike spread = " << io::rate(cube.strikeSpreads[k]) <<
                                "\n         atm vol = " << io::volatility(atmVol) <<
                                "\n      smiled vol = " << io::volatility(vol) <<
                                "\n      vol spread = " << io::volatility(spread) <<
                                "\n exp. vol spread = " << io::volatility(expVolSpread) <<
                                "\n           error = " << io::volatility(error) <<
                                "\n       tolerance = " << tolerance);
                        }
                    }
                }
        }

        CommonVars() {

            conventions.setConventions();

            // ATM swaptionvolmatrix
            atm.setMarketData();

            atmVolMatrix = RelinkableHandle<SwaptionVolatilityStructure>(
                boost::shared_ptr<SwaptionVolatilityStructure>(new
                SwaptionVolatilityMatrix(conventions.calendar,
                conventions.optionBdc,
                atm.tenors.options,
                atm.tenors.swaps,
                atm.volsHandle,
                conventions.dayCounter)));

            // Swaptionvolcube
            cube.setMarketData();

            termStructure.linkTo(flatRate(0.05, Actual365Fixed()));

            swapIndexBase = boost::shared_ptr<SwapIndex>(new
                EuriborSwapIsdaFixA(2*Years, termStructure));
            shortSwapIndexBase = boost::shared_ptr<SwapIndex>(new
                EuriborSwapIsdaFixA(1*Years, termStructure));

            vegaWeighedSmileFit=false;
        }
    };

}



#include<boost/math/distributions.hpp>

namespace QlTesting {

Real blackScholesPriceFwd(const Real& fwd,
                         const Real& strike,
                         const Volatility& vol,
                         const Rate& rd,
                         const Rate& rf,
                         const Time& tau,
                         const Integer& phi){
    boost::math::normal_distribution<> d(0.0,1.0);
    Real dp,dm, stdDev, res, domDf, forDf;

    domDf=std::exp(-rd*tau); forDf=std::exp(-rf*tau);
    stdDev=vol*std::sqrt(tau);

    dp=(std::log(fwd/strike)+0.5*stdDev*stdDev)/stdDev;
    dm=(std::log(fwd/strike)-0.5*stdDev*stdDev)/stdDev;

    res=phi*domDf*(fwd*cdf(d,phi*dp)-strike*cdf(d,phi*dm));
    return res;
}

class SimpleVolQuote: public Quote{
private:
    Volatility vol_;
    Real strike_;

public:
    SimpleVolQuote(Volatility vol, Real strike) :vol_(vol),strike_(strike){}

    bool isValid() const {return vol_!=Null<Real>();}
    Real value() const {return vol_;}
    Real strike()const {return strike_;}

    void setVolatility(const Volatility& vol){
        vol_=vol;
        notifyObservers();
    }
};

class SimpleSmile: public Observable, Observer{
private:
    std::vector<boost::shared_ptr<SimpleVolQuote> > volVec_;
public:
    SimpleSmile(const std::vector<boost::shared_ptr<SimpleVolQuote> >& volVec)
        :volVec_(volVec){
        for(Size i=0;i<volVec_.size();i++){
            registerWith(volVec_[i]);
        }
    }
    void update(){
        notifyObservers();
    }

    std::vector<boost::shared_ptr<SimpleVolQuote> > getVolVec() const{return volVec_;}
    Size getVolNumber() const{return volVec_.size();}
};

class SabrModel: public LazyObject{
public:
    SabrModel(const boost::shared_ptr<SimpleSmile>& smile,
        const Real& fwd, const Time& tau, const Real& rd,const Real& rf)
        :smile_(smile),fwd_(fwd),tau_(tau),rd_(rd),rf_(rf),
        strikeVec_(std::vector<Real>(smile->getVolNumber())),
        volVec_(std::vector<Real>(smile->getVolNumber())){
            // register smile as observable
            registerWith(smile_);
    }
    Real getVanillaPrice(const Real& strike){
        calculate();
        return blackScholesPriceFwd(fwd_,strike,(*intp_)(strike),rd_,rf_,tau_,1);
    }
private:
    void performCalculations() const{
        volQuoteVec_=smile_->getVolVec();
        for(Size i=0;i< volQuoteVec_.size();++i){
            strikeVec_[i]=(*volQuoteVec_[i]).strike();
            volVec_[i]=(*volQuoteVec_[i]).value();
        }
        if(intp_==NULL){
            intp_.reset(new SABRInterpolation(strikeVec_.begin(), strikeVec_.end(),
            volVec_.begin(),tau_,fwd_,0.1,0.1,0.1,0.1,false, false, false, false));
        }
            intp_->update(); std::cout << "Recalibration Performed!" << std::endl;
    }
    Real fwd_,rd_,rf_;
    Time tau_;
    boost::shared_ptr<SimpleSmile> smile_;
    mutable boost::shared_ptr<SABRInterpolation> intp_;
    mutable std::vector<Real> strikeVec_,volVec_;
    mutable std::vector<boost::shared_ptr<SimpleVolQuote> > volQuoteVec_;
};

void testingLazyObject1(){

    boost::shared_ptr<SimpleVolQuote> v1(new SimpleVolQuote(0.200,  90.0));
    boost::shared_ptr<SimpleVolQuote> v2(new SimpleVolQuote(0.194,  95.0));
    boost::shared_ptr<SimpleVolQuote> v3(new SimpleVolQuote(0.187, 100.0));
    boost::shared_ptr<SimpleVolQuote> v4(new SimpleVolQuote(0.191, 105.0));

    std::vector<boost::shared_ptr<SimpleVolQuote> > volVec;
    volVec.push_back(v1); volVec.push_back(v2);
    volVec.push_back(v3); volVec.push_back(v4);

    boost::shared_ptr<SimpleSmile> mySmile(new SimpleSmile(volVec));

    Time tau=0.5; Real spot=100.0, rd=0.03, rf=0.024;
    Real fwd=spot*std::exp((rd-rf)*tau);

    SabrModel myModel(mySmile, fwd, tau, rd, rf);

    Real res=myModel.getVanillaPrice(100.0);
    std::cout << "Initial Sabr ATM Vanilla Price:" << res << std::endl;
    res=myModel.getVanillaPrice(90.0);
    res=myModel.getVanillaPrice(95.0);
    res=myModel.getVanillaPrice(105.0);

    v1->setVolatility(0.22);
    v2->setVolatility(0.214);
    v3->setVolatility(0.207);
    v4->setVolatility(0.211);

    res=myModel.getVanillaPrice(100.0);
    std::cout << "Last Sabr ATM Vanilla Price:" << res << std::endl;
}

}

int main(int, char* []) {

    try {

        boost::timer timer;
        std::cout << std::endl;



        CommonVars vars;


        LARGE_TITLE("Testing swaption volatility cube (atm vols)...");

        SwaptionVolCube2 volCube2(vars.atmVolMatrix,
            vars.cube.tenors.options,
            vars.cube.tenors.swaps,
            vars.cube.strikeSpreads,
            vars.cube.volSpreadsHandle,
            vars.swapIndexBase,
            vars.shortSwapIndexBase,
            vars.vegaWeighedSmileFit);


        LARGE_TITLE("Testing swaption volatility cube (sabr interpolation)...");

        std::vector<std::vector<Handle<Quote> > >
            parametersGuess(vars.cube.tenors.options.size()*vars.cube.tenors.swaps.size());
        for (Size i=0; i<vars.cube.tenors.options.size()*vars.cube.tenors.swaps.size(); i++) {
            parametersGuess[i] = std::vector<Handle<Quote> >(4);
            parametersGuess[i][0] =
                Handle<Quote>(boost::shared_ptr<Quote>(new SimpleQuote(0.2)));
            parametersGuess[i][1] =
                Handle<Quote>(boost::shared_ptr<Quote>(new SimpleQuote(0.5)));
            parametersGuess[i][2] =
                Handle<Quote>(boost::shared_ptr<Quote>(new SimpleQuote(0.4)));
            parametersGuess[i][3] =
                Handle<Quote>(boost::shared_ptr<Quote>(new SimpleQuote(0.0)));
        }
        std::vector<bool> isParameterFixed(4, false);

        SwaptionVolCube1 volCube1(vars.atmVolMatrix,
            vars.cube.tenors.options,
            vars.cube.tenors.swaps,
            vars.cube.strikeSpreads,
            vars.cube.volSpreadsHandle,
            vars.swapIndexBase,
            vars.shortSwapIndexBase,
            vars.vegaWeighedSmileFit,
            parametersGuess,
            isParameterFixed,
            true);

        //SwaptionVolCube1::Cube marketVolCube;
        //volCube1.sabrCalibration(marketVolCube);


        Time optionTime = 5;
        Time swapLength = 10;
        boost::shared_ptr<SmileSection> smileSection =
                volCube1.smileSectionImpl(optionTime, swapLength);


        std::cout << "Market vol cube" << std::endl;
        std::cout << volCube1.marketVolCube() << std::endl << std::endl;


        std::cout << "Calibrated ATM  vol matrix" << std::endl;
        std::cout << volCube1.volCubeAtmCalibrated() << std::endl << std::endl;


        for (Size size=0; size<4; ++size) {
            std::cout << "Market volatility cube at time " << size << std::endl;
            std::cout << volCube1.marketVolCube(size) << std::endl << std::endl;
        }


        if(false){
            LARGE_TITLE("SABR volatility");
            // http://www.math.umn.edu/finmath/modeling/Materials/Vinar_presentation_Jan2012.pdf
            // http://www.frouah.com/finance%20notes/The%20SABR%20Model.pdf
            Rate strike = 0.0730;
            Rate forward = 0.0816;
            Time expiryTime = 1.0;
            Real alpha = 0.037561;
            // beta=0 for normal process
            // beta=1 for log normal process
            // beta=0.5 for stochastic CIR
            Real beta = 0.5;
            Real nu = 0.573296;
            Real rho = 0.100044;
            std::cout << "SABR volatility: " << std::endl;
            for (Size i=0; i<50; ++i) {
                std::cout //<< std::setw(5) << (strike+i-25.0) << ": "
                          << sabrVolatility(strike+(i-25.0)/50.0*strike, forward, expiryTime, alpha, beta, nu, rho)
                          << std::endl;
            }
        }


        if(true){
            LARGE_TITLE("SABR Interpolation");
            SabrInterpolatedSmileSection(optionDate, forward, strikes, hasFloatingStrikes,
                                         atmVolatility, vols, alpha, beta, nu, rho);

            SabrSmileSection (Time timeToExpiry, Rate forward, const std::vector< Real > &sabrParameters);


        }


        LARGE_TITLE("Testing spreaded swaption volatility cube...");

        Handle<SwaptionVolatilityStructure> volCube(boost::shared_ptr<SwaptionVolatilityStructure>(new
            SwaptionVolCube1(vars.atmVolMatrix,
            vars.cube.tenors.options,
            vars.cube.tenors.swaps,
            vars.cube.strikeSpreads,
            vars.cube.volSpreadsHandle,
            vars.swapIndexBase,
            vars.shortSwapIndexBase,
            vars.vegaWeighedSmileFit,
            parametersGuess,
            isParameterFixed,
            true)));

        boost::shared_ptr<SimpleQuote> spread(new SimpleQuote(0.0001));
        Handle<Quote> spreadHandle(spread);
        boost::shared_ptr<SwaptionVolatilityStructure> spreadedVolCube(new
            SpreadedSwaptionVolatility(volCube, spreadHandle));
        std::vector<Real> strikes;
        for (Size k=1; k<100; k++)
            strikes.push_back(k*.01);
        for (Size i=0; i<vars.cube.tenors.options.size(); i++) {
            for (Size j=0; j<vars.cube.tenors.swaps.size(); j++) {
                /*boost::shared_ptr<SmileSection> smileSectionByCube =
                    volCube->smileSection(vars.cube.tenors.options[i], vars.cube.tenors.swaps[j]);
                boost::shared_ptr<SmileSection> smileSectionBySpreadedCube =
                    spreadedVolCube->smileSection(vars.cube.tenors.options[i], vars.cube.tenors.swaps[j]);*/
                for (Size k=0; k<strikes.size(); k++) {

                    Real volcubevol = volCube->volatility(vars.cube.tenors.options[i], vars.cube.tenors.swaps[j], strikes[k]);
//                    std::cout << "\nSwaption(i, j, k) = (" << i << ", " << j << ", " << k << ")\n"
//                        << "expiry time = " << vars.cube.tenors.options[i] << "\n"
//                        << "swap length = " << vars.cube.tenors.swaps[j] << "\n"
//                        << "atm strike = " << io::rate(strikes[k]) << "\n"
//                        << "volatility = " << io::rate(volcubevol) << std::endl;

                    /*
                    Real strike = strikes[k];
                    Real diff = spreadedVolCube->volatility(vars.cube.tenors.options[i], vars.cube.tenors.swaps[j], strike)
                        - volCube->volatility(vars.cube.tenors.options[i], vars.cube.tenors.swaps[j], strike);
                    if (std::fabs(diff-spread->value())>1e-16)
                        BOOST_ERROR("\ndiff!=spread in volatility method:"
                        "\nexpiry time = " << vars.cube.tenors.options[i] <<
                        "\nswap length = " << vars.cube.tenors.swaps[j] <<
                        "\n atm strike = " << io::rate(strike) <<
                        "\ndiff = " << diff <<
                        "\nspread = " << spread->value());

                    diff = smileSectionBySpreadedCube->volatility(strike)
                        - smileSectionByCube->volatility(strike);
                    if (std::fabs(diff-spread->value())>1e-16)
                        BOOST_ERROR("\ndiff!=spread in smile section method:"
                        "\nexpiry time = " << vars.cube.tenors.options[i] <<
                        "\nswap length = " << vars.cube.tenors.swaps[j] <<
                        "\n atm strike = " << io::rate(strike) <<
                        "\ndiff = " << diff <<
                        "\nspread = " << spread->value());
                    */
                }
            }
        }


        LARGE_TITLE("Local volatility curve derived from a Black curve");
        //LocalVolCurve lvolcurve(curve);


        if(false){
        LARGE_TITLE("Testing volatility model construction...");

        TimeSeries<Real> ts;
        ts[Date(25, March, 2005)] = 1.2;
        ts[Date(29, March, 2005)] = 2.3;
        ts[Date(15, March, 2005)] = 0.3;

        SimpleLocalEstimator sle(1/360.0);
        TimeSeries<Volatility> locale = sle.calculate(ts);

        ConstantEstimator ce(1);
        TimeSeries<Volatility> sv = ce.calculate(locale);
        sv.begin();
        }


        if(true){
        LARGE_TITLE("SABR Lazy Object Test");
        QlTesting::testingLazyObject1();
        }


        std::cout << std::endl << std::endl;

        Real seconds  = timer.elapsed();
        Integer hours = Integer(seconds/3600);
        seconds -= hours * 3600;
        Integer minutes = Integer(seconds/60);
        seconds -= minutes * 60;
        std::cout << "Run completed in ";
        if (hours > 0)
            std::cout << hours << " h ";
        if (hours > 0 || minutes > 0)
            std::cout << minutes << " m ";
        std::cout << std::fixed << std::setprecision(0)
             << seconds << " s" << std::endl;

        return 0;
    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }
}

/*
void VolatilityCubeTest::testVolatilityCube()
{
    
    VolatilityCube(const std::vector<Handle<InterestRateVolSurface> >&,
        const std::vector<Handle<AbcdAtmVolCurve> >&);

    AbcdAtmVolCurve(Natural settlementDays,
        const Calendar& cal,
        const std::vector<Period>& optionTenors,
        const std::vector<Handle<Quote> >& volsHandles,
        const std::vector<bool> inclusionInInterpolationFlag
        = std::vector<bool>(1, true),
        BusinessDayConvention bdc = Following,
        const DayCounter& dc = Actual365Fixed());

    SwaptionVolatilityCube(
        const Handle<SwaptionVolatilityStructure>& atmVolStructure,
        const std::vector<Period>& optionTenors,
        const std::vector<Period>& swapTenors,
        const std::vector<Spread>& strikeSpreads,
        const std::vector<std::vector<Handle<Quote> > >& volSpreads,
        const boost::shared_ptr<SwapIndex>& swapIndexBase,
        const boost::shared_ptr<SwapIndex>& shortSwapIndexBase,
        bool vegaWeightedSmileFit);
    
}*/
