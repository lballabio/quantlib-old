/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Klaus Spanderen

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*/

#include <ql/option.hpp>
#include <ql/settings.hpp>
#include <ql/exercise.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/time/calendars/target.hpp>
#include <ql/time/daycounters/actual365fixed.hpp>
#include <ql/instruments/vanillaoption.hpp>
#include <ql/processes/blackscholesprocess.hpp>
#include <ql/processes/hestonprocess.hpp>
#include <ql/models/equity/hestonmodel.hpp>
#include <ql/pricingengines/vanilla/analyticeuropeanengine.hpp>
#include <ql/pricingengines/vanilla/fdhestonvanillaengine.hpp>
#include <ql/termstructures/yield/flatforward.hpp>
#include <ql/termstructures/volatility/equityfx/blackconstantvol.hpp>

#include <boost/date_time/posix_time/posix_time.hpp>
#include <iostream>

using namespace QuantLib;

int main() {
  const Calendar calendar = TARGET();  
  const Option::Type type(Option::Put);
  const Real underlying = 36;
  const Real strike = underlying;
  const Spread dividendYield = 0.00;
  const Rate riskFreeRate = 0.06;
  const Real v0    = 0.2;
  const Real kappa = 1.0;
  const Real theta = v0;
  const Real sigma = 0.0065;
  const Real rho   = -0.75;
  const DayCounter dayCounter = Actual365Fixed();

  const Date maturity(17, May, 2014, 17, 30, 0);
  
  const boost::shared_ptr<Exercise> europeanExercise(
    new EuropeanExercise(maturity));
  const boost::shared_ptr<StrikedTypePayoff> payoff(
    new PlainVanillaPayoff(type, strike));
  VanillaOption option(payoff, europeanExercise);
  
  const Handle<Quote> s0(
     boost::shared_ptr<Quote>(new SimpleQuote(underlying)));
  RelinkableHandle<BlackVolTermStructure> flatVolTS;
  RelinkableHandle<YieldTermStructure> flatTermStructure, flatDividendTS;
  const boost::shared_ptr<HestonProcess> process(
    new HestonProcess(flatTermStructure, flatDividendTS, s0, 
		      v0, kappa, theta, sigma, rho));
  const boost::shared_ptr<HestonModel> model(new HestonModel(process)); 
  const boost::shared_ptr<PricingEngine> fdm(
    new FdHestonVanillaEngine(model, 20, 100, 26, 0));  

  option.setPricingEngine(fdm);
  
  for (Size i = 0; i < 150; ++i) {
    const Date now(17, May, 2014, 15, i, 0);
    Settings::instance().evaluationDate() = now;

    flatTermStructure.linkTo(boost::shared_ptr<YieldTermStructure>(
      new FlatForward(now, riskFreeRate, dayCounter)));
    flatDividendTS.linkTo(boost::shared_ptr<YieldTermStructure>(
      new FlatForward(now, dividendYield, dayCounter)));
        
    std::cout << now.dateTime() << ", "
	      << option.NPV()   << ", " 
	      << option.delta() << ", " << option.gamma() << std::endl;
  }
  
  
  return 0;
}
