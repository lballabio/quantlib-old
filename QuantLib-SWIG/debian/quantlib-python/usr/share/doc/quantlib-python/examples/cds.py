# -*- mode: python; tab-width: 4;

# Copyright (C) 2014 Thema Consulting
#
# This file is part of QuantLib, a free-software/open-source library
# for financial quantitative analysts and developers - http://quantlib.org/
#
# QuantLib is free software: you can redistribute it and/or modify it under the
# terms of the QuantLib license.  You should have received a copy of the
# license along with this program; if not, please email
# <quantlib-dev@lists.sf.net>. The license is also available online at
# <http://quantlib.org/license.shtml>.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the license for more details.

from QuantLib import *

calendar = TARGET()

# set evaluation date
todaysDate = Date(15,May,2007);
todaysDate = calendar.adjust(todaysDate)
Settings.instance().evaluationDate = todaysDate

risk_free_rate = YieldTermStructureHandle(
        FlatForward(todaysDate, 0.01, Actual365Fixed()))

# CDS parameters
recovery_rate = 0.5
quoted_spreads = [ 0.0150, 0.0150, 0.0150, 0.0150 ]
tenors = [ Period(3, Months), Period(6, Months),
           Period(1, Years), Period(2, Years) ]
maturities = [ calendar.adjust(todaysDate + x, Following) for x in tenors]

instruments = [
    SpreadCdsHelper(QuoteHandle(SimpleQuote(s)),
                    tenor,
                    0,
                    calendar,
                    Quarterly,
                    Following,
                    DateGeneration.TwentiethIMM,
                    Actual365Fixed(),
                    recovery_rate,
                    risk_free_rate)
    for s,tenor in zip(quoted_spreads, tenors) ]

hazard_curve = PiecewiseFlatHazardRate(todaysDate, instruments,
                                       Actual365Fixed())
print "Calibrated hazard rate values: "
for x in hazard_curve.nodes():
    print "hazard rate on %s is %.7f" % x

print "Some survival probability values: "
print "1Y survival probability: %.4g, \n\t\texpected %.4g" % (
    hazard_curve.survivalProbability(todaysDate + Period("1Y")),
    0.9704)
print "2Y survival probability: %.4g, \n\t\texpected %.4g" % (
    hazard_curve.survivalProbability(todaysDate + Period("2Y")),
    0.9418)

# reprice instruments
nominal = 1000000.0
probability = DefaultProbabilityTermStructureHandle(hazard_curve)

# create a cds for every maturity
all_cds = []
for maturity, s in zip(maturities, quoted_spreads):
    schedule = Schedule(todaysDate, maturity, Period(Quarterly),
                        calendar, Following, Unadjusted,
                        DateGeneration.TwentiethIMM, False)
    cds = CreditDefaultSwap(Protection.Seller, nominal, s,
                            schedule, Following, Actual365Fixed())
    engine = MidPointCdsEngine(probability, recovery_rate, risk_free_rate)
    cds.setPricingEngine(engine)
    all_cds.append(cds)

print "Repricing of quoted CDSs employed for calibration: "
for i in range(len(tenors)):
    print "%s fair spread: %.7g" % (tenors[i], all_cds[i].fairSpread())
    print "   NPV: %g" % all_cds[i].NPV()
    print "   default leg: %.7g" % all_cds[i].defaultLegNPV()
    print "   coupon leg: %.7g" % all_cds[i].couponLegNPV()
    print
