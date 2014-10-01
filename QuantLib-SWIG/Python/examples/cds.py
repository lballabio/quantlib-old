import QuantLib as QL

calendar = QL.TARGET()

# set evaluation date
todaysDate = QL.Date(15,QL.May,2007);
todaysDate = calendar.adjust(todaysDate)
QL.Settings.instance().evaluationDate = todaysDate

# define curve
curve = QL.FlatForward(todaysDate, 0.01, QL.Actual365Fixed())
tscurve = QL.YieldTermStructureHandle(curve)

# CDS parameters
recovery_rate = 0.5
quoted_spreads = ( 0.0150, 0.0150, 0.0150, 0.0150 )
tenors = [ QL.Period(3, QL.Months), QL.Period("6M"), QL.Period("1Y"), QL.Period("2Y")]
maturities = [ calendar.adjust(todaysDate + x, QL.Following) for x in tenors]

instruments = [ QL.SpreadCdsHelper(QL.QuoteHandle(QL.SimpleQuote(quoted_spreads[x])), 
							tenors[x], 
							0, 
							calendar, 
							QL.Quarterly, 
							QL.Following, 
							QL.DateGeneration.TwentiethIMM, 
							QL.Actual365Fixed(), 
							recovery_rate, 
							tscurve) for x in range(len(tenors))]

hazardcurve = QL.PiecewiseFlatHazardRate(todaysDate, instruments, QL.Actual365Fixed())
print "Calibrated hazard rate values: "
for x in hazardcurve.nodes():
	print "hazard rate on %s is %.7f" % x

print "Some survival probability values: "
print "1Y survival probability: %.4g, \n\t\texpected %.4g" % (
				hazardcurve.survivalProbability(todaysDate + QL.Period("1Y")),
				0.9704)
print "2Y survival probability: %.4g, \n\t\texpected %.4g" % (
				hazardcurve.survivalProbability(todaysDate + QL.Period("2Y")),
				0.9418)

# reprice instruments
nominal = 1000000.0
probability = QL.DefaultProbabilityTermStructureHandle(hazardcurve)

# create a cds for every maturity
allcds = []
for x in range(len(tenors)):
	schedule = QL.Schedule(todaysDate, maturities[x], QL.Period(QL.Quarterly),
						   calendar, 
						   QL.Following, QL.Unadjusted,
						   QL.DateGeneration.TwentiethIMM,
						   False)
	cds = QL.CreditDefaultSwap(QL.Protection.Seller, nominal, quoted_spreads[x], schedule, QL.Following, QL.Actual365Fixed())
	engine = QL.MidPointCdsEngine(probability, recovery_rate, tscurve)
	cds.setPricingEngine(engine)
	allcds.append(cds)

print "Repricing of quoted CDSs employed for calibration: "
for x in range(len(tenors)):
	print "%s fair spread: %.7g" % (tenors[x], allcds[x].fairSpread())
	print "   NPV: %g" % allcds[x].NPV()
	print "   default leg: %.7g" % allcds[x].defaultLegNPV()
	print "   coupon leg: %.7g" % allcds[x].couponLegNPV()
	print
