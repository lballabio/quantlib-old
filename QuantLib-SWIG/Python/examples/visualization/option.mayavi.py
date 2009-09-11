# Example of option baskets
# Distributed under BSD License

from enthought.mayavi.scripts import mayavi2
mayavi2.standalone(globals())


import scipy
import numpy
from QuantLib import *
from enthought.tvtk.tools import mlab
from enthought.mayavi.sources.vtk_data_source import VTKDataSource
from enthought.mayavi.filters.warp_scalar import WarpScalar
from enthought.mayavi.modules.outline import Outline
from enthought.mayavi.modules.surface import Surface


spot = scipy.arange(10.0, 100.0, 5.0)
vol = scipy.arange(0.1, 1.0, 0.1)
riskfree = scipy.arange(0.0, 5.0, 1.0)

todaysDate = Date(15, May, 1998)
Settings.instance().evaluationDate = todaysDate
settlementDate = Date(17, May, 1998)
riskFreeQuote = SimpleQuote(0.05)
riskFreeRate = FlatForward(settlementDate,
                           QuoteHandle(riskFreeQuote), Actual365Fixed())

# option parameters
exercise1 = AmericanExercise(settlementDate, Date(17,May,1999))
exercise2 = EuropeanExercise(settlementDate)
payoff = PlainVanillaPayoff(Option.Call, 40.0)

# market data
underlying = SimpleQuote(36.0)
volatilityQuote = SimpleQuote(0.05)
volatility = BlackConstantVol(todaysDate,
                              QuoteHandle(volatilityQuote),
                              Actual365Fixed())
dividendYield = FlatForward(settlementDate, 0.00, Actual365Fixed())

# good to go
process = BlackScholesMertonProcess(QuoteHandle(underlying),
                                    YieldTermStructureHandle(dividendYield),
                                    YieldTermStructureHandle(riskFreeRate),
                                    BlackVolTermStructureHandle(volatility))
option1 = VanillaOption(process, payoff, exercise1)
option1.setPricingEngine(BaroneAdesiWhaleyEngine())


def f(x,y):
    underlying.setValue(x)
    volatilityQuote.setValue(y)
    return option1.NPV()

    
def add_data(tvtk_data):
    """Add a TVTK data object `tvtk_data` to the mayavi pipleine.
    """
    d = VTKDataSource()
    d.data = tvtk_data
    mayavi.add_source(d)
    return d

def surf_regular(source):
    """Now visualize the data as done in mlab.
    """
    w = WarpScalar()
    source.add_child(w)
    s = Surface()
    w.add_child(s)


# 3D visualization of f:
from enthought.tvtk.tools import mlab

if __name__ == '__main__':
    mayavi.new_scene()
    for r in riskfree:
        riskFreeQuote.setValue(r)
        s1 = mlab.SurfRegular(spot, vol, scipy.vectorize(f), scale=[1,100, 1])
        s1.lut.alpha_range=(0.2,0.2)
        d = add_data(s1.data)
        surf_regular(d)

