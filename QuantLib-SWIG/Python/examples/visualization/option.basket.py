# Example of option baskets
# 
# Distributed under BSD License

from enthought.mayavi.scripts import mayavi2
from enthought.tvtk.tools import mlab
from plotspace import PlotSpace
mayavi2.standalone(globals())
import eurooption
import basketoption

import scipy
import numpy
import threading

spot = scipy.arange(10.0, 100.0, 5.0)
vol = scipy.arange(0.1, 1.0, 0.1)
riskfree = scipy.arange(0.0, 5.0, 1.0)

u1 = scipy.arange(0.5, 15.0, 0.5)
u2 = scipy.arange(0.5, 15.0, 0.5)

if __name__ == '__main__':
    bmin = basketoption.BasketOptionClass('min')
    bmax = basketoption.BasketOptionClass('max')
    bavg = basketoption.BasketOptionClass('avg')
    mayavi.new_scene()
    p = PlotSpace(mayavi.engine.current_scene, [1,1,1])

    p.add_surface_data(u1, u2, bmin.npv)
    p.add_surface_data(u1, u2, bmax.npv)
    p.add_surface_data(u1, u2, bavg.npv)


    
