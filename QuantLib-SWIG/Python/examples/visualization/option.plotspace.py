# Example of option baskets
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
    mayavi.new_scene()
    p = PlotSpace(mayavi.engine.current_scene,
                  [1,100,1])

    p.add_points([[1,2,1],
                  [1,3,1],
                  [2,4,2]])
    p.add_lines([[2,2,1],
                 [3,3,1],
                 [3,3,1],
                 [3,4,1],                 
                 [4,4,2]])
    mayavi.new_scene()
    p = PlotSpace(mayavi.engine.current_scene,
                  [1,100,1])

    for r in riskfree:
        eurooption.setQuote(r)
        p.add_surface_data_immediate(spot, vol, eurooption.f)
    


    
