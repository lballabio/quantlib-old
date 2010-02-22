from enthought.mayavi.scripts import mayavi2
from enthought.tvtk.tools import mlab
from enthought.tvtk.api import tvtk
import scipy
import numpy
import thread
from enthought.mayavi.sources.vtk_data_source import VTKDataSource
from enthought.mayavi.filters.warp_scalar import WarpScalar
from enthought.mayavi.filters.filter_base import FilterBase
from enthought.mayavi.modules.outline import Outline
from enthought.mayavi.modules.surface import Surface
from enthought.mayavi.modules.vectors import Vectors
from enthought.pyface.gui import GUI
from threading import Thread



class PlotSpace:
    def __init__(self, scene, scale):
        self.scale = scale
        self.scene = scene
    class CalcThread(Thread):
        def __init__(self, p, xrange, yrange, f):
            self.p = p
            self.xrange = xrange
            self.yrange = yrange
            self.f = f
            Thread.__init__(self)
        def run(self):
            s1 = mlab.SurfRegular(self.xrange, self.yrange,
                                  scipy.vectorize(self.f),
                                  scale=self.p.scale)
            GUI.invoke_later(self.p.add_source_data, s1.data)

    def add_surface_data(self, xrange, yrange, f):
        print "Start!!!"
        calc_thread = self.CalcThread(self, xrange, yrange, f)
        calc_thread.start()
    def add_surface_data_immediate(self, xrange, yrange, f):
        s1 = mlab.SurfRegular(xrange, yrange,
                                  scipy.vectorize(f),
                                  scale=self.scale)
        self.add_source_data(s1.data)
    def add_source_data(self, data):
        from enthought.mayavi.filters.poly_data_normals import PolyDataNormals
        d = VTKDataSource()
        d.data = data
        obj = self.scene.add_child(d)
        w = WarpScalar()
        d.add_child(w)
        n = PolyDataNormals()
        n.filter.feature_angle = 45
        w.add_child(n)
        s = Surface()
        n.add_child(s)
        
    def add_points(self, points):
        g = mlab.Glyphs(points, None, None)
        d = VTKDataSource()
        d.data = g.poly_data
        self.scene.add_child(d)
        v = Vectors()
        v.glyph.color_mode = 'no_coloring'
        v.glyph.glyph_source = tvtk.PointSource(radius=0,
                                                number_of_points=1)
        d.add_child(v)
    def add_lines(self, points):
        np = len(points) - 1
        lines = scipy.zeros((np, 2), 'l')
        lines[:,0] = scipy.arange(0, np-0.5, 1, 'l')
        lines[:,1] = scipy.arange(1, np+0.5, 1, 'l')
        pd = tvtk.PolyData(points=points, lines=lines)
        d = VTKDataSource()
        d.data = pd
        self.scene.add_child(d)
        filter = tvtk.TubeFilter(number_of_sides=6)
        filter.radius = 0.01
        f = FilterBase(filter=filter, name='TubeFilter')
        d.add_child(f)
        s = Surface()
        s.actor.mapper.scalar_visibility = False
        d.add_child(s)

    
